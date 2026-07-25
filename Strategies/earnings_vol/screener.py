"""
Daily LIVE screener for both pre-earnings straddle books:

  A. LAG-2 UNGATED (the core): buy the ATM straddle on the first expiry
     spanning the earnings jump, at the close ~2 calendar days before the
     report; sell at the last close strictly before the announcement.
     No filters (backtest sanity floors only: spot >= $5, debit >= $0.30,
     ATM within 10% of spot).
  B. LAG-15 + 4-SIGNAL MODEL (capacity sleeve): same trade entered ~15
     calendar days out, only when the walk-forward OLS of %/spot on the four
     Oakquants signals (fit on the full 2013-2024 backtest) predicts > 0.

Data plumbing (checked live 2026-07-15):
  - ORATS LIVE cores (api.orats.io/datav2/cores): impErnMv (signal),
    wksNextErn (weeks-out prefilter), nextErnTod (CLOCK time here: 900=BMO,
    1630=AMC — unlike historical files where lastErnTod is a CODE 2/3),
    stock price. nextErn itself is NEVER populated on this subscription
    (checked all 200 names), hence:
  - yfinance calendar for the exact next earnings DATE (confirmed if a
    single date; a 2-date range = estimate -> excluded from lag-2, flagged
    on the watchlist). Cross-checked against wksNextErn.
  - LOCAL core day files (~/trading/HistoricalData/ORATS/core, kept current
    by orats_downloader.py) extend the event history past the 2024 caches:
    per-event implied (impErnMv at last pre-event close) and realized
    (|close-to-close| on reaction day, pxCls shifted — it is the PREVIOUS
    close) feed the expanding signal aggregates. Constructions copied from
    pipeline/build_events.py and analysis/oq_signals.py — keep in sync.
  - ORATS LIVE strikes fetched only for today's actual entries (expiry,
    ATM strike, straddle quote).

Also prints a DATE CHECK on ALL open positions (from the signal log): if the
currently-resolved earnings date differs from the one logged at entry, it advises
per case (date_move_advice) — if the held expiry NO LONGER spans the event, SELL
and re-enter (new contract); if the same expiry still spans it, only the exit date
shifts (>> adjust exit, don't churn); if the exit is now due, SELL at the close.
(The backtest can't see moves; it uses realized dates.) Plus a RESTRIKE CHECK on
open lag-15 positions:
close-and-reopen-ATM when |net straddle delta| >= 0.30, bank-excess sizing
(verified in analysis/restrike_delta.py: monthly Sharpe 1.23 -> 1.91).
Lag-2 positions are never restruck (tested irrelevant on the ~2-day hold).

Usage (run on a US trading day BEFORE the close, orders go in at/near close):
    python3 screener.py --capital 20000            # unit solved for 20% ann std
    python3 screener.py --capital 20000 --pct 15   # other risk target
    python3 screener.py --unit 800                 # fixed unit (overrides)
    python3 screener.py --capital 20000 --refit    # also refit model betas
Sizing: one of --capital or --unit is REQUIRED (without a unit the quotes carry
no contract count). --capital/--pct solve the equal-premium unit from the lag-2
backtest (bisection on integer-contract realized ann-std,
analysis/unit_for_risk.py); trades/day, clustering and rounding are all baked
into the backtest, so no further inputs are needed.
Appends every signal to out/live_signals.csv. Model betas cached in
cache/live_model.json. Token is read from orats_downloader.py (not stored
here).
"""
import argparse
import datetime as dt
import glob
import json
import os
import re
import sys
import urllib.request
from multiprocessing import Pool

import numpy as np
import pandas as pd

HERE = os.path.dirname(os.path.abspath(__file__))
CACHE = f"{HERE}/cache"
OUTDIR = f"{HERE}/out"
CORE_DIR = os.path.expanduser("~/trading/HistoricalData/ORATS/core")
TOKEN_FILE = os.path.expanduser("~/trading/HistoricalData/ORATS/orats_downloader.py")
TOP200 = f"{HERE}/out/top200_earnings_live.csv"  # pipeline/refresh_universe.py
if not os.path.exists(TOP200):  # fallback: historical backtest universe
    TOP200 = os.path.expanduser(
        "~/trading/Systems/Options/Strategies/ff_calendar_v2/out/"
        "top200_single_names_clean.csv")
USECOLS = ["ticker", "tradeDate", "pxCls", "lastErn", "lastErnTod",
           "impErnMv", "impliedEarningsMove", "absAvgErnMv"]
SIGS = ["s1_vs_last_imp", "s2_vs_last_rlz", "s3_vs_avg_imp", "s4_vs_avg_rlz"]
MIN_PX, MAX_ATM_DIST = 5.0, 0.10
MIN_DEBIT = 0.75   # per-contract fees ($4/contract round-trip) make cheaper
                   # straddles EV-negative; net-optimal floor, was 0.30 gross
DELTA_RESTRIKE = 0.30   # lag-15 restrike trigger on |net straddle delta|
HOLIDAYS = [  # NYSE full-day closes (extend yearly)
    "2026-01-01", "2026-01-19", "2026-02-16", "2026-04-03", "2026-05-25",
    "2026-06-19", "2026-07-03", "2026-09-07", "2026-11-26", "2026-12-25",
    "2027-01-01", "2027-01-18", "2027-02-15", "2027-03-26", "2027-05-31",
    "2027-06-18", "2027-07-05", "2027-09-06", "2027-11-25", "2027-12-24",
]


def token():
    return re.search(r'TOKEN = "([^"]+)"', open(TOKEN_FILE).read()).group(1)


def api(endpoint, tickers, tok):
    rows = []
    for i in range(0, len(tickers), 50):
        url = (f"https://api.orats.io/datav2/{endpoint}?token={tok}"
               f"&ticker={','.join(tickers[i:i+50])}")
        rows += json.loads(urllib.request.urlopen(url, timeout=60).read())["data"]
    return pd.DataFrame(rows)


# ---------------------------------------------------------------- calendar
def trading_days():
    """Past days from the downloaded core files; future = business days
    minus the HOLIDAYS list."""
    past = pd.DatetimeIndex(sorted(
        pd.Timestamp(os.path.basename(f)[11:19])
        for f in glob.glob(f"{CORE_DIR}/orats_core_*.csv.gz")))
    fut = pd.bdate_range(past[-1] + pd.Timedelta(days=1),
                         past[-1] + pd.Timedelta(days=400))
    fut = fut[~fut.isin(pd.to_datetime(HOLIDAYS))]
    return past.append(fut)


def last_td_leq(cal, ts):
    i = cal.searchsorted(ts, side="right") - 1
    return cal[i] if i >= 0 else None


def date_move_advice(pos, new_ern, conf, cal, today, ev_by_tkr):
    """Message for an open position whose earnings date moved.

    The trade depends on TWO things: the contract you hold (the first expiry after the
    event) and the exit (last close before the jump). A date move only *needs* a re-entry
    if it invalidates the CONTRACT — i.e. the held expiry no longer sits after the event, so
    the straddle would expire before the jump. If the same expiry still spans the new event,
    nothing about the position is wrong; only the exit date shifts, so re-buying the
    identical option would just pay the spread twice. Distinguish the two."""
    d0 = pd.Timestamp(pos.ernDate)
    row = ev_by_tkr.get(pos.ticker)
    new_when = row.when if row is not None else "?"
    # jump = when the announcement lands; use the calendar row if we have it, else the
    # earliest possible jump (open of new_ern, BMO-conservative) so "still spans" is strict.
    new_jump = pd.Timestamp(row.jump_span) if row is not None else new_ern
    xs = cal[cal < new_jump]
    new_exit = xs[-1] if len(xs) else None
    est = "" if conf else " (est.)"
    tag = f"{pos.ticker:6s} DATE MOVED {d0.date()} -> {new_ern.date()}{est} {new_when}"
    exp = getattr(pos, "expir", "")
    held = pd.Timestamp(exp) if isinstance(exp, str) and exp else None

    if held is not None and held < new_jump:
        return (f"  !!   {tag} — held {held.date()} expiry NO LONGER spans the event: "
                f"SELL and RE-ENTER at the proper lag (needs a new, later contract)")
    if new_exit is None:
        return f"  ??   {tag} — cannot resolve the new exit; verify and EXIT manually"
    if new_exit <= today:
        span = f", same expiry {held.date()}" if held is not None else ""
        return (f"  !!   {tag} — exit now DUE (last close before the jump): "
                f"SELL at today's close{span}")
    span = (f"same expiry {held.date()} still spans it — "
            if held is not None else "expiry not logged; verify it still spans the event — ")
    return (f"  >>   {tag} — {span}adjust EXIT to close {new_exit.date()}, "
            f"do NOT churn the contract")


# ------------------------------------------------- event history (signals)
def _one_file(path):
    try:
        df = pd.read_csv(path, usecols=lambda c: c in USECOLS)
    except Exception as e:
        print(f"SKIP {os.path.basename(path)}: {e}")
        return None
    df = df[df.ticker.isin(_one_file.tickers)]
    return df if len(df) else None


def extend_panel(tickers):
    """ern_panel.parquet (2013-2024, built by pipeline/build_events.py) +
    incremental ern_panel_live.parquet for everything after it."""
    base = pd.read_parquet(f"{CACHE}/ern_panel.parquet")
    live_path = f"{CACHE}/ern_panel_live.parquet"
    live = pd.read_parquet(live_path) if os.path.exists(live_path) else None
    have = max(base.tradeDate.max(),
               live.tradeDate.max() if live is not None else base.tradeDate.max())
    new_files = [f for f in sorted(glob.glob(f"{CORE_DIR}/orats_core_*.csv.gz"))
                 if pd.Timestamp(os.path.basename(f)[11:19]) > have]
    if new_files:
        print(f"extending panel: {len(new_files)} new core files "
              f"(> {have.date()})")
        _one_file.tickers = set(tickers)
        with Pool(8) as p:
            parts = [d for d in p.map(_one_file, new_files, chunksize=10)
                     if d is not None]
        if parts:
            add = pd.concat(parts, ignore_index=True)
            add["tradeDate"] = pd.to_datetime(add.tradeDate)
            add["lastErn"] = pd.to_datetime(add.lastErn, errors="coerce")
            for c in USECOLS[2:]:
                if c != "lastErn":
                    add[c] = pd.to_numeric(add[c], errors="coerce")
            live = add if live is None else pd.concat([live, add])
            live = live.sort_values(["ticker", "tradeDate"])
            live.to_parquet(live_path)
    panel = base if live is None else pd.concat([base, live])
    return panel.sort_values(["ticker", "tradeDate"]).reset_index(drop=True)


def event_history(panel):
    """Per past event: implied at the last pre-event close (ev_imp) and
    |realized| reaction-day move (ev_rlz); then the expanding aggregates the
    four signals need. Mirrors analysis/oq_signals.build_event_table
    (pxCls = PREVIOUS close -> shift(-1) recovers same-day close)."""
    ev = (panel.dropna(subset=["lastErn"])
                .groupby(["ticker", "lastErn"], as_index=False)
                .agg(tod=("lastErnTod", "first")))
    ev = ev.rename(columns={"lastErn": "ernDate"})
    tod = pd.to_numeric(ev.tod, errors="coerce")
    ev["when"] = np.select([tod == 2, tod == 3], ["BMO", "AMC"], default="UNK")
    ev = ev[ev.ernDate >= "2012-06-01"]

    panel = panel.sort_values(["ticker", "tradeDate"])
    panel["close"] = panel.groupby("ticker").pxCls.shift(-1)
    panel["ret"] = panel.groupby("ticker").close.pct_change(fill_method=None)

    rows = []
    for tkr, sub in panel.groupby("ticker"):
        sub = sub.reset_index(drop=True)
        dates = sub.tradeDate.values
        for e in ev[ev.ticker == tkr].itertuples():
            pos = dates.searchsorted(np.datetime64(e.ernDate))
            react = pos + 1 if e.when == "AMC" else pos
            if react < 1 or react >= len(sub):
                continue
            rows.append(dict(ticker=tkr, ernDate=e.ernDate, when=e.when,
                             ev_imp=sub.impErnMv.iloc[react - 1],
                             ev_rlz=abs(sub.ret.iloc[react]) * 100))
    et = pd.DataFrame(rows).sort_values(["ticker", "ernDate"])
    g = et.groupby("ticker")
    et["last_imp"] = g.ev_imp.shift(1)
    et["last_rlz"] = g.ev_rlz.shift(1)
    et["avg_imp"] = g.ev_imp.transform(
        lambda s: s.expanding(min_periods=4).mean().shift(1))
    et["avg_rlz"] = g.ev_rlz.transform(
        lambda s: s.expanding(min_periods=4).mean().shift(1))
    return et


def current_aggregates(et):
    """State per ticker as of NOW (i.e. including its latest completed
    event) — what shift(1)/expanding would give the NEXT event."""
    g = et.groupby("ticker")
    agg = g.agg(last_imp=("ev_imp", "last"), last_rlz=("ev_rlz", "last"),
                n_events=("ev_imp", "count"))
    means = g[["ev_imp", "ev_rlz"]].mean()
    agg["avg_imp"] = np.where(agg.n_events >= 4, means.ev_imp, np.nan)
    agg["avg_rlz"] = np.where(agg.n_events >= 4, means.ev_rlz, np.nan)
    return agg


# ---------------------------------------------------------------- model fit
def fit_model(et):
    """Full-sample analog of analysis/oq_model.build_model_book: OLS of
    %/spot on the 4 signals over ALL backtest lag-15 trades (2013-2024),
    features winsorized at the training 1/99 pctiles."""
    tr = pd.read_parquet(f"{CACHE}/trades_prern.parquet")
    d = tr[tr.lag == 15].dropna(subset=["impErnMv"]).merge(
        et[["ticker", "ernDate", "last_imp", "last_rlz", "avg_imp", "avg_rlz"]],
        on=["ticker", "ernDate"], how="left")
    m = d.impErnMv
    d["s1_vs_last_imp"] = m / d.last_imp.replace(0, np.nan)
    d["s2_vs_last_rlz"] = m - d.last_rlz
    d["s3_vs_avg_imp"] = m / d.avg_imp.replace(0, np.nan)
    d["s4_vs_avg_rlz"] = m - d.avg_rlz
    d = d.replace([np.inf, -np.inf], np.nan).dropna(subset=SIGS)
    lo, hi = d[SIGS].quantile(0.01), d[SIGS].quantile(0.99)
    X = np.c_[np.ones(len(d)), d[SIGS].clip(lo, hi, axis=1).values]
    beta, *_ = np.linalg.lstsq(X, d.ret_spot_mid.values, rcond=None)
    kept = (X @ beta > 0).mean()
    print(f"model fit: {len(d)} trades 2013-2024, in-sample keep-rate "
          f"{kept*100:.0f}%, betas {np.round(beta*100, 4).tolist()} (x100)")
    with open(f"{CACHE}/live_model.json", "w") as f:
        json.dump(dict(beta=beta.tolist(), lo=lo.tolist(), hi=hi.tolist(),
                       sigs=SIGS, fitted=str(dt.date.today()),
                       n_train=len(d)), f, indent=1)
    return beta, lo.values, hi.values


def load_model(et):
    p = f"{CACHE}/live_model.json"
    if os.path.exists(p):
        m = json.load(open(p))
        return np.array(m["beta"]), np.array(m["lo"]), np.array(m["hi"])
    return fit_model(et)


# ------------------------------------------------------------- live snapshot
def yahoo_next_ern(tkr):
    """(date, confirmed) from yfinance; None if unavailable."""
    import logging
    import yfinance as yf
    logging.getLogger("yfinance").setLevel(logging.CRITICAL)
    try:
        ds = yf.Ticker(tkr.replace("_", "-")).calendar.get("Earnings Date", [])
    except Exception:
        return None, False
    if not ds:
        return None, False
    return pd.Timestamp(ds[0]), len(ds) == 1


def straddle_quote(tkr, jump_span, tok):
    """Live chain -> first expiry spanning the jump, ATM strike, quotes."""
    s = api("strikes", [tkr], tok)
    s["expirDate"] = pd.to_datetime(s.expirDate)
    spot = s.stockPrice.iloc[0]
    exps = s.expirDate[s.expirDate >= jump_span]
    if exps.empty:
        return None
    ch = s[s.expirDate == exps.min()]
    r = ch.iloc[(ch.strike - spot).abs().values.argmin()]
    return dict(spot=spot, expir=exps.min(), strike=r.strike,
                mid=(r.callBidPrice + r.callAskPrice) / 2
                    + (r.putBidPrice + r.putAskPrice) / 2,
                bid=r.callBidPrice + r.putBidPrice,
                ask=r.callAskPrice + r.putAskPrice,
                atm_dist=abs(r.strike - spot) / spot)


def net_delta(spot, K, atm_mid):
    """Straddle net delta 2*Phi(d1)-1; remaining sig*sqrt(T) backed out of
    the current ATM straddle mid (see analysis/restrike_delta.py)."""
    sv = atm_mid / (0.8 * spot)
    if not sv > 1e-4:
        return 1.0 if spot > K else -1.0
    d1 = (np.log(spot / K) + sv * sv / 2) / sv
    from math import erf
    return erf(d1 / np.sqrt(2))        # 2*Phi(d1)-1


def open_positions(today, book=None, after_exit=False):
    """Last BUY/RESTRIKE row per ticker+ernDate from the signal log with
    exit >= today (strictly > with after_exit). Signals, not fills —
    ignore names you didn't actually trade."""
    lp = f"{OUTDIR}/live_signals.csv"
    if not os.path.exists(lp):
        return pd.DataFrame()
    d = pd.read_csv(lp)
    ex = pd.to_datetime(d.exit)
    d = d[d.action.isin(["BUY", "RESTRIKE"])
          & ((ex > today) if after_exit else (ex >= today))]
    if book:
        d = d[d.book == book]
    if d.empty:
        return d
    return (d.sort_values("run")
             .groupby(["ticker", "ernDate", "book"], as_index=False).last())


def open_lag15_positions(today):
    d = open_positions(today, book="lag15_model", after_exit=True)
    return d[d.strike.notna()] if len(d) else d


def restrike_check(pos, tok, unit):
    s = api("strikes", [pos.ticker], tok)
    s["expirDate"] = pd.to_datetime(s.expirDate)
    ch = s[s.expirDate == pd.Timestamp(pos.expir)]
    if ch.empty:
        return (f"  ??   {pos.ticker:6s} expiry {pos.expir} not in live "
                f"chain"), None
    spot = ch.stockPrice.iloc[0]
    mids = dict(zip(ch.strike,
                    (ch.callBidPrice + ch.callAskPrice) / 2
                    + (ch.putBidPrice + ch.putAskPrice) / 2))
    k_atm = ch.strike.iloc[(ch.strike - spot).abs().values.argmin()]
    if pos.strike not in mids:
        return f"  ??   {pos.ticker:6s} strike {pos.strike:g} not quoted", None
    nd = net_delta(spot, pos.strike, mids[k_atm])
    head = (f"{pos.ticker:6s} K {pos.strike:g}  spot {spot:.2f}  "
            f"net delta {nd:+.2f}")
    if abs(nd) < DELTA_RESTRIKE or k_atm == pos.strike:
        return f"  ok   {head} — hold", None
    m_old, m_new = mids[pos.strike], mids[k_atm]
    n_old = pos.contracts if np.isfinite(pos.contracts) else 0
    line = (f"  >>   {head}  RESTRIKE: close {pos.strike:g} straddle "
            f"(mid {m_old:.2f}), open {k_atm:g} (mid {m_new:.2f})")
    n_new = np.nan
    if n_old and unit and m_new >= MIN_DEBIT:
        proceeds = n_old * 100 * m_old
        spend = min(proceeds, unit)     # bank-excess sizing
        n_new = int(spend // (100 * m_new))
        line += (f"  {n_old:.0f}x -> {n_new}x = ${n_new*100*m_new:,.0f}"
                 + (f", bank ${proceeds-spend:,.0f}" if proceeds > spend
                    else ""))
    return line, dict(strike=k_atm, mid=m_new, contracts=n_new)


def n_contracts(q, unit):
    """Equal-premium sizing: spend ~unit $ of debit per trade."""
    if not unit or q is None or q["mid"] < MIN_DEBIT:
        return None
    return int(unit // (100 * q["mid"]))


def fmt_quote(q, unit=0):
    if q is None:
        return "no chain past the jump"
    warn = []
    if q["atm_dist"] > MAX_ATM_DIST:
        warn.append("ATM>10% off")
    if q["mid"] < MIN_DEBIT:
        warn.append(f"debit<${MIN_DEBIT:.2f} — fees eat it, SKIP")
    if q["spot"] < MIN_PX:
        warn.append("spot<$5")
    # relative spread on the two-leg straddle: (ask-bid)/mid. You cross half of
    # it entering and half exiting, so this is ~the round-trip cost in % of debit
    spr = (q["ask"] - q["bid"]) / q["mid"] * 100 if q["mid"] > 0 else float("nan")
    size = ""
    n = n_contracts(q, unit)
    if n is not None:
        size = (f"  ->  {n}x = ${n * 100 * q['mid']:,.0f} premium" if n
                else "  ->  0 contracts (unit < 1 straddle) — SKIP")
    return (f"spot {q['spot']:.2f}  exp {q['expir'].date()}  K {q['strike']:g}"
            f"  straddle {q['bid']:.2f}/{q['mid']:.2f}/{q['ask']:.2f}"
            f"  (spread {spr:.1f}%, mid {q['mid']/q['spot']*100:.1f}% of spot)"
            + ("  !! " + ", ".join(warn) if warn else "") + size)


# --------------------------------------------------------------------- main
def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--refit", action="store_true",
                    help="refit model betas before screening")
    ap.add_argument("--unit", type=float, default=0,
                    help="premium $ per trade (equal-premium sizing); "
                         "prints contracts = unit // (100*mid); overrides "
                         "--capital/--pct")
    ap.add_argument("--capital", type=float, default=0,
                    help="account size $; unit is solved from the backtest "
                         "to hit --pct annual std (analysis/unit_for_risk)")
    ap.add_argument("--pct", type=float, default=20,
                    help="target annual std as %% of capital (default 20)")
    args = ap.parse_args()

    # sizing is mandatory — without a unit every quote prints with no contract
    # count, which is the whole point of the run
    if not args.unit and not args.capital:
        ap.error("sizing required: pass --capital (unit solved for --pct "
                 "annual std) or --unit (fixed premium per trade)")
    if args.unit < 0 or args.capital < 0:
        ap.error("--unit and --capital must be positive")
    if args.pct <= 0:
        ap.error("--pct must be > 0 (it scales --capital into a risk target)")

    if not args.unit and args.capital:
        sys.path.insert(0, f"{HERE}/analysis")
        import unit_for_risk
        b = unit_for_risk.load()
        risk = args.capital * args.pct / 100
        args.unit = unit_for_risk.solve(risk, b)
        print(f"sizing: ${args.capital:,.0f} @ {args.pct:g}% ann std "
              f"(${risk:,.0f}) -> --unit {args.unit:.0f} "
              f"(realized ann std ${unit_for_risk.ann_std(b, args.unit):,.0f}"
              f", lag-2 book)")

    tok = token()
    tickers = pd.read_csv(TOP200).ticker.tolist()
    # keep names with OPEN logged positions even if they left the universe
    # (their EXIT lines come from the live scan, not the log)
    lp = f"{OUTDIR}/live_signals.csv"
    if os.path.exists(lp):
        sig = pd.read_csv(lp, parse_dates=["exit"])
        held = sig[(sig.action.isin(["BUY", "RESTRIKE"]))
                   & (sig["exit"] >= pd.Timestamp.now().normalize())]
        extra = sorted(set(held.ticker) - set(tickers))
        if extra:
            print(f"open positions outside universe kept: {', '.join(extra)}")
            tickers += extra
    cal = trading_days()
    now_et = pd.Timestamp.now(tz="US/Eastern")
    today = last_td_leq(cal, now_et.tz_localize(None).normalize())
    print(f"run {now_et:%Y-%m-%d %H:%M} ET — screening for trading day "
          f"{today.date()} ({len(tickers)} names)")
    if not (9.5 <= now_et.hour + now_et.minute / 60 < 16
            and now_et.weekday() < 5):
        print("!! market CLOSED — straddle quotes below are stale/indicative;"
              " rerun before the close for tradeable marks")
    print()

    panel = extend_panel(tickers)
    et = event_history(panel)
    beta, lo, hi = (fit_model(et) if args.refit else load_model(et))
    agg = current_aggregates(et)

    live = api("cores", tickers, tok)
    for c in ["impErnMv", "wksNextErn", "nextErnTod", "pxCls"]:
        live[c] = pd.to_numeric(live[c], errors="coerce")
    cand = live[live.wksNextErn <= 4].copy()
    print(f"live snapshot: {len(live)} names, {len(cand)} within ~4 weeks "
          f"of earnings; resolving dates via yfinance...")

    rows = []
    for r in cand.itertuples():
        ern, confirmed = yahoo_next_ern(r.ticker)
        if ern is None or ern < today:
            continue
        if abs((ern - today).days - 7 * r.wksNextErn) > 12:
            print(f"  ?? {r.ticker}: yahoo {ern.date()} vs ORATS "
                  f"wksNextErn {r.wksNextErn} — mismatch, skipped")
            continue
        when = ("BMO" if r.nextErnTod < 1200 else
                "AMC" if r.nextErnTod >= 1500 else "UNK")
        if when == "UNK" and r.nextErnTod == 2359:
            when = "UNK"
        cutoff = ern + pd.Timedelta(days=1) if when == "AMC" else ern
        xs = cal[cal < cutoff]
        rows.append(dict(
            ticker=r.ticker, ernDate=ern, when=when, confirmed=confirmed,
            px=r.pxCls, impErnMv=r.impErnMv,
            entry2=last_td_leq(cal, ern - pd.Timedelta(days=2)),
            entry15=last_td_leq(cal, ern - pd.Timedelta(days=15)),
            exit=xs[-1] if len(xs) else None,
            jump_span=ern if when == "BMO" else ern + pd.Timedelta(days=1)))
    ev = pd.DataFrame(rows)
    if ev.empty:
        print("no upcoming earnings resolved — nothing to do")
        return

    ev = ev.merge(agg, left_on="ticker", right_index=True, how="left")
    m = ev.impErnMv
    ev["s1_vs_last_imp"] = m / ev.last_imp.replace(0, np.nan)
    ev["s2_vs_last_rlz"] = m - ev.last_rlz
    ev["s3_vs_avg_imp"] = m / ev.avg_imp.replace(0, np.nan)
    ev["s4_vs_avg_rlz"] = m - ev.avg_rlz
    X = ev[SIGS].clip(pd.Series(lo, SIGS), pd.Series(hi, SIGS), axis=1).values
    ev["pred"] = np.where(ev[SIGS].notna().all(axis=1),
                          np.c_[np.ones(len(ev)), X] @ beta, np.nan)

    log = []

    def act(book, r, note=""):
        q = straddle_quote(r.ticker, r.jump_span, tok)
        line = (f"  BUY  {r.ticker:6s} reports {r.ernDate.date()} {r.when}"
                f"{'' if r.confirmed else ' (DATE UNCONFIRMED)'}  "
                f"exit at close {r.exit.date()}  |  "
                f"{fmt_quote(q, args.unit)}{note}")
        print(line)
        n = n_contracts(q, args.unit)
        log.append(dict(run=str(today.date()), book=book, action="BUY",
                        ticker=r.ticker, ernDate=str(r.ernDate.date()),
                        when=r.when, confirmed=r.confirmed,
                        exit=str(r.exit.date()),
                        expir=str(q["expir"].date()) if q else "",
                        strike=q["strike"] if q else np.nan,
                        straddle_mid=q["mid"] if q else np.nan,
                        spot=q["spot"] if q else np.nan,
                        contracts=n if n is not None else np.nan,
                        pred=r.pred if book == "lag15_model" else np.nan))

    e2 = ev[(ev.entry2 == today) & ev.confirmed]
    print(f"\n== LAG-2 BOOK: enter at TODAY'S close ({len(e2)}) ==")
    for r in e2.itertuples():
        act("lag2", r)
    if e2.empty:
        print("  (none)")

    e15 = ev[ev.entry15 == today].copy()
    print(f"\n== LAG-15 MODEL BOOK: candidates entering today "
          f"({len(e15)}) ==")
    for r in e15.itertuples():
        if not np.isfinite(r.pred):
            print(f"  --   {r.ticker:6s} pred n/a (<4 prior events) — skip")
        elif r.pred <= 0:
            print(f"  --   {r.ticker:6s} pred {r.pred*100:+.2f}%/spot <= 0 "
                  f"— model says skip")
        else:
            act("lag15_model", r, note=f"  pred {r.pred*100:+.2f}%")
    if e15.empty:
        print("  (none)")

    opos = open_positions(today)
    if len(opos):
        print(f"\n== DATE CHECK: open positions vs current calendar "
              f"({len(opos)}) ==")
        known = {r.ticker: (r.ernDate, r.confirmed) for r in ev.itertuples()}
        ev_by_tkr = {r.ticker: r for r in ev.itertuples()}
        n_ok = 0
        for pos in opos.itertuples():
            d0 = pd.Timestamp(pos.ernDate)
            ern, conf = known.get(pos.ticker) or yahoo_next_ern(pos.ticker)
            if ern is None:
                print(f"  ??   {pos.ticker:6s} logged {d0.date()} — can't "
                      f"resolve current date, verify manually")
            elif ern != d0 and abs((ern - d0).days) <= 45:
                # smart advice: only SELL & re-enter if the held expiry no longer spans the
                # event; if the same contract still works, just shift the exit (no churn).
                print(date_move_advice(pos, ern, conf, cal, today, ev_by_tkr))
            elif ern != d0:
                print(f"  !!   {pos.ticker:6s} logged {d0.date()} but yahoo "
                      f"now shows {ern.date()} — event likely passed or "
                      f"pulled, verify and EXIT")
            else:
                n_ok += 1
        if n_ok:
            print(f"  ok   {n_ok} position(s) confirmed on schedule")

    ex = ev[ev["exit"] == today]
    print(f"\n== EXITS: sell at TODAY'S close if held ({len(ex)}) ==")
    for r in ex.itertuples():
        print(f"  SELL {r.ticker:6s} reports {r.ernDate.date()} {r.when} — "
              f"last close before the jump")
    if ex.empty:
        print("  (none)")

    op = open_lag15_positions(today)
    print(f"\n== RESTRIKE CHECK: open lag-15 positions, trigger |net delta| "
          f">= {DELTA_RESTRIKE} ({len(op)}) ==")
    if len(op):
        print("  (positions from the signal log — ignore any you didn't "
              "actually trade; never restrike on the exit day)")
    for pos in op.itertuples():
        line, new = restrike_check(pos, tok, args.unit)
        print(line)
        if new is not None:
            log.append(dict(run=str(today.date()), book="lag15_model",
                            action="RESTRIKE", ticker=pos.ticker,
                            ernDate=pos.ernDate, when=pos.when,
                            confirmed=True, exit=pos.exit, expir=pos.expir,
                            strike=new["strike"], straddle_mid=new["mid"],
                            spot=np.nan, contracts=new["contracts"],
                            pred=np.nan))
    if op.empty:
        print("  (none)")

    week = cal[cal > today][:5]
    w = ev[(ev.entry2.isin(week)) | (ev.entry15.isin(week))]
    print(f"\n== WATCHLIST: entries in the next 5 trading days ==")
    for r in w.sort_values("ernDate").itertuples():
        which = []
        if r.entry2 in week:
            which.append(f"lag2 {r.entry2.date()}")
        if r.entry15 in week:
            p = (f"pred {r.pred*100:+.2f}%" if np.isfinite(r.pred)
                 else "pred n/a")
            which.append(f"model {r.entry15.date()} ({p} today)")
        print(f"  {r.ticker:6s} reports {r.ernDate.date()} {r.when}"
              f"{'' if r.confirmed else ' (unconfirmed)'}: "
              + ";  ".join(which))
    if w.empty:
        print("  (none)")

    if log:
        os.makedirs(OUTDIR, exist_ok=True)
        lp = f"{OUTDIR}/live_signals.csv"
        new = pd.DataFrame(log)
        if os.path.exists(lp):
            new = pd.concat([pd.read_csv(lp), new], ignore_index=True)
        new.drop_duplicates(subset=["run", "book", "action", "ticker"],
                            keep="last").to_csv(lp, index=False)
        print(f"\nlogged {len(log)} signals to out/live_signals.csv")


if __name__ == "__main__":
    main()
