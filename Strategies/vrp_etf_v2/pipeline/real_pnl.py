#!/usr/bin/env python3
"""
Price the video's trade with REAL quotes instead of the variance-swap formula.

The entire vrp_etf_model study -- signals, regression, equity curves, the Sharpe 2.04 --
was scored on

    P&L / premium  ~  1 - (RV/IV)^2

which is a continuous-limit approximation of a delta-hedged short straddle. It assumes a
constant dollar-gamma, a costless continuous rehedge, and that the option you sold is
exactly a 30-day ATM variance contract. None of those hold. `_data/strikes` now carries
per-strike NBBO *and* per-strike delta, so the trade can simply be simulated.

The simulation, per entry:
  - sell 1 ATM call + 1 ATM put, expiry nearest 30 `dte_ltd`, strike nearest spot
  - each session, hold H = 2*delta - 1 shares (the hedge that flattens a short straddle;
    delta here is the archive's CALL delta, put delta = delta - 1)
  - hedge P&L accumulates H_t * (S_{t+1} - S_t)
  - at `dte_ltd == 0` the straddle settles to |S_T - K| intrinsic. Terminal spot is taken
    from the dte_ltd==0 row UNFILTERED by quote validity: requiring two-sided quotes there
    drops ~half of all cycles and silently takes the terminal spot a day early.

GROSS FIRST: `pnl_mid` crosses no spread at all. `pnl_taker` pays the full option spread
both sides plus a hedging cost on shares. They are separate columns, never blended.

SPLITS. `stk` and `strike` are raw, unadjusted. A split inside the hold would look like a
50% crash and destroy the hedge. It is detected STRUCTURALLY -- at a split the whole strike
grid rescales with the spot, in a crash only the spot moves -- never by the size of the
return. Filtering on "the move was too big" is what corrupted `straRetM1`; it deletes
exactly the crashes a short-vol study exists to measure.

Run: /home/marco/trading/.venv_orats/bin/python real_pnl.py [etf|stock]
Writes cache/real_trades_<kind>.parquet
"""
import sys
from concurrent.futures import ProcessPoolExecutor

import numpy as np
import pandas as pd

sys.path.insert(0, "/home/marco/trading/Systems/Options/Strategies/vrp_etf_v2/analysis")
from common import BASE, load  # noqa: E402

STRIKES = "/home/marco/trading/Systems/Options/Strategies/_data/strikes"
DTE_LO, DTE_HI, DTE_TGT = 20, 45, 30
MAX_MONEY = 0.03          # entry gate: nearest strike within 3% of spot, else no ATM to sell
SHARE_COST_BPS = 2.0      # round-trip share cost per rehedge leg, taker column only
COLS = ["tdate", "expir", "dte_ltd", "strike", "stk", "cBid", "cAsk", "pBid", "pAsk", "delta"]


def split_days(ch):
    """Sessions whose spot move is impossible for the name's OWN local scale -> a corporate
    action, not a market move.

    z = |r_t| / (1.4826 * rolling median |r|) over a centred 121-session window. A 2:1 split
    scores z~60; the worst real sessions in this sample score z<9 (EWA -14.9% in Mar-2020 is
    z=8.8, SPY -8.7% is z=5.8). Verified to catch EWT 2016-11-07, USO 2020-04-29 and the two
    UNG reverse splits while flagging nothing in the COVID crash.

    The scale is LOCAL and robust, which is the whole point: during a crash the denominator
    rises with the numerator, so volatility itself never trips the filter. A plain "drop
    moves over 25%" rule would delete precisely the sessions a short-vol study exists to
    measure -- the `straRetM1` mistake.

    The same test also catches 2021-05-05, the session where quotes resume after the
    2021-01..04 archive hole; that "return" is four months long and any trade touching it
    is meaningless.
    """
    s = ch.drop_duplicates("td").set_index("td").sort_index().stk
    r = np.log(s / s.shift(1))
    scale = 1.4826 * r.abs().rolling(121, center=True, min_periods=40).median()
    z = r.abs() / scale
    hit = ((z > 12) & (r.abs() > 0.20)).fillna(False)
    return set(s.index[hit])


def one_ticker(args):
    tk, entries = args
    try:
        ch = pd.read_parquet(f"{STRIKES}/ticker={tk}", columns=COLS)
    except Exception:
        return None
    ch["td"] = pd.to_datetime(ch.tdate)
    ch["ex"] = pd.to_datetime(ch.expir)
    ch = ch[(ch.dte_ltd >= 0) & (ch.stk > 0)]
    if ch.empty:
        return None
    bad = split_days(ch)
    spot = ch.groupby("td").stk.first()
    out = []

    for d in entries:
        d = pd.Timestamp(d)
        day = ch[ch.td == d]
        if day.empty:
            continue
        cand = day[(day.dte_ltd >= DTE_LO) & (day.dte_ltd <= DTE_HI)]
        if cand.empty:
            continue
        dtes = cand.dte_ltd.unique()
        ex = cand[cand.dte_ltd == dtes[np.argmin(np.abs(dtes - DTE_TGT))]].ex.iloc[0]
        leg = cand[(cand.ex == ex) & (cand.cBid > 0) & (cand.pBid > 0)
                   & (cand.cAsk > cand.cBid) & (cand.pAsk > cand.pBid)]
        if leg.empty:
            continue
        S0 = leg.stk.iloc[0]
        i = (leg.strike - S0).abs().idxmin()
        K = leg.strike.loc[i]
        if abs(K / S0 - 1) > MAX_MONEY:
            continue                                   # no real ATM strike listed
        e = leg.loc[i]

        # daily path of this exact (expiry, strike) from entry to expiry
        p = ch[(ch.ex == ex) & (ch.strike == K) & (ch.td >= d)].sort_values("td")
        p = p[p.td <= p[p.dte_ltd == 0].td.min()] if (p.dte_ltd == 0).any() else p
        if len(p) < 5 or not (p.dte_ltd == 0).any():
            continue                                   # no settlement row -> drop, never fill
        days = p.td.tolist()
        if bad & set(days):
            continue                                   # split inside the hold

        S = spot.reindex(days).values
        if np.isnan(S).any():
            continue
        # hedge: hold H = 2*delta - 1 shares from t to t+1, flat after the last session
        H = (2.0 * p.delta.values - 1.0)[:-1]
        hedge = float(np.sum(H * np.diff(S)))
        n_reh = int(np.sum(np.abs(np.diff(np.append(0.0, H))) > 1e-9))

        prem_mid = (e.cBid + e.cAsk + e.pBid + e.pAsk) / 4.0 * 2.0   # sold at mid
        prem_bid = e.cBid + e.pBid                                   # sold at bid
        ST = S[-1]
        settle = abs(ST - K)                                         # intrinsic at expiry

        pnl_mid = prem_mid - settle + hedge
        share_cost = SHARE_COST_BPS / 1e4 * np.sum(np.abs(np.diff(np.append(0.0, H)))) * S.mean()
        pnl_taker = prem_bid - settle + hedge - share_cost
        margin = 0.20 * S0 + prem_mid

        out.append(dict(ticker=tk, tradeDate=d, expir=ex, dte=int(e.dte_ltd), strike=K,
                        S0=S0, ST=ST, held=len(days) - 1, K_money=K / S0 - 1,
                        prem_mid=prem_mid, prem_bid=prem_bid, settle=settle, hedge=hedge,
                        n_rehedge=n_reh, pnl_mid=pnl_mid, pnl_taker=pnl_taker,
                        r_mid=pnl_mid / margin, r_taker=pnl_taker / margin,
                        ppp_mid=pnl_mid / prem_mid, margin=margin))
    return pd.DataFrame(out) if out else None


def main():
    kind = sys.argv[1] if len(sys.argv) > 1 else "etf"
    d = load(kind)
    d = d[(d.tradeDate >= "2013-01-01") & (d.tradeDate <= "2024-11-15")]
    d["ym"] = d.tradeDate.values.astype("datetime64[M]")
    m = d.sort_values("tradeDate").groupby(["ticker", "ym"], as_index=False).first()
    jobs = [(tk, g.tradeDate.tolist()) for tk, g in m.groupby("ticker")]
    print(f"{kind}: {len(m):,} monthly entries over {len(jobs)} tickers, "
          f"{m.ym.min():%Y-%m}..{m.ym.max():%Y-%m}")

    parts = []
    with ProcessPoolExecutor(10) as ex:
        for i, r in enumerate(ex.map(one_ticker, jobs, chunksize=4)):
            if r is not None:
                parts.append(r)
            if (i + 1) % 100 == 0:
                print(f"  {i+1}/{len(jobs)} tickers, {sum(len(p) for p in parts):,} trades")
    t = pd.concat(parts, ignore_index=True)
    # carry the signals over so the trades can be sorted by them
    t = t.merge(m[["ticker", "tradeDate", "f_ivrv", "f_ivpct", "f_ffr", "f_vov", "f_voiv",
                   "f_ivlvl", "y_risk", "y_margin", "iv30d", "clsHv20d", "fwd_rv",
                   "avgOptVolu20d"]], on=["ticker", "tradeDate"], how="left")
    out = f"{BASE}/cache/real_trades_{kind}.parquet"
    t.to_parquet(out, index=False, compression="zstd")
    print(f"\nwrote {out}: {len(t):,} trades, {t.ticker.nunique()} tickers "
          f"({100*len(t)/len(m):.0f}% of entries priced)")
    print(f"gross mean r_mid {100*t.r_mid.mean():+.3f}%  taker {100*t.r_taker.mean():+.3f}%  "
          f"win {100*(t.r_mid>0).mean():.1f}%")


if __name__ == "__main__":
    main()
