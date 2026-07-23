"""
Out-of-sample replication of the pre-earnings straddle books on an INDEPENDENT
dataset: RobotWealth "macro-pod" earnings_straddles.feather (ORATS-sourced but
a different pull, 4108 tickers, 2006 to Feb-2024, real NBBO on both legs).

Books replicated (same trade logic as pipeline/backtest_prern.py: buy the ATM
straddle on the first expiry after the report, LAG calendar days before it,
sell at the last close strictly BEFORE the announcement):
  A  lag-2  ungated                      -- the recommended live book
  N  lag-15 ungated                      -- naive, no signals
  B  lag-15 + s4-low gate (past-only expanding median)
  B' lag-15 + walk-forward 4-signal OLS, trade when predicted return > 0

CAVEATS, both structural to this dataset (see README):
  - dtc=0 is the EXIT day (last close before the jump), not the earnings
    date; entry lags are taken off earningsDate to match our convention.
  - it carries ONE fixed strike per event, ATM as of ~14 days out, so the
    lag-2 book cannot re-strike at entry the way ours does; the 10% ATM
    filter is what keeps it honest (and induces a mild low-drift selection).
  - extSmvVol is unusable here, so ORATS impErnMv cannot be rebuilt; the
    s1-s4 signals use a straddle-premium PROXY for the implied move,
    measured at the entry row. Directionally the same signal, not identical.

Run from analysis/:  python3 macropod.py
"""
import numpy as np
import pandas as pd

FEATHER = ("/home/marco/trading/RobotWealth/macro-pod/research/options/"
           "straddle_over_earnings/macropod/earnings_straddles.feather")
LAGS = [2, 14]          # 14 not 15: 78% of macropod panels start at -14
MIN_PX = 5.0
MAX_ATM_DIST = 0.10
MAX_DEBIT_SPOT = 0.40   # data-quality cap; our own book tops out at 0.386
SIGS = ["s1_vs_last_imp", "s2_vs_last_rlz", "s3_vs_avg_imp", "s4_vs_avg_rlz"]
COLS = ["ticker", "tradeDate", "earningsDate", "earningsTime", "daysToCover",
        "side", "dte", "strike", "stockPrice", "callBidPrice", "callAskPrice",
        "putBidPrice", "putAskPrice", "callVolume", "putVolume"]


def tstat(r):
    return r.mean() / r.std() * np.sqrt(len(r)) if len(r) > 2 else np.nan


def monthly_sharpe(d, col="ret_spot_mid"):
    if not len(d):
        return np.nan
    idx = pd.date_range(d.exit.min().normalize().replace(day=1),
                        d.exit.max(), freq="ME")
    m = d.set_index("exit")[col].resample("ME").sum().reindex(idx, fill_value=0.0)
    return m.mean() / m.std() * np.sqrt(12) if m.std() > 0 else np.nan


# ------------------------------------------------------------------ loading
def load():
    d = pd.read_feather(FEATHER, columns=COLS)
    for c in ["tradeDate", "earningsDate"]:
        d[c] = pd.to_datetime(d[c])
    d["cmid"] = (d.callBidPrice + d.callAskPrice) / 2
    d["pmid"] = (d.putBidPrice + d.putAskPrice) / 2
    d["mid"] = d.cmid + d.pmid
    d["ok"] = ((d.callAskPrice > 0) & (d.putAskPrice > 0)
               & (d.callBidPrice >= 0) & (d.putBidPrice >= 0)
               & (d.callAskPrice >= d.callBidPrice)
               & (d.putAskPrice >= d.putBidPrice))
    return d


def realized_moves(d):
    """|earnings reaction move| %, from the close before the jump to the
    close after it, using the stock prices carried in both panels."""
    px = (d.groupby(["ticker", "tradeDate"], as_index=False).stockPrice.first()
           .sort_values(["ticker", "tradeDate"]))
    px["nxt"] = px.groupby("ticker").stockPrice.shift(-1)
    rows = []
    for (tkr, ern), g in d.groupby(["ticker", "earningsDate"], sort=False):
        when = g.earningsTime.iloc[0]
        pre = g[(g.side == "long") & (g.daysToCover < 0)]
        day0 = g[g.daysToCover == 0]
        # AMC jumps overnight from the report-day close; BMO from the prior close
        anchor = day0 if when == "amc" else pre
        if not len(anchor):
            continue
        rows.append((tkr, ern, anchor.tradeDate.max()))
    a = pd.DataFrame(rows, columns=["ticker", "earningsDate", "anchor"])
    a = a.merge(px.rename(columns={"tradeDate": "anchor"}),
                on=["ticker", "anchor"], how="left")
    a["ev_rlz"] = (a.nxt / a.stockPrice - 1).abs() * 100
    return a[["ticker", "earningsDate", "ev_rlz"]]


# ------------------------------------------------------------------- trades
def build_trades(d, min_debit):
    L = d[(d.side == "long") & (d.daysToCover <= 0) & d.ok].copy()
    # dtc is anchored on the EXIT day (last close before the jump), NOT on the
    # earnings date: for a BMO report dtc=0 is the day before it. So exit is
    # always the dtc=0 row, and entry lags are measured off earningsDate
    # separately to match our own backtest's convention.
    L["ern_off"] = (L.tradeDate - L.earningsDate).dt.days
    out = []
    for (tkr, ern), g in L.groupby(["ticker", "earningsDate"], sort=False):
        when = g.earningsTime.iloc[0]
        g = g.sort_values("daysToCover")
        # last close strictly before the jump == the dtc=0 row, both sessions
        ex = g[g.daysToCover == 0]
        if not len(ex):
            continue
        xr = ex.iloc[-1]
        for lag in LAGS:
            en = g[g.ern_off <= -lag]
            if not len(en):
                continue
            er = en.iloc[-1]
            if er.tradeDate >= xr.tradeDate:
                continue
            if not (er.stockPrice >= MIN_PX and er.mid >= min_debit):
                continue
            if (er.mid / er.stockPrice > MAX_DEBIT_SPOT
                    or xr.mid / xr.stockPrice > MAX_DEBIT_SPOT):
                continue      # bad prints: $10k straddles, straddle > spot
            if abs(er.strike - er.stockPrice) / er.stockPrice > MAX_ATM_DIST:
                continue
            out.append(dict(
                ticker=tkr, ernDate=ern,
                when={"amc": "AMC", "bmo": "BMO"}.get(when, "UNK"), lag=lag,
                entry=er.tradeDate, exit=xr.tradeDate, strike=er.strike,
                spot=er.stockPrice, debit_mid=er.mid, exit_mid=xr.mid,
                vol=er.callVolume + er.putVolume,
                ret_spot_mid=(xr.mid - er.mid) / er.stockPrice,
                ret_debit_mid=(xr.mid - er.mid) / er.mid))
    return pd.DataFrame(out)


def add_signals(tr, rlz):
    """s1-s4 exactly as in screener.py, with a straddle-premium PROXY for
    the implied earnings move (see module docstring)."""
    ev = tr[tr.lag == 14][["ticker", "ernDate", "debit_mid", "spot"]].copy()
    ev["ev_imp"] = ev.debit_mid / ev.spot * 100
    ev = ev.merge(rlz.rename(columns={"earningsDate": "ernDate"}),
                  on=["ticker", "ernDate"], how="left")
    ev = ev.sort_values(["ticker", "ernDate"])
    g = ev.groupby("ticker")
    ev["last_imp"] = g.ev_imp.shift(1)
    ev["last_rlz"] = g.ev_rlz.shift(1)
    ev["avg_imp"] = g.ev_imp.transform(
        lambda s: s.expanding(min_periods=4).mean().shift(1))
    ev["avg_rlz"] = g.ev_rlz.transform(
        lambda s: s.expanding(min_periods=4).mean().shift(1))

    d = tr.merge(ev[["ticker", "ernDate", "ev_imp", "last_imp", "last_rlz",
                     "avg_imp", "avg_rlz"]], on=["ticker", "ernDate"],
                 how="left")
    m = d.ev_imp
    d["s1_vs_last_imp"] = m / d.last_imp.replace(0, np.nan)
    d["s2_vs_last_rlz"] = m - d.last_rlz
    d["s3_vs_avg_imp"] = m / d.avg_imp.replace(0, np.nan)
    d["s4_vs_avg_rlz"] = m - d.avg_rlz
    return d.replace([np.inf, -np.inf], np.nan)


# -------------------------------------------------------------------- books
def s4_gate(d):
    d = d.dropna(subset=["s4_vs_avg_rlz"]).sort_values("entry").copy()
    med = d.s4_vs_avg_rlz.expanding(min_periods=200).median().shift(1)
    return d[d.s4_vs_avg_rlz <= med]


def wf_model(d, first_test_year):
    d = d.dropna(subset=SIGS).copy()
    d["year"] = d.exit.dt.year
    keep = []
    for y in range(first_test_year, int(d.year.max()) + 1):
        tr_s, te_s = d[d.year < y], d[d.year == y]
        if len(te_s) == 0 or len(tr_s) < 500:
            continue
        lo, hi = tr_s[SIGS].quantile(0.01), tr_s[SIGS].quantile(0.99)
        A = np.c_[np.ones(len(tr_s)), tr_s[SIGS].clip(lo, hi, axis=1).values]
        beta, *_ = np.linalg.lstsq(A, tr_s.ret_spot_mid.values, rcond=None)
        X = np.c_[np.ones(len(te_s)), te_s[SIGS].clip(lo, hi, axis=1).values]
        keep.append(te_s[X @ beta > 0])
    return pd.concat(keep) if keep else d.iloc[:0]


def report(title, books):
    print(f"\n{title}")
    print(f"{'book':<26}{'n':>7}{'%/spot':>9}{'t':>7}{'win':>6}"
          f"{'moSh':>7}{'%/debit':>9}")
    for lab, b in books.items():
        if not len(b):
            print(f"{lab:<26}{'-':>7}")
            continue
        r = b.ret_spot_mid
        print(f"{lab:<26}{len(b):>7}{r.mean()*100:>+8.3f}%{tstat(r):>7.1f}"
              f"{(r>0).mean()*100:>5.0f}%{monthly_sharpe(b):>7.2f}"
              f"{b.ret_debit_mid.mean()*100:>+8.2f}%")


def main():
    print("loading feather ...", flush=True)
    d = load()
    rlz = realized_moves(d)
    print(f"realized earnings moves: {rlz.ev_rlz.notna().sum()} events, "
          f"median |move| {rlz.ev_rlz.median():.2f}%")

    for min_debit in (0.30, 0.75):
        print(f"\n{'='*74}\nMIN_DEBIT ${min_debit:.2f}")
        tr = build_trades(d, min_debit)
        tr = add_signals(tr, rlz)
        # liquidity subset comparable to our top-200 live universe
        med_v = tr.groupby("ticker").vol.median()
        top = set(med_v.sort_values(ascending=False).head(200).index)
        print(f"trades {len(tr)}  tickers {tr.ticker.nunique()}  "
              f"lag2 {(tr.lag==2).sum()}  lag14 {(tr.lag==14).sum()}")

        for uni, ulab in [(None, "ALL tickers"), (top, "top-200 by volume")]:
            u = tr if uni is None else tr[tr.ticker.isin(uni)]
            for lo, hi, plab in [(2013, 2024, "2013-2024 (our window)"),
                                 (2006, 2012, "2006-2012 (pre-sample OOS)")]:
                s = u[(u.exit.dt.year >= lo) & (u.exit.dt.year <= hi)]
                l2, l15 = s[s.lag == 2], s[s.lag == 14]
                books = {"A  lag-2 ungated": l2,
                         "N  lag-14 ungated (naive)": l15,
                         "B  lag-14 + s4-low gate": s4_gate(l15),
                         "B' lag-14 + WF OLS model": wf_model(l15, lo + 3)}
                report(f"--- {ulab} | {plab} ---", books)

        if min_debit == 0.30:
            tr.to_parquet("../cache/macropod_trades.parquet")
            print("\nwrote ../cache/macropod_trades.parquet")


if __name__ == "__main__":
    main()
