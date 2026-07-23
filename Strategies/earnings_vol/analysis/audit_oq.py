"""
Audit of the Oakquants-signal work (2026-07-12): event table, signal
construction, walk-forward model, and cross-report number consistency.

 A. event table: ernDate alignment vs panel trading days; known-event
    spot checks; merge integrity (no row multiplication).
 B. signals really past-only: manual recomputation for sample tickers.
 C. walk-forward model:
    - refit-boundary nuance: drop test trades ENTERED before Jan 1 of
      their test year (model fitted on data not yet complete at entry)
    - feature-robustness: rank-transformed features instead of winsorized
      levels
    - baseline: past-only per-year s4 top-75% keep rule (does the
      regression beat the single signal it is supposedly built on?)
    - yearly coefficient signs/stability
 D. Sharpe convention reconciliation (s4 gate: 1.36 vs 1.30 discrepancy).
 E. price-split window consistency: lag-2 price pattern on 2016+ only.

Run from analysis/:  python3 audit_oq.py
"""
import numpy as np
import pandas as pd

from gate import add_mv_rank
from oq_signals import build_event_table, add_signals, SIGS
from oq_model import load_lag15, build_model_book, FIRST_TEST_YEAR

R = "ret_spot_mid"


def t(r):
    r = np.asarray(r, float)
    return r.mean() / r.std() * np.sqrt(len(r))


def msharpe(d, start):
    idx = pd.date_range(start, "2024-12-31", freq="ME")
    m = (d.set_index("exit")[R].resample("ME").sum()
          .reindex(idx, fill_value=0.0))
    return m.mean() / m.std() * np.sqrt(12)


def main():
    et = build_event_table()

    # ---- A. event table ---------------------------------------------------
    print("== A. event table ==")
    dup = et.duplicated(["ticker", "ernDate"]).sum()
    print(f"duplicate (ticker, ernDate) rows: {dup}")
    panel = pd.read_parquet("../cache/ern_panel.parquet",
                            columns=["ticker", "tradeDate"])
    ev = pd.read_parquet("../cache/events.parquet")
    on_day = ev.merge(panel.rename(columns={"tradeDate": "ernDate"}),
                      on=["ticker", "ernDate"], how="left", indicator=True)
    miss = (on_day._merge == "left_only").mean()
    print(f"events whose ernDate is NOT a panel trading day: {miss*100:.2f}%")

    tr = pd.read_parquet("../cache/trades_prern.parquet")
    t15 = tr[tr.lag == 15]
    m = t15.merge(et, on=["ticker", "ernDate"], how="left")
    print(f"merge row preservation: {len(t15)} trades -> {len(m)} rows "
          f"({'OK' if len(m) == len(t15) else 'ROW MULTIPLICATION!'})")
    print(f"signal coverage on lag-15 trades: "
          f"{m.avg_rlz.notna().mean()*100:.0f}% avg_rlz, "
          f"{m.last_rlz.notna().mean()*100:.0f}% last_rlz")

    # ---- B. signals past-only: manual recomputation -----------------------
    print("\n== B. past-only property (manual recomputation) ==")
    bad = 0
    for tkr in ["AAPL", "NVDA", "WMT"]:
        s = et[et.ticker == tkr].sort_values("ernDate").reset_index(drop=True)
        for i in range(len(s)):
            exp_avg = s.ev_rlz.iloc[:i].mean() if i >= 4 else np.nan
            got = s.avg_rlz.iloc[i]
            if not (np.isnan(exp_avg) and np.isnan(got)) and \
               not np.isclose(exp_avg, got, equal_nan=True):
                bad += 1
        # spot: last_rlz[i] == ev_rlz[i-1]
        lr = s.ev_rlz.shift(1)
        assert lr.equals(s.last_rlz), f"{tkr} last_rlz mismatch"
    print(f"avg_rlz mismatches across AAPL/NVDA/WMT: {bad} "
          f"({'OK' if bad == 0 else 'FAIL'}); last_rlz == prior ev_rlz: OK")

    # ---- C. walk-forward model --------------------------------------------
    print("\n== C. walk-forward model ==")
    d = load_lag15()
    book, scored = build_model_book(d)
    base = d[d.year >= FIRST_TEST_YEAR]
    start = f"{FIRST_TEST_YEAR}-01-31"
    print(f"headline: model {book[R].mean()*100:+.3f}% t={t(book[R]):+.2f} "
          f"n={len(book)}  vs base {base[R].mean()*100:+.3f}%")

    strict = book[book.entry.dt.year == book.exit.dt.year]
    print(f"refit-boundary strict (entry & exit same year): "
          f"n={len(strict)}  {strict[R].mean()*100:+.3f}% t={t(strict[R]):+.2f}")

    # rank-feature variant
    scored_r = []
    for y in range(FIRST_TEST_YEAR, 2025):
        tr_s, te_s = d[d.year < y], d[d.year == y].copy()
        if len(te_s) == 0:
            continue
        Xtr = np.column_stack([tr_s[c].rank(pct=True) for c in SIGS])
        # map test values onto train ECDF
        Xte = np.column_stack([
            np.searchsorted(np.sort(tr_s[c].values), te_s[c].values)
            / len(tr_s) for c in SIGS])
        A = np.c_[np.ones(len(Xtr)), Xtr]
        beta, *_ = np.linalg.lstsq(A, tr_s[R].values, rcond=None)
        te_s["pred"] = np.c_[np.ones(len(Xte)), Xte] @ beta
        scored_r.append(te_s)
    scored_r = pd.concat(scored_r)
    br = scored_r[scored_r.pred > 0]
    print(f"rank-feature variant:  n={len(br)}  {br[R].mean()*100:+.3f}% "
          f"t={t(br[R]):+.2f}  Sharpe {msharpe(br, start):.2f}")

    # baseline: keep per-year s4 <= train 75th pctile (single signal, no fit)
    keep = []
    for y in range(FIRST_TEST_YEAR, 2025):
        tr_s, te_s = d[d.year < y], d[d.year == y]
        cut = tr_s.s4_vs_avg_rlz.quantile(0.75)
        keep.append(te_s[te_s.s4_vs_avg_rlz <= cut])
    keep = pd.concat(keep)
    print(f"s4-only top-75% keep:  n={len(keep)}  {keep[R].mean()*100:+.3f}% "
          f"t={t(keep[R]):+.2f}  Sharpe {msharpe(keep, start):.2f}")
    print(f"model book Sharpe {msharpe(book, start):.2f}; "
          f"overlap model vs s4-rule: "
          f"{len(pd.merge(book[['ticker','ernDate']], keep[['ticker','ernDate']]))/len(book)*100:.0f}%")

    # yearly betas
    print("yearly betas (intercept, s1, s2, s3, s4):")
    for y in range(FIRST_TEST_YEAR, 2025):
        tr_s = d[d.year < y]
        lo, hi = tr_s[SIGS].quantile(0.01), tr_s[SIGS].quantile(0.99)
        X = tr_s[SIGS].clip(lo, hi, axis=1).values
        beta, *_ = np.linalg.lstsq(np.c_[np.ones(len(X)), X],
                                   tr_s[R].values, rcond=None)
        print(f"  {y}: " + " ".join(f"{b:+.4f}" for b in beta))

    # ---- D. Sharpe conventions --------------------------------------------
    print("\n== D. Sharpe convention (s4 gate 1.36 vs 1.30) ==")
    u = add_signals(add_mv_rank(tr[tr.lag == 15]), et).dropna(subset=["mv_rk"])
    med = u.s4_vs_avg_rlz.median()
    g = u[(u.when == "AMC") & (u.s4_vs_avg_rlz <= med)]
    for lab, start_ in [("from first trade month", None),
                        ("full 2013-01..2024-12", "2013-01-31")]:
        if start_ is None:
            mm = g.set_index("exit")[R].resample("ME").sum()
            idx = pd.date_range(mm.index.min(), "2024-12-31", freq="ME")
            mm = mm.reindex(idx, fill_value=0.0)
        else:
            mm = (g.set_index("exit")[R].resample("ME").sum()
                   .reindex(pd.date_range(start_, "2024-12-31", freq="ME"),
                            fill_value=0.0))
        print(f"  {lab:24s} Sharpe {mm.mean()/mm.std()*np.sqrt(12):.2f}")

    # ---- E. price-split window consistency ---------------------------------
    print("\n== E. lag-2 price split, 2016+ only ==")
    ls = pd.read_parquet("../cache/trades_prern_lagscan.parquet")
    l2 = ls[(ls.lag == 2) & (ls.exit.dt.year >= FIRST_TEST_YEAR)].copy()
    bands = [5, 20, 40, 80, 150, 300, np.inf]
    labs = ["$5-20", "$20-40", "$40-80", "$80-150", "$150-300", ">$300"]
    l2["pb"] = pd.cut(l2.spot, bands, labels=False, include_lowest=True)
    for b, lab in enumerate(labs):
        r = l2.loc[l2.pb == b, R]
        print(f"  {lab:9s} n={len(r):5d}  {r.mean()*100:+.3f}%  t={t(r):+.2f}")


if __name__ == "__main__":
    main()
