"""
Solve for the screener --unit that hits a target annual $ risk on the LAG-2
book, using the backtest trades with INTEGER contracts (the rounding matters:
realized ann-std per unit-dollar drifts from ~4.2 at $500 to ~5.6 at $1500,
so a fixed ratio is only locally right).

Ann-std convention: daily book P&L landed on exit date, calendar-filled,
monthly sums * sqrt(12) — same as the README Sizing section.

Usage from analysis/:
  python3 unit_for_risk.py --risk 4000            # target ann $ std directly
  python3 unit_for_risk.py --capital 20000 --pct 20
"""
import argparse
import os

import numpy as np
import pandas as pd

CACHE = os.path.join(os.path.dirname(os.path.abspath(__file__)),
                     "..", "cache")


MIN_DEBIT = 0.75   # keep in sync with screener.py — per-contract-fee floor


def load(min_debit=MIN_DEBIT):
    b = pd.read_parquet(f"{CACHE}/trades_prern_lagscan.parquet")
    return b[(b.lag == 2) & (b.debit_mid >= min_debit)].copy()


def ann_std(b, u):
    n = (u // (100 * b.debit_mid)).astype(int)
    pnl = n * 100 * b.debit_mid * b.ret_debit_mid
    d = pnl.groupby(pd.to_datetime(b.exit)).sum()
    f = d.reindex(pd.date_range(d.index.min(), d.index.max(), freq="D"),
                  fill_value=0.0)
    return f.resample("ME").sum().std() * np.sqrt(12)


def realized(b, u):
    n = (u // (100 * b.debit_mid)).astype(int)
    pnl = n * 100 * b.debit_mid * b.ret_debit_mid
    d = pnl.groupby(pd.to_datetime(b.exit)).sum()
    f = d.reindex(pd.date_range(d.index.min(), d.index.max(), freq="D"),
                  fill_value=0.0)
    y = f.resample("Y").sum()
    c = d.sort_index().cumsum()
    days = pd.DatetimeIndex(sorted(set(b.entry) | set(b.exit)))
    dep = pd.Series(0.0, index=days)
    prem = n * 100 * b.debit_mid
    for e, x, p in zip(pd.to_datetime(b.entry), pd.to_datetime(b.exit), prem):
        dep[(days >= e) & (days < x)] += p
    return dict(ann=f.resample("ME").sum().std() * np.sqrt(12),
                mean=y.mean(), worst=y.min(),
                dd=(c - c.cummax()).min(), tradeable=(n >= 1).mean(),
                dep90=dep.quantile(0.9), peak=dep.max())


def solve(risk, b=None):
    """Bisection on integer-contract realized ann-std; unit rounded to $25."""
    if b is None:
        b = load()
    lo, hi = 100.0, 20000.0
    for _ in range(18):
        mid = (lo + hi) / 2
        if ann_std(b, mid) < risk:
            lo = mid
        else:
            hi = mid
    return round((lo + hi) / 2 / 25) * 25


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--risk", type=float, help="target annual $ std")
    ap.add_argument("--capital", type=float)
    ap.add_argument("--pct", type=float, help="annual std target, %% of capital")
    a = ap.parse_args()
    risk = a.risk if a.risk else a.capital * a.pct / 100
    b = load()
    u = solve(risk, b)
    s = realized(b, u)

    print(f"target ann std ${risk:,.0f}  ->  --unit {u:.0f}")
    print(f"backtest at that unit (2013-2024, gross at mid):")
    print(f"  ann std ${s['ann']:,.0f}   mean/yr ${s['mean']:+,.0f}   "
          f"worst yr ${s['worst']:+,.0f}   maxDD ${s['dd']:,.0f}")
    print(f"  tradeable signals {s['tradeable']*100:.0f}%   premium deployed "
          f"p90 ${s['dep90']:,.0f} / peak ${s['peak']:,.0f}")
    if a.capital and s["peak"] > a.capital:
        print(f"  !! peak week deployment exceeds capital ${a.capital:,.0f} "
              f"— the cash gate will bind; expect to skip late signals")


if __name__ == "__main__":
    main()
