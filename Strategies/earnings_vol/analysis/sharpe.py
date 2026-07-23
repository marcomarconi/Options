"""
Annualized Sharpe ratios for the pre-earnings straddle variants (GROSS mid).

Convention: equal spot-notional per trade, constant capital, pnl booked at
exit into a calendar-month series (months with no exits = 0), Sharpe =
mean/std * sqrt(12) of that series. This respects earnings-week clustering
(unlike the naive per-trade annualization t/sqrt(years), shown for
comparison — it assumes independent trades and reads high).
Sharpe is scale-invariant to the constant capital assumption, but NOT to
capital timing: a book that concentrates deployment in reporting season
would differ. Run from analysis/:  python3 sharpe.py
"""
import numpy as np
import pandas as pd

from gate import add_mv_rank

R = "ret_spot_mid"


def sharpe_rows(d, label, start=None):
    if start:
        d = d[d.ernDate >= start]
    yrs = (d["exit"].max() - d["exit"].min()).days / 365.25
    m = (d.set_index("exit")[R].resample("ME").sum()
          .reindex(pd.date_range(d["exit"].min(), d["exit"].max(), freq="ME"),
                   fill_value=0.0))
    sh_m = m.mean() / m.std() * np.sqrt(12)
    r = d[R]
    sh_naive = r.mean() / r.std() * np.sqrt(len(r) / yrs)
    return dict(book=label, n=len(d), yrs=round(yrs, 1),
                sharpe_monthly=round(sh_m, 2),
                sharpe_naive_pertrade=round(sh_naive, 2),
                pos_months_pct=round((m > 0).mean() * 100),
                worst_month_pct=round(m.min() * 100, 2))


def main():
    tr = pd.read_parquet("../cache/trades_prern.parquet")
    ls = pd.read_parquet("../cache/trades_prern_lagscan.parquet")
    t15 = tr[tr.lag == 15].copy()
    g = add_mv_rank(t15).dropna(subset=["mv_rk"])
    gate = g[(g.when == "AMC") & (g.mv_rk < 0.33)]

    rows = [
        sharpe_rows(t15, "lag 15 ungated"),
        sharpe_rows(t15[t15.when == "AMC"], "lag 15 AMC only"),
        sharpe_rows(gate, "lag 15 AMC + cheap 1/3 (past-only)"),
        sharpe_rows(gate, "  same, 2019+", start="2019-01-01"),
        sharpe_rows(ls[ls.lag == 2], "lag 2 ungated (short hold)"),
        sharpe_rows(ls[ls.lag == 8], "lag 8 ungated"),
    ]
    print(pd.DataFrame(rows).to_string(index=False))


if __name__ == "__main__":
    main()
