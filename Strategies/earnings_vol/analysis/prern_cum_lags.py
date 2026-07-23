"""
Cumulative gross equity curves of the pre-earnings straddle by entry lag.

Equal spot-notional per trade, pnl booked at exit, cumulative sum of
%/spot (same convention as fig 1). Trade counts per lag are ~equal (~6,300)
so the curves are directly comparable.
Writes figures/9_cum_by_lag.png.  Run from analysis/:  python3 prern_cum_lags.py
"""
import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt
import numpy as np
import pandas as pd

LAGS = [2, 4, 8, 14, 22, 30]


def main():
    ls = pd.read_parquet("../cache/trades_prern_lagscan.parquet")
    cmap = plt.get_cmap("coolwarm_r")
    fig, ax = plt.subplots(figsize=(9.5, 5.2))
    for i, lag in enumerate(LAGS):
        d = ls[ls.lag == lag].sort_values("exit")
        c = cmap(i / (len(LAGS) - 1))
        lw = 2.2 if lag == 2 else 1.3
        r = d.ret_spot_mid
        sh = (d.set_index("exit").ret_spot_mid.resample("ME").sum()
                .reindex(pd.date_range(d["exit"].min(), d["exit"].max(),
                                       freq="ME"), fill_value=0.0))
        sh = sh.mean() / sh.std() * np.sqrt(12)
        ax.plot(d["exit"], r.cumsum() * 100, color=c, lw=lw,
                label=f"lag {lag:2d}  (n={len(d)}, Sharpe {sh:.1f})")
    ax.axhline(0, color="k", lw=0.7)
    ax.set_ylabel("cumulative %/spot (sum over trades, equal notional)")
    ax.set_title("Long pre-earnings straddle: cumulative gross pnl by entry lag")
    ax.legend(fontsize=9)
    fig.tight_layout()
    fig.savefig("../figures/9_cum_by_lag.png", dpi=110)
    print("wrote ../figures/9_cum_by_lag.png")


if __name__ == "__main__":
    main()
