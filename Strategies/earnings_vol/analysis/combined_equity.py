"""
Fig 13: cumulative gross equity of the two recommended books and their sum:
  - lag-2 ungated (core)
  - lag-15 + video-faithful 4-signal walk-forward model (oq_model.py,
    pred > 0, out-of-sample 2016+)
  - both combined (equal notional per trade)
Monthly-series Sharpe (empty months = 0) in the legend; common 2016+ window
so the model book's shorter OOS history doesn't skew the comparison.
Run from analysis/:  python3 combined_equity.py
"""
import os

import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt
import numpy as np
import pandas as pd

from oq_model import build_model_book, FIRST_TEST_YEAR

R = "ret_spot_mid"
OUT = "../figures"
BLUE, VIOLET, INK = "#4878cf", "#4a3aa7", "#333333"


def main():
    os.makedirs(OUT, exist_ok=True)
    ls = pd.read_parquet("../cache/trades_prern_lagscan.parquet")
    lag2 = ls[(ls.lag == 2)
              & (ls.exit.dt.year >= FIRST_TEST_YEAR)][["exit", R]]

    model, _ = build_model_book()
    gate = model[["exit", R]]

    both = pd.concat([lag2, gate])
    idx = pd.date_range(f"{FIRST_TEST_YEAR}-01-31", "2024-12-31", freq="ME")

    def sharpe(d):
        m = (d.set_index("exit")[R].resample("ME").sum()
              .reindex(idx, fill_value=0.0))
        return m.mean() / m.std() * np.sqrt(12)

    fig, ax = plt.subplots(figsize=(9.5, 5.2))
    for d, lab, c, lw in [
            (lag2, "lag 2 ungated", BLUE, 1.4),
            (gate, "lag 15 + 4-signal model (OOS)", VIOLET, 1.4),
            (both, "combined (both books)", INK, 2.0)]:
        s = d.sort_values("exit")
        ax.plot(s.exit, s[R].cumsum() * 100,
                label=f"{lab} (n={len(s)}, Sharpe {sharpe(d):.2f})",
                color=c, lw=lw)
    ax.axhline(0, color="k", lw=0.7)
    ax.set_ylabel("cumulative %/spot (sum over trades, gross mid)")
    ax.set_title(f"core + sleeve, common {FIRST_TEST_YEAR}+ window: lag-2 "
                 "ungated, 4-signal lag-15, both combined")
    ax.legend(fontsize=9, loc="upper left")
    fig.tight_layout()
    fig.savefig(f"{OUT}/13_combined_equity.png", dpi=110)
    print(f"wrote 13_combined_equity.png to {OUT}/")


if __name__ == "__main__":
    main()
