"""
Performance by entry stock price for the two candidate books:
  A. lag-2 ungated (the recommended trade)
  B. lag-15 + video-faithful 4-signal walk-forward model (oq_model.py,
     pred > 0, out-of-sample 2016+)

Fixed dollar bands on entry spot. GROSS mid only.
Writes fig 12.  Run from analysis/:  python3 price_split.py
"""
import os

import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt
import numpy as np
import pandas as pd

from oq_model import build_model_book

R = "ret_spot_mid"
OUT = "../figures"
BLUE, RED, VIOLET = "#4878cf", "#d1615d", "#4a3aa7"

BANDS = [5, 20, 40, 80, 150, 300, np.inf]
LABS = ["$5-20", "$20-40", "$40-80", "$80-150", "$150-300", ">$300"]


def tstat(r):
    return r.mean() / r.std() * np.sqrt(len(r))


def by_price(d):
    d = d.copy()
    d["pb"] = pd.cut(d.spot, BANDS, labels=False, include_lowest=True)
    rows = []
    for b in range(len(LABS)):
        r = d.loc[d.pb == b, R]
        rows.append((LABS[b], len(r), r.mean() * 100, tstat(r),
                     (r > 0).mean() * 100))
    return rows


def main():
    os.makedirs(OUT, exist_ok=True)
    ls = pd.read_parquet("../cache/trades_prern_lagscan.parquet")
    lag2 = ls[ls.lag == 2]

    model, _ = build_model_book()

    fig, axes = plt.subplots(1, 2, figsize=(12.5, 4.6), sharex=True)
    for ax, (d, title, c) in zip(axes, [
            (lag2, f"lag 2 ungated (n={len(lag2)})", BLUE),
            (model, f"lag 15 + 4-signal model, OOS 2016+ (n={len(model)})",
             VIOLET)]):
        rows = by_price(d)
        print(f"\n== {title} ==")
        means = []
        for lab, n, m, t, win in rows:
            print(f"{lab:9s} n={n:5d}  mean {m:+.3f}%/spot  t={t:+.2f}  "
                  f"win {win:.0f}%")
            means.append(m)
        ax.bar(np.arange(len(LABS)), means,
               color=[c if m >= 0 else RED for m in means])
        for xi, (lab, n, m, t, win) in enumerate(rows):
            ax.text(xi, m, f"t={t:.1f}\nn={n}", ha="center",
                    va="bottom" if m >= 0 else "top", fontsize=7.5,
                    color="#52514e")
        ax.set_xticks(np.arange(len(LABS)), LABS, fontsize=8.5)
        ax.axhline(0, color="k", lw=0.7)
        ax.set_ylabel("mean %/spot (gross mid)")
        ax.set_title(title, fontsize=10.5)
    fig.suptitle("gross return by entry stock price", fontsize=12)
    fig.tight_layout(rect=[0, 0, 1, 0.94])
    fig.savefig(f"{OUT}/12_by_price.png", dpi=110)
    print(f"\nwrote 12_by_price.png to {OUT}/")


if __name__ == "__main__":
    main()
