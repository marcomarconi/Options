"""
Fig 17: sizing & risk (README "Sizing & risk management" section, 2026-07-17).

(a) equal-premium is the correct weight: per-trade pnl std by debit/spot
    quintile, each rule normalized to its own Q3 — pnl/spot ramps ~5x,
    pnl/debit is flat;
(b) realized annual $ std vs --unit with INTEGER contracts (lag-2 book),
    $4k target and the solved $825 unit marked;
(c) yearly $ P&L at --unit 800 — no losing year in 12;
(d) sigma_r ex-ante Monte-Carlo (sigma_r_mc.py) vs the realized backtest
    pnl/debit distribution.
Run from analysis/:  python3 sizing_plots.py
"""
import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt
import numpy as np
import pandas as pd

from sigma_r_mc import sim, NU, VOL_J
from unit_for_risk import load, ann_std, realized

BLUE, RED, GRAY, VIOLET = "#4878cf", "#d1615d", "#888888", "#4a3aa7"
OUT = "../figures"


def main():
    b = load()
    b["relvol"] = b.debit_mid / b.spot
    b["q"] = pd.qcut(b.relvol, 5, labels=False)

    fig, axs = plt.subplots(2, 2, figsize=(12.5, 8.6))
    ax = axs[0, 0]
    g = b.groupby("q")
    for col, c, lab in [("ret_spot_mid", BLUE, "pnl/spot (equal notional)"),
                        ("ret_debit_mid", RED, "pnl/debit (equal premium)")]:
        s = g[col].std()
        s = s / s.loc[2]
        ax.plot(s.index, s.values, "-o", color=c, lw=2, ms=6, label=lab)
        ax.annotate(lab, (4.05, s.iloc[-1]), color=c, fontsize=9, va="center")
    ax.set_xticks(range(5))
    ax.set_xticklabels([f"Q{i+1}\n{v*100:.0f}%" for i, v in
                        enumerate(g.relvol.mean())], fontsize=8)
    ax.set_xlim(-0.3, 5.6)
    ax.set_xlabel("debit/spot quintile (mean shown)", fontsize=9)
    ax.set_ylabel("per-trade pnl std, rel. to own Q3", fontsize=9)
    ax.set_title("a) equal PREMIUM equalizes per-trade risk (lag-2)",
                 fontsize=10, loc="left")

    ax = axs[0, 1]
    units = np.arange(200, 2001, 100)
    ax.plot(units, [ann_std(b, u) for u in units], "-", color=BLUE, lw=2)
    ax.axhline(4000, color=GRAY, ls="--", lw=1)
    ax.annotate("\\$4k target (20% of \\$20k)", (210, 4150), color=GRAY,
                fontsize=9)
    ax.plot([825], [4038], "o", color=RED, ms=8)
    ax.annotate("--unit 825", (900, 3850), color=RED, fontsize=9, va="top")
    ax.set_xlabel("--unit (premium $ per trade)", fontsize=9)
    ax.set_ylabel("realized annual $ std (integer contracts)", fontsize=9)
    ax.set_title("b) risk vs unit size, lag-2 book 2013-2024",
                 fontsize=10, loc="left")

    ax = axs[1, 0]
    u = 800
    n = (u // (100 * b.debit_mid)).astype(int)
    pnl = n * 100 * b.debit_mid * b.ret_debit_mid
    y = (pnl.groupby(pd.to_datetime(b.exit)).sum()
            .resample("Y").sum())
    ax.bar(y.index.year, y.values, color=BLUE, width=0.7)
    ax.axhline(0, color="#444444", lw=0.8)
    ax.set_ylim(0, 18200)
    ax.annotate("2021 = May-Dec only (data gap)", xy=(2021, 4700),
                xytext=(2020.4, 17000), fontsize=8, color="#555555",
                ha="center",
                arrowprops=dict(arrowstyle="-", color=GRAY, lw=0.8))
    ax.set_ylabel("$ P&L / year at --unit 800", fontsize=9)
    ax.set_title("c) yearly P&L at the $800 unit — no losing year",
                 fontsize=10, loc="left")

    ax = axs[1, 1]
    mc = sim(NU, VOL_J)
    bt = b.ret_debit_mid
    bins = np.linspace(-0.8, 1.2, 81)
    ax.hist(bt, bins=bins, density=True, color=BLUE,
            alpha=0.45, label=f"backtest (std {bt.std():.3f})")
    ax.hist(mc, bins=bins, density=True, color=RED,
            histtype="step", lw=2, label=f"forward MC (std {mc.std():.3f})")
    ax.legend(fontsize=9, frameon=False)
    ax.set_xlabel("per-trade pnl / debit", fontsize=9)
    ax.set_ylabel("density", fontsize=9)
    ax.set_title("d) sigma_r: no-backtest Monte-Carlo vs realized (lag-2)",
                 fontsize=10, loc="left")

    for a in axs.flat:
        a.spines[["top", "right"]].set_visible(False)
        a.grid(axis="y", color="#dddddd", lw=0.6)
        a.set_axisbelow(True)
    fig.suptitle("Sizing & risk: equal-premium weighting, unit solve, "
                 "ex-ante sigma_r", fontsize=12, y=0.995)
    fig.tight_layout()
    fig.savefig(f"{OUT}/17_sizing.png", dpi=110)
    print(f"wrote {OUT}/17_sizing.png")


if __name__ == "__main__":
    main()
