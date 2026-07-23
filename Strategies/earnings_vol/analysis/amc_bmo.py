"""
Fig 19: AMC vs BMO split for both live books.

Books:
  - lag-2 ungated core (trades_prern_lagscan, lag == 2)
  - lag-15 ungated (context)
  - lag-15 + s4-low gate: s4_vs_avg_rlz <= expanding PAST-ONLY median of all
    prior trades (sorted by entry; >= 200 prior trades required) — no AMC
    condition in the gate, since AMC/BMO is the split being tested.

Prints per-trade stats (mean pnl/spot, t, win rate, monthly Sharpe) by
announcement timing, then plots:
  (a) mean pnl/spot per trade, AMC vs BMO, per book (t-stats annotated);
  (b) cumulative monthly pnl/spot, lag-2, AMC vs BMO;
  (c) same for lag-15 s4-low.
Run from analysis/:  python3 amc_bmo.py
"""
import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt
import numpy as np
import pandas as pd

from oq_signals import build_event_table, add_signals

BLUE, RED, GRAY, VIOLET = "#4878cf", "#d1615d", "#888888", "#4a3aa7"
OUT = "../figures"
R = "ret_spot_mid"
IDX = pd.date_range("2013-01-31", "2024-12-31", freq="ME")


def t(r):
    return r.mean() / r.std() * np.sqrt(len(r))


def msharpe(d):
    m = (d.set_index("exit")[R].resample("ME").sum()
          .reindex(IDX, fill_value=0.0))
    return m.mean() / m.std() * np.sqrt(12)


def monthly_cum(d):
    return (d.set_index("exit")[R].resample("ME").sum()
             .reindex(IDX, fill_value=0.0).cumsum() * 100)


def s4_gate(d):
    """Past-only expanding-median gate on s4, sorted by entry date."""
    d = d.dropna(subset=["s4_vs_avg_rlz"]).sort_values("entry").copy()
    med = d.s4_vs_avg_rlz.expanding(min_periods=200).median().shift(1)
    return d[d.s4_vs_avg_rlz <= med]


def main():
    et = build_event_table()
    ls = pd.read_parquet("../cache/trades_prern_lagscan.parquet")
    tr = pd.read_parquet("../cache/trades_prern.parquet")

    l2 = ls[ls.lag == 2].copy()
    l15 = add_signals(tr[tr.lag == 15].dropna(subset=["impErnMv"]).copy(), et)
    l15_s4 = s4_gate(l15)

    books = [("lag-2 ungated", l2),
             ("lag-15 ungated", l15),
             ("lag-15 s4-low", l15_s4)]

    print("book              when   n      mean%    t      win%   moSharpe")
    stats = {}
    for lab, d in books:
        for w in ["AMC", "BMO"]:
            s = d[d.when == w]
            if len(s) == 0:
                continue
            r = s[R]
            stats[(lab, w)] = (r.mean() * 100, t(r))
            print(f"{lab:16s}  {w}  {len(s):5d}  {r.mean()*100:+.3f}  "
                  f"{t(r):+5.1f}  {(r > 0).mean()*100:4.1f}  {msharpe(s):5.2f}")
        other = d[~d.when.isin(["AMC", "BMO"])]
        if len(other):
            print(f"{lab:16s}  other n={len(other)} "
                  f"({', '.join(other.when.unique())})")

    fig, axs = plt.subplots(1, 3, figsize=(14.5, 4.6),
                            gridspec_kw={"width_ratios": [1.1, 1, 1]})

    ax = axs[0]
    x = np.arange(len(books))
    for off, w, c in [(-0.17, "AMC", BLUE), (0.17, "BMO", RED)]:
        vals = [stats[(lab, w)][0] for lab, _ in books]
        ax.bar(x + off, vals, width=0.32, color=c, label=w)
        for xi, (lab, _) in zip(x + off, books):
            m, tt = stats[(lab, w)]
            ax.annotate(f"t={tt:+.1f}", (xi, m + 0.02), ha="center",
                        fontsize=8, color="#333333")
    ax.set_xticks(x)
    ax.set_xticklabels([lab for lab, _ in books], fontsize=9)
    ax.set_ylabel("mean pnl/spot per trade, %", fontsize=9)
    ax.legend(fontsize=9, frameon=False, loc="upper left")
    ax.set_title("a) per-trade edge by announcement timing",
                 fontsize=10, loc="left")

    for ax, (lab, d), panel in [(axs[1], books[0], "b"),
                                (axs[2], books[2], "c")]:
        for w, c in [("AMC", BLUE), ("BMO", RED)]:
            s = d[d.when == w]
            m = monthly_cum(s)
            ax.plot(m.index, m.values, color=c, lw=1.8)
            ax.annotate(f"{w} (n={len(s)})", (m.index[-1], m.iloc[-1]),
                        color=c, fontsize=9, va="center", xytext=(4, 0),
                        textcoords="offset points")
        ax.set_xlim(IDX[0], IDX[-1] + pd.Timedelta(days=1400))
        ax.set_ylabel("cumulative pnl/spot, %", fontsize=9)
        ax.set_title(f"{panel}) {lab}: cumulative, equal notional",
                     fontsize=10, loc="left")

    for a in axs:
        a.spines[["top", "right"]].set_visible(False)
        a.grid(axis="y", color="#dddddd", lw=0.6)
        a.set_axisbelow(True)
    fig.suptitle("AMC vs BMO: after-close reporters carry most of the "
                 "per-trade edge; lag-15 BMO is barely alive", fontsize=12,
                 y=0.99)
    fig.tight_layout()
    fig.savefig(f"{OUT}/19_amc_bmo.png", dpi=110)
    print(f"\nwrote {OUT}/19_amc_bmo.png")


if __name__ == "__main__":
    main()
