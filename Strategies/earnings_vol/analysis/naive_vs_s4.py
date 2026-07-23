"""
lag15 naive (ungated) vs lag15 + s4 (4-signal walk-forward gate) — what the gate buys.
Left: GROSS cumulative %/spot. Right: NET (realistic f=0.5 fill). Monthly Sharpe per book.
The point: gross barely changes, but the gate is the difference between a net LOSER and a
net winner. Recommended book = s4 AND AMC-only shown too.

Run from analysis/:  python3 naive_vs_s4.py
"""
import numpy as np, pandas as pd
import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt

from oq_model import build_model_book, load_lag15, FIRST_TEST_YEAR

R = "ret_spot_mid"
INK, MUTED, GRID, BG = "#1c1c1c", "#5c5c5c", "#dcdcdc", "#fcfcfb"
GRAY, VIOLET, GREEN = "#888888", "#4a3aa7", "#1baf7a"
START = f"{FIRST_TEST_YEAR}-01-31"


def ret_at(t, f):
    return ((t.exit_mid - f * (t.exit_mid - t.exit_bid))
            - (t.debit_mid + f * (t.debit_ask - t.debit_mid))) / t.spot


def mosh(exit_s, r):
    idx = pd.date_range(START, "2024-12-31", freq="ME")
    m = pd.Series(r.values, index=exit_s.values).resample("ME").sum().reindex(idx, fill_value=0.0)
    return m.mean() / m.std() * np.sqrt(12) if m.std() > 0 else np.nan


def curve(ax, s, f, color, label, lw=1.9):
    s = s.sort_values("exit")
    r = ret_at(s, f) if f is not None else s[R]
    sh = mosh(s.exit, r)
    ax.plot(s.exit, (r.cumsum() * 100).values, color=color, lw=lw,
            label=f"{label}   Sh {sh:+.2f}   Σ {r.sum()*100:+.0f}%")


def style(ax, title):
    ax.axhline(0, color=INK, lw=.9)
    ax.set_facecolor(BG); ax.grid(color=GRID, lw=.7); ax.set_axisbelow(True)
    for s in ("top", "right"):
        ax.spines[s].set_visible(False)
    ax.set_title(title, loc="left", color=INK, fontsize=10.5)
    ax.set_ylabel("cumulative % of spot (sum)", color=MUTED, fontsize=9)
    ax.tick_params(colors=MUTED, labelsize=8)
    ax.legend(frameon=False, fontsize=8.5, loc="upper left")


def main():
    d = load_lag15()
    book, _ = build_model_book(d)
    naive = d[d.year >= FIRST_TEST_YEAR]
    amc = book[book.when == "AMC"]

    plt.rcParams.update({"font.size": 9, "figure.facecolor": BG})
    fig, ax = plt.subplots(1, 2, figsize=(15.5, 5.4))

    curve(ax[0], naive, None, GRAY, f"lag15 naive (ungated, n={len(naive)})")
    curve(ax[0], book, None, VIOLET, f"lag15 + s4 (n={len(book)})", lw=2.3)
    curve(ax[0], amc, None, GREEN, f"lag15 + s4, AMC-only (n={len(amc)})")
    style(ax[0], "GROSS (mid) — the gate keeps ~all the profit on 75% of the trades")

    curve(ax[1], naive, 0.5, GRAY, "lag15 naive")
    curve(ax[1], book, 0.5, VIOLET, "lag15 + s4", lw=2.3)
    curve(ax[1], amc, 0.5, GREEN, "lag15 + s4, AMC-only")
    style(ax[1], "NET (realistic f=0.5) — the gate flips a net LOSER into a winner")

    fig.suptitle("lag15 pre-earnings straddle: naive vs 4-signal gate (s4), out-of-sample 2016+",
                 fontsize=12.5, color=INK, x=0.006, ha="left")
    fig.tight_layout(rect=[0, 0, 1, 0.95])
    p = "../figures/naive_vs_s4.png"
    fig.savefig(p, dpi=150, facecolor=BG)
    print("wrote", p)


if __name__ == "__main__":
    main()
