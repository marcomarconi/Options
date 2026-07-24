"""
Restriking as a poor-man's delta hedge: unhedged short straddle re-centered to a fresh
ATM straddle whenever position delta |2d-1| breaches a band D. GROSS (mid), 2013-2024,
aligned to the SAME entries as the share-hedged book so naked reproduces its unhedged row.

Left: monthly Sharpe vs band, with naked (floor) and share-hedge (ceiling) references.
Right: P&L std vs band. Restrike count annotated. Sharpe is the headline on both.

Run from analysis/:  python3 restrike_plot.py [etf|stock] [all|top20|top10|net]
"""
import sys
import numpy as np, pandas as pd
import restrike_books as rb
import matplotlib; matplotlib.use("Agg")
import matplotlib.pyplot as plt

INK, MUTED, GRID, BG = "#1c1c1c", "#5c5c5c", "#dcdcdc", "#fcfcfb"
GRAY, VIOLET, GREEN = "#8a8a8a", "#4a3aa7", "#1baf7a"
BANDS = [0.9, 0.8, 0.7, 0.6, 0.5, 0.4, 0.3, 0.2, 0.15, 0.1]


def sh(g, col="r"):
    s = pd.Series(g[col].values, index=g.tradeDate.values.astype("datetime64[M]")).groupby(level=0).mean()
    return s.mean() / s.std() * np.sqrt(12) if s.std() > 0 else np.nan


def main():
    kind = sys.argv[1] if len(sys.argv) > 1 else "etf"
    book = sys.argv[2] if len(sys.argv) > 2 else "all"
    rs, hd, key = rb.load(kind, book)
    hd = hd[["ticker", "tradeDate", "r_mid"]]

    naked = rs[~np.isfinite(rs.band)]
    sh_naked, std_naked = sh(naked), naked.r.std() * 100
    sh_hedge, std_hedge = sh(hd, "r_mid"), hd.r_mid.std() * 100

    xs, shs, stds, nrs = [], [], [], []
    for D in BANDS:
        g = rs[rs.band == D]
        xs.append(D); shs.append(sh(g)); stds.append(g.r.std() * 100); nrs.append(g.n_restrike.mean())

    plt.rcParams.update({"font.size": 9, "figure.facecolor": BG})
    fig, ax = plt.subplots(1, 2, figsize=(14, 5.4))

    ax[0].axhline(sh_hedge, color=GREEN, lw=1.6, ls="--", label=f"share delta-hedge (ceiling)  Sh {sh_hedge:+.2f}")
    ax[0].axhline(sh_naked, color=GRAY, lw=1.6, ls=":", label=f"naked (floor)  Sh {sh_naked:+.2f}")
    ax[0].plot(xs, shs, "-o", color=VIOLET, lw=2.2, label="restrike-only book")
    for x, y, n in zip(xs, shs, nrs):
        ax[0].annotate(f"{n:.0f}rs", (x, y), textcoords="offset points", xytext=(0, 7),
                       ha="center", fontsize=7.5, color=MUTED)
    ax[0].set_title("Monthly Sharpe vs restrike band (gross)", loc="left", color=INK, fontsize=10.5)
    ax[0].set_ylabel("monthly Sharpe", color=MUTED, fontsize=9)

    ax[1].axhline(std_hedge, color=GREEN, lw=1.6, ls="--", label=f"share delta-hedge  {std_hedge:.1f}%")
    ax[1].axhline(std_naked, color=GRAY, lw=1.6, ls=":", label=f"naked  {std_naked:.1f}%")
    ax[1].plot(xs, stds, "-o", color=VIOLET, lw=2.2, label="restrike-only book")
    ax[1].set_title("P&L std vs restrike band (gross)", loc="left", color=INK, fontsize=10.5)
    ax[1].set_ylabel("P&L std (% of margin)", color=MUTED, fontsize=9)

    for a in ax:
        a.set_xlabel("delta band D  (restrike when |2δ−1| > D)  ← tighter = more restrikes", color=MUTED, fontsize=9)
        a.invert_xaxis()
        a.set_facecolor(BG); a.grid(color=GRID, lw=.7); a.set_axisbelow(True)
        for s in ("top", "right"):
            a.spines[s].set_visible(False)
        a.tick_params(colors=MUTED, labelsize=8)
        a.legend(frameon=False, fontsize=8.5, loc="best")

    fig.suptitle(f"Restriking a naked short straddle ≈ poor-man's delta hedge — {kind.upper()}, {rb.LABEL[book]}, "
                 f"gross 2013-2024 (n={len(key):,} entries, {hd.ticker.nunique()} names)", fontsize=12.3, color=INK, x=0.006, ha="left")
    fig.tight_layout(rect=[0, 0, 1, 0.95])
    p = f"../out/plots/46_restrike_{kind}_{book}.png"
    fig.savefig(p, dpi=150, facecolor=BG)
    print("wrote", p)


if __name__ == "__main__":
    main()
