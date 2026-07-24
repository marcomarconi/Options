"""
Equity curves comparing each restrike band — the naked short straddle re-centered to a
fresh ATM straddle whenever position delta |2d-1| breaches band D. GROSS (mid), 2013-2024,
aligned to the SAME entries as the share-hedged book (so 'naked' = the hedged book's
unhedged row and 'share hedge' is the ceiling). Monthly Sharpe annotated per curve.

Equity = cumulative sum of the equal-weight monthly-mean return (r = pnl/margin), by entry
month — the same monthly series the Sharpe is computed on.

Run from analysis/:  python3 restrike_equity.py [etf|stock] [all|top20|top10|net]
"""
import sys
import numpy as np, pandas as pd
import restrike_books as rb
import matplotlib; matplotlib.use("Agg")
import matplotlib.pyplot as plt

INK, MUTED, GRID, BG = "#1c1c1c", "#5c5c5c", "#dcdcdc", "#fcfcfb"
GRAY, GREEN = "#8a8a8a", "#1baf7a"
BANDS = [0.9, 0.8, 0.7, 0.6, 0.5, 0.4, 0.3, 0.2, 0.15, 0.1]
CMAP = plt.get_cmap("plasma")


def monthly(g, col="r"):
    return pd.Series(g[col].values, index=g.tradeDate.values.astype("datetime64[M]")).groupby(level=0).mean()


def sharpe(m):
    return m.mean() / m.std() * np.sqrt(12) if m.std() > 0 else np.nan


def curve(ax, m, color, label, lw=1.6, ls="-", z=3):
    m = m.sort_index()
    ax.plot(m.index, (m.cumsum() * 100).values, color=color, lw=lw, ls=ls,
            label=f"{label}   Sh {sharpe(m):+.2f}   Σ {m.sum()*100:+.0f}%", zorder=z)


def main():
    kind = sys.argv[1] if len(sys.argv) > 1 else "etf"
    book = sys.argv[2] if len(sys.argv) > 2 else "all"
    rs, hd, key = rb.load(kind, book)
    hd = hd[["ticker", "tradeDate", "r_mid"]]

    plt.rcParams.update({"font.size": 9, "figure.facecolor": BG})
    fig, ax = plt.subplots(figsize=(13, 6.6))

    curve(ax, monthly(rs[~np.isfinite(rs.band)]), GRAY, "naked (no restrike)", lw=1.8, ls=":", z=2)
    for i, D in enumerate(BANDS):
        g = rs[rs.band == D]
        nr = g.n_restrike.mean()
        curve(ax, monthly(g), CMAP(i / (len(BANDS) - 1)), f"restrike D={D:.2f}  ({nr:.0f}rs/tr)",
              lw=1.7, z=4)
    curve(ax, monthly(hd, "r_mid"), GREEN, "share delta-hedge (ceiling)", lw=2.6, ls="--", z=6)

    ax.axhline(0, color=INK, lw=.9)
    ax.set_facecolor(BG); ax.grid(color=GRID, lw=.7); ax.set_axisbelow(True)
    for s in ("top", "right"):
        ax.spines[s].set_visible(False)
    ax.set_title(f"Restrike-band equity curves — {kind.upper()} short straddle, {rb.LABEL[book]}, "
                 f"gross 2013-2024 (n={len(key):,} entries, {hd.ticker.nunique()} names)", loc="left", color=INK, fontsize=11)
    ax.set_ylabel("cumulative return (sum of monthly %, equal-weight)", color=MUTED, fontsize=9)
    ax.tick_params(colors=MUTED, labelsize=8)
    ax.legend(frameon=False, fontsize=8.3, loc="upper left", ncol=2)
    fig.tight_layout()
    p = f"../out/plots/47_restrike_equity_{kind}_{book}.png"
    fig.savefig(p, dpi=150, facecolor=BG)
    print("wrote", p)


if __name__ == "__main__":
    main()
