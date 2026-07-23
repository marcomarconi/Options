"""
lag2 and lag15-s4 book equity curves, SPLIT BY ticker option volume.

Same two books as book_equity.py, but avgOptVolu20d (merged from ern_panel_wide
at each trade's entry date) buckets the trades. Top row = GROSS (mid), bottom
row = realistic net (f=0.5). Monthly Sharpe per bucket in each legend.

The top-200 universe is liquid, so the low-volume buckets are sparse; only
buckets with >=120 trades are drawn. The question: within tradeable names, does
more volume (tighter spread) help, or is the edge arbitraged out at the top —
as the wide study's liquidity-U suggested?

Run from analysis/:  python3 book_equity_byvol.py
"""
import numpy as np, pandas as pd
import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt

from oq_model import build_model_book, load_lag15

R = "/home/marco/trading/Systems/Options/Strategies/earnings_vol"
INK, MUTED, GRID, BG = "#1c1c1c", "#5c5c5c", "#dcdcdc", "#fcfcfb"
EDGES = [0, 1e4, 5e4, 2.5e5, 1e12]
BLAB = ["<10k", "10-50k", "50-250k", "250k+"]
RAMP = ["#d98a3d", "#2a68b4", "#1f8a5c", "#154a8a"]
MINN = 120


def ret_at(t, f):
    pay = t.debit_mid + f * (t.debit_ask - t.debit_mid)
    recv = t.exit_mid - f * (t.exit_mid - t.exit_bid)
    return (recv - pay) / t.spot


def monthly_sharpe(exit_s, r, start):
    idx = pd.date_range(start, "2024-12-31", freq="ME")
    m = pd.Series(r.values, index=exit_s.values).resample("ME").sum().reindex(idx, fill_value=0.0)
    return m.mean() / m.std() * np.sqrt(12) if m.std() > 0 else np.nan


def add_vol(d):
    panel = pd.read_parquet(f"{R}/cache/ern_panel_wide.parquet",
                            columns=["ticker", "tradeDate", "avgOptVolu20d"])
    d = d.merge(panel.rename(columns={"tradeDate": "entry"}), on=["ticker", "entry"], how="left")
    d["vb"] = pd.cut(d.avgOptVolu20d, EDGES, labels=BLAB)
    return d


def panel(ax, d, f, title, start):
    d = d.sort_values("exit")
    for bl, col in zip(BLAB, RAMP):
        x = d[d.vb == bl]
        if len(x) < MINN:
            continue
        r = ret_at(x, f)
        sh = monthly_sharpe(x.exit, r, start)
        ax.plot(x.exit, (r.cumsum() * 100).values, color=col, lw=1.9,
                label=f"{bl}  (n={len(x)})  Sh {sh:+.2f}")
    ax.axhline(0, color=INK, lw=.9)
    ax.set_facecolor(BG); ax.grid(color=GRID, lw=.7); ax.set_axisbelow(True)
    for s in ("top", "right"):
        ax.spines[s].set_visible(False)
    ax.set_title(title, loc="left", color=INK, fontsize=10.3)
    ax.set_ylabel("cumulative % of spot (sum)", color=MUTED, fontsize=8.5)
    ax.tick_params(colors=MUTED, labelsize=8)
    ax.legend(frameon=False, fontsize=8, loc="upper left", title="avgOptVolu20d")


def main():
    d2 = add_vol(pd.read_parquet(f"{R}/cache/trades_prern_lagscan.parquet")
                 .query("lag == 2").assign(exit=lambda x: pd.to_datetime(x.exit)))
    book = build_model_book(load_lag15())[0].copy()
    book["exit"] = pd.to_datetime(book.exit)
    book = add_vol(book)

    plt.rcParams.update({"font.size": 9, "figure.facecolor": BG})
    fig, ax = plt.subplots(2, 2, figsize=(15.5, 9.4))
    panel(ax[0, 0], d2, 0.0, "lag2 — GROSS (mid), by volume", "2013-01-31")
    panel(ax[0, 1], book, 0.0, "lag15-s4 — GROSS (mid), by volume", "2016-01-31")
    panel(ax[1, 0], d2, 0.5, "lag2 — realistic net (f=0.5), by volume", "2013-01-31")
    panel(ax[1, 1], book, 0.5, "lag15-s4 — realistic net (f=0.5), by volume", "2016-01-31")

    fig.suptitle("Pre-earnings straddle books split by ticker option volume (baseline, real quotes)",
                 fontsize=12.5, color=INK, x=0.006, ha="left")
    fig.tight_layout(rect=[0, 0, 1, 0.96])
    p = f"{R}/figures/book_equity_byvol.png"
    fig.savefig(p, dpi=150, facecolor=BG)
    print("wrote", p)

    for lab, d, start in [("lag2", d2, "2013-01-31"), ("lag15-s4", book, "2016-01-31")]:
        print(f"\n{lab}: n={len(d)}  (matched vol {d.avgOptVolu20d.notna().mean()*100:.0f}%)")
        print(f"{'bucket':9}{'n':>6}{'gross%':>9}{'grSh':>7}{'real%':>9}{'realSh':>8}{'takerSh':>9}")
        for bl in BLAB:
            x = d[d.vb == bl]
            if len(x) < MINN:
                continue
            g, rl, tk = ret_at(x, 0), ret_at(x, 0.5), ret_at(x, 1.0)
            print(f"{bl:9}{len(x):>6}{g.mean()*100:>9.3f}{monthly_sharpe(x.exit,g,start):>7.2f}"
                  f"{rl.mean()*100:>9.3f}{monthly_sharpe(x.exit,rl,start):>8.2f}"
                  f"{monthly_sharpe(x.exit,tk,start):>9.2f}")


if __name__ == "__main__":
    main()
