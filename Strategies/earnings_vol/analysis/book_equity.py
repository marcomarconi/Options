"""
Equity curves for the two live pre-earnings straddle books:
  * lag2        — enter 2 days before the event, ALL confirmed events, ungated
                  (the "recommended trade"); from the lag-scan cache.
  * lag15-s4    — the lag-15 walk-forward 4-signal OLS model book (retrained
                  yearly, keep pred>0), out-of-sample 2016+; via oq_model.

Gross first (mid), then realistic (f=0.5 of the entry+exit spread) and full taker.
Additive cumulative %/spot (per-event returns; events cluster in earnings season,
so a fully-compounded curve is meaningless — sum per unit premium instead).
Monthly-resampled Sharpe annotated on every series.

Run from analysis/:  python3 book_equity.py
"""
import numpy as np, pandas as pd
import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt

from oq_model import build_model_book, load_lag15

R = "/home/marco/trading/Systems/Options/Strategies/earnings_vol"
INK, MUTED, GRID, BG = "#1c1c1c", "#5c5c5c", "#dcdcdc", "#fcfcfb"
C = {"gross": "#2a78d6", "real": "#1baf7a", "taker": "#eb6834"}


def ret_at(t, f):
    pay = t.debit_mid + f * (t.debit_ask - t.debit_mid)
    recv = t.exit_mid - f * (t.exit_mid - t.exit_bid)
    return (recv - pay) / t.spot


def monthly_sharpe(t, r, start):
    idx = pd.date_range(start, "2024-12-31", freq="ME")
    m = pd.Series(r.values, index=t.exit.values).resample("ME").sum().reindex(idx, fill_value=0.0)
    return m.mean() / m.std() * np.sqrt(12) if m.std() > 0 else np.nan


def lag2_book():
    ls = pd.read_parquet(f"{R}/cache/trades_prern_lagscan.parquet")
    d = ls[ls.lag == 2].copy()
    d["exit"] = pd.to_datetime(d.exit)
    return d.sort_values("exit")


def panel(ax, d, title, start):
    d = d.sort_values("exit")
    for key, f, lab in [("gross", 0.0, "gross (mid)"), ("real", 0.5, "realistic (f=0.5)"),
                        ("taker", 1.0, "full taker")]:
        r = ret_at(d, f)
        sh = monthly_sharpe(d, r, start)
        ax.plot(d.exit, (r.cumsum() * 100).values, color=C[key], lw=1.9,
                label=f"{lab}   Sh {sh:+.2f}   Σ {r.sum()*100:+.0f}%")
    ax.axhline(0, color=INK, lw=.9)
    ax.set_facecolor(BG); ax.grid(color=GRID, lw=.7); ax.set_axisbelow(True)
    for s in ("top", "right"):
        ax.spines[s].set_visible(False)
    ax.set_title(title, loc="left", color=INK, fontsize=10.5)
    ax.set_ylabel("cumulative return, % of spot (sum)", color=MUTED, fontsize=9)
    ax.tick_params(colors=MUTED, labelsize=8)
    ax.legend(frameon=False, fontsize=8, loc="upper left")


def main():
    plt.rcParams.update({"font.size": 9, "figure.facecolor": BG})
    fig, ax = plt.subplots(1, 2, figsize=(15.5, 5.2))

    d2 = lag2_book()
    panel(ax[0], d2, f"lag2 — enter 2d pre-event, all confirmed events  "
                     f"(n={len(d2)}, 191 names)", "2013-01-31")

    book, scored = build_model_book(load_lag15())
    book = book.copy(); book["exit"] = pd.to_datetime(book.exit)
    panel(ax[1], book, f"lag15-s4 — 4-signal walk-forward model, pred>0, OOS 2016+  "
                       f"(n={len(book)})", "2016-01-31")

    fig.suptitle("Pre-earnings straddle — live book equity curves (baseline top-200, real quotes)",
                 fontsize=12.5, color=INK, x=0.006, ha="left")
    fig.tight_layout(rect=[0, 0, 1, 0.95])
    p = f"{R}/figures/book_equity.png"
    fig.savefig(p, dpi=150, facecolor=BG)
    print("wrote", p)

    for lab, d, start in [("lag2", d2, "2013-01-31"), ("lag15-s4", book, "2016-01-31")]:
        print(f"\n{lab}: n={len(d)}")
        for f, fl in [(0.0, "gross"), (0.5, "realistic"), (1.0, "taker")]:
            r = ret_at(d, f)
            print(f"  {fl:10} mean {r.mean()*100:+.3f}%/spot  Σ {r.sum()*100:+.0f}%  "
                  f"monthlySharpe {monthly_sharpe(d, r, start):+.2f}  win {(r>0).mean()*100:.0f}%")


if __name__ == "__main__":
    main()
