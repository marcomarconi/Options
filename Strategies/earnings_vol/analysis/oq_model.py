"""
Video-faithful Oakquants model on the lag-15 book: walk-forward OLS of
%/spot on ALL FOUR relative-value signals, retrained yearly on an expanding
window, trade when predicted return > 0. All sessions (the video does not
gate on AMC). Signals past-only as in oq_signals.py; features winsorized at
the training set's 1/99 pctiles.

Exposes build_model_book() for the plot scripts; main prints stats and
writes fig 11 (equity + prediction-decile calibration).
Run from analysis/:  python3 oq_model.py
"""
import os

import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt
import numpy as np
import pandas as pd

from gate import add_mv_rank
from oq_signals import build_event_table, add_signals, SIGS

R = "ret_spot_mid"
OUT = "../figures"
BLUE, RED, GRAY, VIOLET = "#4878cf", "#d1615d", "#888888", "#4a3aa7"
FIRST_TEST_YEAR = 2016  # >=3 train years


def tstat(r):
    return r.mean() / r.std() * np.sqrt(len(r))


def monthly_sharpe(d, start="2013-01-31"):
    idx = pd.date_range(start, "2024-12-31", freq="ME")
    m = (d.set_index("exit")[R].resample("ME").sum()
          .reindex(idx, fill_value=0.0))
    return m.mean() / m.std() * np.sqrt(12)


def load_lag15():
    et = build_event_table()
    tr = pd.read_parquet("../cache/trades_prern.parquet")
    d = add_signals(add_mv_rank(tr[tr.lag == 15]), et)
    d = d.replace([np.inf, -np.inf], np.nan).dropna(subset=SIGS)
    d["year"] = d.exit.dt.year
    return d


def build_model_book(d=None):
    """Walk-forward: for each test year, fit OLS on all earlier exits,
    keep test-year trades with predicted return > 0. Returns (book, all
    scored trades with a `pred` column)."""
    if d is None:
        d = load_lag15()
    scored = []
    for y in range(FIRST_TEST_YEAR, 2025):
        tr_set = d[d.year < y]
        te_set = d[d.year == y].copy()
        if len(te_set) == 0:
            continue
        lo = tr_set[SIGS].quantile(0.01)
        hi = tr_set[SIGS].quantile(0.99)
        Xtr = tr_set[SIGS].clip(lo, hi, axis=1).values
        Xte = te_set[SIGS].clip(lo, hi, axis=1).values
        A = np.c_[np.ones(len(Xtr)), Xtr]
        beta, *_ = np.linalg.lstsq(A, tr_set[R].values, rcond=None)
        te_set["pred"] = np.c_[np.ones(len(Xte)), Xte] @ beta
        scored.append(te_set)
    scored = pd.concat(scored)
    return scored[scored.pred > 0], scored


def main():
    os.makedirs(OUT, exist_ok=True)
    d = load_lag15()
    book, scored = build_model_book(d)
    base = d[d.year >= FIRST_TEST_YEAR]

    print(f"scored {len(scored)} trades {FIRST_TEST_YEAR}-2024; "
          f"model keeps {len(book)} ({len(book)/len(scored)*100:.0f}%)")
    start = f"{FIRST_TEST_YEAR}-01-31"
    for lab, s in [("lag15 ungated (2016+)", base),
                   ("4-signal model book", book),
                   ("model book, AMC only", book[book.when == "AMC"])]:
        r = s[R]
        print(f"{lab:24s} n={len(r):5d}  {r.mean()*100:+.3f}%/spot  "
              f"t={tstat(r):+.2f}  win {(r>0).mean()*100:.0f}%  "
              f"Sharpe {monthly_sharpe(s, start):.2f}")

    # fig 11: equity + prediction-decile calibration
    fig, (ax1, ax2) = plt.subplots(1, 2, figsize=(12.5, 4.8),
                                   gridspec_kw={"width_ratios": [1.7, 1]})
    for sub, lab, c, lw in [
            (base, f"lag 15 ungated (n={len(base)}, "
                   f"{base[R].mean()*100:+.2f}%)", GRAY, 1.2),
            (book, f"4-signal model, pred>0 (n={len(book)}, "
                   f"{book[R].mean()*100:+.2f}%)", VIOLET, 1.8)]:
        s = sub.sort_values("exit")
        ax1.plot(s.exit, s[R].cumsum() * 100, label=lab, color=c, lw=lw)
    ax1.axhline(0, color="k", lw=0.7)
    ax1.set_ylabel("cumulative %/spot (sum over trades)")
    ax1.set_title(f"walk-forward 4-signal model (out-of-sample "
                  f"{FIRST_TEST_YEAR}+)")
    ax1.legend(fontsize=8, loc="upper left")

    q = pd.qcut(scored.pred.rank(method="first"), 10, labels=False)
    means = [scored.loc[q == k, R].mean() * 100 for k in range(10)]
    ax2.bar(np.arange(10), means,
            color=[VIOLET if m >= 0 else RED for m in means])
    for k in range(10):
        r = scored.loc[q == k, R]
        ax2.text(k, means[k], f"{tstat(r):.1f}", ha="center",
                 va="bottom" if means[k] >= 0 else "top", fontsize=7,
                 color="#52514e")
    ax2.set_xticks(np.arange(10), [f"D{k+1}" for k in range(10)], fontsize=8)
    ax2.axhline(0, color="k", lw=0.7)
    ax2.set_ylabel("mean %/spot (gross mid)")
    ax2.set_title("realized return by model-prediction decile\n"
                  "(out-of-sample; numbers = t)", fontsize=10)
    fig.tight_layout()
    fig.savefig(f"{OUT}/11_oq_gate.png", dpi=110)
    print(f"wrote 11_oq_gate.png to {OUT}/")


if __name__ == "__main__":
    main()
