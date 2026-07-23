"""
Fig 18: the restrike rule (README RESTRIKE RULE / DELTA TRIGGER verdicts).

(a) cumulative pnl per unit premium, lag-15 book: hold vs the 5%-move
    trigger vs the net-delta triggers (bank-excess sizing);
(b) trigger rate by entry-vol quintile — the %-rule fires 15%->90% low->high
    vol, the delta rule is flat (the normalization that wins);
(c) monthly Sharpe (equal-premium convention) by rule.
Reads cache/restrike_delta_results.parquet (analysis/restrike_delta.py).
Run from analysis/:  python3 restrike_plots.py
"""
import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt
import numpy as np
import pandas as pd

BLUE, RED, GRAY, VIOLET = "#4878cf", "#d1615d", "#888888", "#4a3aa7"
OUT = "../figures"
IDX = pd.date_range("2013-01-31", "2024-12-31", freq="ME")


def monthly(d, col):
    return (d.set_index("exit")[col].resample("ME").sum()
             .reindex(IDX, fill_value=0.0))


def main():
    res = pd.read_parquet("../cache/restrike_delta_results.parquet")
    res["exit"] = pd.to_datetime(res["exit"])
    arms = [("pct", 0.05, BLUE, "5%-move trigger"),
            ("delta", 0.30, VIOLET, "net delta ≥ 0.30"),
            ("delta", 0.20, RED, "net delta ≥ 0.20")]

    fig, axs = plt.subplots(1, 3, figsize=(14.5, 4.6),
                            gridspec_kw={"width_ratios": [1.5, 1, 1]})
    ax = axs[0]
    base = res[(res.kind == "pct") & (res.thresh == 0.05)]
    mh = monthly(base, "hold").cumsum()
    ax.plot(mh.index, mh.values, color=GRAY, lw=1.6)
    ax.annotate("hold", (mh.index[-1], mh.iloc[-1]), color=GRAY,
                fontsize=9, va="center", xytext=(4, 0),
                textcoords="offset points")
    for kind, th, c, lab in arms:
        d = res[(res.kind == kind) & (res.thresh == th)]
        m = monthly(d, "pnl").cumsum()
        ax.plot(m.index, m.values, color=c, lw=1.8)
        ax.annotate(lab, (m.index[-1], m.iloc[-1]), color=c, fontsize=9,
                    va="center", xytext=(4, 0), textcoords="offset points")
    ax.set_xlim(IDX[0], IDX[-1] + pd.Timedelta(days=1500))
    ax.set_ylabel("cumulative pnl, units of premium", fontsize=9)
    ax.set_title("a) lag-15 book: restrike vs hold (bank-excess)",
                 fontsize=10, loc="left")

    ax = axs[1]
    res["q"] = pd.qcut(res.relvol, 5, labels=False)
    for kind, th, c, lab in [("pct", 0.05, BLUE, "5%-move"),
                             ("delta", 0.30, VIOLET, "delta ≥ 0.30")]:
        d = res[(res.kind == kind) & (res.thresh == th)]
        r = d.groupby("q").apply(lambda g: (g.restrikes > 0).mean())
        ax.plot(r.index, r.values * 100, "-o", color=c, lw=2, ms=6, label=lab)
    ax.set_ylim(0, 100)
    ax.set_xticks(range(5))
    ax.set_xticklabels(["Q1\nlow vol", "Q2", "Q3", "Q4", "Q5\nhigh vol"],
                       fontsize=8)
    ax.set_ylabel("% of trades triggered", fontsize=9)
    ax.legend(fontsize=9, frameon=False, loc="center right")
    ax.set_title("b) who gets restruck: fixed % is a\n    high-vol rule in disguise",
                 fontsize=10, loc="left")

    ax = axs[2]
    rules, sh, cols = [], [], []
    rules.append("hold"); cols.append(GRAY)
    m = monthly(base, "hold"); sh.append(m.mean() / m.std() * np.sqrt(12))
    for kind, th, c, lab in [("pct", 0.05, BLUE, "move\n≥5%"),
                             ("delta", 0.50, VIOLET, "Δ≥.5"),
                             ("delta", 0.40, VIOLET, "Δ≥.4"),
                             ("delta", 0.30, VIOLET, "Δ≥.3"),
                             ("delta", 0.20, RED, "Δ≥.2")]:
        d = res[(res.kind == kind) & (res.thresh == th)]
        m = monthly(d, "pnl")
        rules.append(lab); cols.append(c)
        sh.append(m.mean() / m.std() * np.sqrt(12))
    bars = ax.bar(range(len(rules)), sh, color=cols, width=0.65)
    for i, v in enumerate(sh):
        ax.annotate(f"{v:.2f}", (i, v + 0.03), ha="center", fontsize=9,
                    color="#333333")
    ax.set_xticks(range(len(rules)))
    ax.set_xticklabels(rules, fontsize=8)
    ax.set_ylim(0, 2.35)
    ax.set_ylabel("monthly Sharpe (equal premium)", fontsize=9)
    ax.set_title("c) tighter delta trigger = better", fontsize=10, loc="left")

    for a in axs:
        a.spines[["top", "right"]].set_visible(False)
        a.grid(axis="y", color="#dddddd", lw=0.6)
        a.set_axisbelow(True)
    fig.suptitle("Restrike rule (lag-15): re-center the straddle when it "
                 "degenerates into delta", fontsize=12, y=0.99)
    fig.tight_layout()
    fig.savefig(f"{OUT}/18_restrike.png", dpi=110)
    print(f"wrote {OUT}/18_restrike.png")


if __name__ == "__main__":
    main()
