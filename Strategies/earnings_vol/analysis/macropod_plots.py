"""
Fig 22: the macropod out-of-sample replication at a glance.

  a) mean pnl per book, 2006-2012 (never-seen era) vs 2013-2024 (our window)
  b) cumulative pnl of the four books over the full macropod history
     (2006 to Feb-2024 — the file ends 2024-02-16)
  c) cross-dataset validation: per-trade returns on the ~3.8k lag-2 events
     that exist in BOTH our ORATS build and macropod, split AMC/BMO

Run from analysis/ AFTER macropod.py:  python3 macropod_plots.py
"""
import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt
import numpy as np
import pandas as pd

from macropod import monthly_sharpe, s4_gate, tstat, wf_model

BLUE, RED, GRAY, VIOLET = "#4878cf", "#d1615d", "#888888", "#4a3aa7"
OUT = "../figures"


def main():
    tr = pd.read_parquet("../cache/macropod_trades.parquet")
    med_v = tr.groupby("ticker").vol.median()
    top = set(med_v.sort_values(ascending=False).head(200).index)
    t2 = tr[tr.ticker.isin(top)].copy()
    yr = t2.exit.dt.year

    def bk(s):
        l2, l14 = s[s.lag == 2], s[s.lag == 14]
        return {"lag-2\nungated": l2, "lag-14\nnaive": l14,
                "lag-14\n+ s4 gate": s4_gate(l14),
                "lag-14\n+ WF OLS": wf_model(l14, int(s.exit.dt.year.min()) + 3)}

    early, late = t2[yr <= 2012], t2[(yr >= 2013) & (yr <= 2024)]
    fig, axs = plt.subplots(1, 3, figsize=(15, 4.7))

    # ------------------------------------------------------------ a) eras
    ax = axs[0]
    be, bl = bk(early), bk(late)
    labs = list(be)
    x = np.arange(len(labs))
    for off, d, c, lab in [(-0.19, be, GRAY, "2006-2012 (unseen era)"),
                           (0.19, bl, BLUE, "2013-2024 (our window)")]:
        vals = [d[l].ret_spot_mid.mean() * 100 for l in labs]
        ts = [tstat(d[l].ret_spot_mid) for l in labs]
        ax.bar(x + off, vals, width=0.36, color=c, label=lab)
        for xi, (v, t_) in enumerate(zip(vals, ts)):
            ax.annotate(f"t={t_:.1f}", (xi + off, v),
                        ha="center", va="bottom" if v >= 0 else "top",
                        fontsize=7.5, color="#333333")
    ax.axhline(0, color="#444444", lw=0.8)
    ax.set_xticks(x)
    ax.set_xticklabels(labs, fontsize=8)
    ax.set_ylabel("mean pnl, % of spot", fontsize=9)
    ax.legend(fontsize=8, frameon=False)
    ax.set_title("a) both eras hold up (top-200 names)", fontsize=10,
                 loc="left")

    # ------------------------------------------------- b) full-history curves
    ax = axs[1]
    # macropod ends 2024-02-16 — do NOT pad to year-end or the curve
    # flatlines through a fake ten-month tail
    idx = pd.date_range("2006-01-31", t2.exit.max(), freq="ME")
    for (lab, b), c in zip(bk(t2).items(), [BLUE, GRAY, RED, VIOLET]):
        m = (b.set_index("exit").ret_spot_mid.resample("ME").sum()
              .reindex(idx, fill_value=0.0))
        ax.plot(idx, m.cumsum() * 100, color=c, lw=1.4,
                label=f"{lab.replace(chr(10), ' ')} "
                      f"(moSh {monthly_sharpe(b):.2f})")
    ax.axvline(pd.Timestamp("2013-01-01"), color="#999999", ls="--", lw=0.9)
    ax.annotate("our sample starts", (pd.Timestamp("2013-03-01"), 5),
                fontsize=7.5, color="#666666")
    ax.set_ylabel("cumulative pnl, % of spot", fontsize=9)
    ax.legend(fontsize=7.5, frameon=False, loc="upper left")
    ax.set_title("b) macropod full history, 2006 to Feb-2024", fontsize=10, loc="left")

    # ------------------------------------------------ c) cross-dataset check
    ax = axs[2]
    ours = pd.read_parquet("../cache/trades_prern_lagscan.parquet")
    ours = ours[ours.lag == 2][["ticker", "ernDate", "when", "ret_spot_mid"]]
    mp = tr[tr.lag == 2][["ticker", "ernDate", "when", "ret_spot_mid"]].rename(
        columns={"when": "when_mp", "ret_spot_mid": "ret_mp"})
    j = ours.merge(mp, on=["ticker", "ernDate"], how="inner")
    j = j[j.when == j.when_mp]
    for w, c in [("AMC", BLUE), ("BMO", RED)]:
        s = j[j.when == w]
        ax.scatter(s.ret_spot_mid * 100, s.ret_mp * 100, s=9, alpha=0.4,
                   color=c, label=f"{w} (n={len(s)}, r="
                                  f"{s.ret_spot_mid.corr(s.ret_mp):.2f})")
    lim = 12
    ax.plot([-lim, lim], [-lim, lim], color="#444444", lw=0.8, ls="--")
    ax.set_xlim(-lim, lim)
    ax.set_ylim(-lim, lim)
    ax.set_xlabel("our ORATS build, % of spot", fontsize=9)
    ax.set_ylabel("macropod, % of spot", fontsize=9)
    ax.legend(fontsize=8, frameon=False, loc="upper left")
    ax.set_title("c) same events, two vendors (lag-2)", fontsize=10,
                 loc="left")

    for a in axs:
        a.spines[["top", "right"]].set_visible(False)
        a.grid(color="#dddddd", lw=0.6)
        a.set_axisbelow(True)
    fig.suptitle("Out-of-sample replication on the independent macropod "
                 "dataset (4108 tickers, 2006 to Feb-2024)", fontsize=12, y=0.99)
    fig.tight_layout()
    fig.savefig(f"{OUT}/22_macropod_replication.png", dpi=110)
    print(f"wrote {OUT}/22_macropod_replication.png")
    print(f"\nmatched events: {len(j)}")
    for w in ["AMC", "BMO"]:
        s = j[j.when == w]
        print(f"  {w}: ours {s.ret_spot_mid.mean()*100:+.3f}%  "
              f"macropod {s.ret_mp.mean()*100:+.3f}%  "
              f"corr {s.ret_spot_mid.corr(s.ret_mp):.2f}")


if __name__ == "__main__":
    main()
