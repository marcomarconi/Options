"""
Fig 21: AMC vs BMO split on the INDEPENDENT macropod dataset — the external
check on fig 19 (which found AMC roughly doubles BMO on our ORATS build).

AMC reports jump after the report-day close, so an AMC trade is held to the
close OF the report day and captures the last full day of IV run-up; a BMO
trade must be sold the previous close, a whole day further from the event.
macropod carries the announcement time explicitly (earningsTime), so unlike
our own build nothing here is inferred.

Books: lag-2 ungated, lag-14 ungated, lag-14 + s4-low gate (gate applied to
the whole book first, then split — no AMC condition inside the gate).
Run from analysis/:  python3 macropod_amc_bmo.py
"""
import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt
import numpy as np
import pandas as pd

from macropod import monthly_sharpe, s4_gate, tstat

BLUE, RED, GRAY = "#4878cf", "#d1615d", "#888888"
OUT = "../figures"
TR = "../cache/macropod_trades.parquet"


def books(s):
    l2, l14 = s[s.lag == 2], s[s.lag == 14]
    return {"lag-2 ungated": l2, "lag-14 ungated": l14,
            "lag-14 + s4-low gate": s4_gate(l14)}


def report(title, s):
    print(f"\n--- {title} ---")
    print(f"{'book':<24}{'when':>6}{'n':>8}{'%/spot':>9}{'t':>7}"
          f"{'win':>6}{'moSh':>7}")
    for lab, b in books(s).items():
        for w in ["AMC", "BMO"]:
            x = b[b.when == w]
            if len(x) < 30:
                continue
            r = x.ret_spot_mid
            print(f"{lab:<24}{w:>6}{len(x):>8}{r.mean()*100:>+8.3f}%"
                  f"{tstat(r):>7.1f}{(r>0).mean()*100:>5.0f}%"
                  f"{monthly_sharpe(x):>7.2f}")


def main():
    tr = pd.read_parquet(TR)
    print(f"{len(tr)} trades; when mix: "
          f"{dict(tr.when.value_counts())}")

    med_v = tr.groupby("ticker").vol.median()
    top = set(med_v.sort_values(ascending=False).head(200).index)
    yr = tr.exit.dt.year
    cuts = [("ALL tickers | 2013-2024", tr[(yr >= 2013) & (yr <= 2024)]),
            ("ALL tickers | 2006-2012", tr[yr <= 2012]),
            ("top-200 | 2013-2024",
             tr[(yr >= 2013) & (yr <= 2024) & tr.ticker.isin(top)]),
            ("top-200 | 2006-2012", tr[(yr <= 2012) & tr.ticker.isin(top)])]
    for t, s in cuts:
        report(t, s)

    # ------------------------------------------------------------- fig 21
    main_cut = tr[(yr >= 2013) & (yr <= 2024)]
    bk = books(main_cut)
    fig, axs = plt.subplots(1, 3, figsize=(14.5, 4.6))

    ax = axs[0]
    labs = list(bk)
    x = np.arange(len(labs))
    for off, w, c in [(-0.18, "AMC", BLUE), (0.18, "BMO", RED)]:
        vals = [bk[l][bk[l].when == w].ret_spot_mid.mean() * 100 for l in labs]
        ts = [tstat(bk[l][bk[l].when == w].ret_spot_mid) for l in labs]
        ax.bar(x + off, vals, width=0.34, color=c, label=w)
        for xi, (v, t_) in enumerate(zip(vals, ts)):
            ax.annotate(f"t={t_:.0f}", (xi + off, v), ha="center",
                        va="bottom", fontsize=7.5, color="#333333")
    ax.axhline(0, color="#444444", lw=0.8)
    ax.set_xticks(x)
    ax.set_xticklabels([l.replace(" + ", "\n+ ") for l in labs], fontsize=8)
    ax.set_ylabel("mean pnl, % of spot", fontsize=9)
    ax.legend(fontsize=8, frameon=False)
    ax.set_title("a) AMC vs BMO, macropod 2013-2024", fontsize=10, loc="left")

    for ax, lab in [(axs[1], "lag-2 ungated"),
                    (axs[2], "lag-14 + s4-low gate")]:
        for w, c in [("AMC", BLUE), ("BMO", RED)]:
            s = bk[lab][bk[lab].when == w]
            m = (s.set_index("exit").ret_spot_mid.resample("ME").sum()
                  .reindex(pd.date_range("2013-01-31", main_cut.exit.max(),
                                         freq="ME"), fill_value=0.0))
            ax.plot(m.index, m.cumsum() * 100, color=c, lw=1.4,
                    label=f"{w} (moSh {monthly_sharpe(s):.2f})")
        ax.set_ylabel("cumulative pnl, % of spot", fontsize=9)
        ax.legend(fontsize=8, frameon=False, loc="upper left")
        ax.set_title(f"{'b' if lab.startswith('lag-2') else 'c'}) {lab}",
                     fontsize=10, loc="left")

    for a in axs:
        a.spines[["top", "right"]].set_visible(False)
        a.grid(axis="y", color="#dddddd", lw=0.6)
        a.set_axisbelow(True)
    fig.suptitle("AMC vs BMO on the independent macropod dataset "
                 "(external check on fig 19)", fontsize=12, y=0.99)
    fig.tight_layout()
    fig.savefig(f"{OUT}/21_macropod_amc_bmo.png", dpi=110)
    print(f"\nwrote {OUT}/21_macropod_amc_bmo.png")


if __name__ == "__main__":
    main()
