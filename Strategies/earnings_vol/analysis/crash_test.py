"""
Fig 20 + table: crash test of the lag-2 pre-earnings straddle book against
the passive core and the weekend short-vol sleeve (the question from
diversifier_tsmom/complement_study: what survives crashes NEXT TO the core?).

Lag-2 book: floor debit >= $0.75 (live fee floor), equal-premium units,
monthly $ pnl scaled to a 10%/yr-vol sleeve — same convention as the
complement study's sleeves. Core + weekend monthly series come from
diversifier_tsmom/results/monthly_streams_complement.csv.

Prints standalone stats, down-month conditioning, named crash windows, and
the 100k core + 30k sleeve blends (weekend / prern / 50-50).
Run from analysis/:  python3 crash_test.py
"""
import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt
import numpy as np
import pandas as pd

import unit_for_risk as ufr

BLUE, RED, GRAY, VIOLET = "#4878cf", "#d1615d", "#888888", "#4a3aa7"
OUT = "../figures"
SP = ("/home/marco/trading/Systems/Options/Strategies/diversifier_tsmom/"
      "results")

WINDOWS = [("2015 China deval", "2015-08-01", "2016-02-29"),
           ("2018 Q4", "2018-10-01", "2018-12-31"),
           ("2020 COVID", "2020-02-01", "2020-03-31"),
           ("2022 bear", "2022-01-01", "2022-10-31")]


def ann(s):
    mu, sd = s.mean() * 12, s.std() * np.sqrt(12)
    return mu, sd, mu / sd


def mdd(s):
    eq = (1 + s).cumprod()
    return (eq / eq.cummax() - 1).min()


def prern_monthly():
    b = ufr.load()                       # lag-2, debit >= $0.75
    u = 1000.0                           # scale-free; re-volled below
    n = (u // (100 * b.debit_mid)).astype(int)
    pnl = n * 100 * b.debit_mid * b.ret_debit_mid
    d = pnl.groupby(pd.to_datetime(b.exit)).sum()
    f = d.reindex(pd.date_range("2013-01-01", "2024-12-31", freq="D"),
                  fill_value=0.0)
    m = f.resample("ME").sum()
    # express as monthly returns of a sleeve vol-matched to 10%/yr
    return m / m.std() * (0.10 / np.sqrt(12))


def main():
    M = pd.read_csv(f"{SP}/monthly_streams_complement.csv",
                    index_col=0, parse_dates=True)[["core", "weekend"]]
    M["weekend"] = (M.weekend / M.weekend.std()
                    * (0.10 / np.sqrt(12)))       # re-vol to 10%/yr window
    M["prern"] = prern_monthly()
    Mc = M.dropna()
    print(f"common window {Mc.index[0].date()} .. {Mc.index[-1].date()} "
          f"(n={len(Mc)}; 2021 Q1 = data gap, prern rows are 0)")

    print(f"\n{'stream':<9}{'ann.ret':>9}{'ann.vol':>9}{'Sharpe':>8}"
          f"{'maxDD':>8}{'corr':>7}{'dn-corr':>9}")
    dn = Mc.core < 0
    for c in Mc.columns:
        mu, sd, sh = ann(Mc[c])
        dc = (Mc.loc[dn, c].corr(Mc.loc[dn, "core"])
              if c != "core" else 1.0)
        print(f"{c:<9}{mu*100:>8.1f}%{sd*100:>8.1f}%{sh:>8.2f}"
              f"{mdd(Mc[c])*100:>7.0f}%{Mc[c].corr(Mc.core):>+7.2f}"
              f"{dc:>+9.2f}")

    for q, lab in [(0.10, "worst-DECILE"), (0.25, "worst-quartile")]:
        thr = Mc.core.quantile(q)
        w = Mc[Mc.core <= thr]
        print(f"\n{lab} core months (n={len(w)}, "
              f"avg core {w.core.mean()*100:+.2f}%/mo):")
        for c in ["weekend", "prern"]:
            print(f"  {c:<9} avg {w[c].mean()*100:+.3f}%/mo   "
                  f"negative in {(w[c] < 0).mean()*100:.0f}% of them")

    print(f"\n{'window':<18}{'core':>9}{'weekend':>9}{'prern':>9}")
    wtot = {}
    for nm, a, b_ in WINDOWS:
        w = Mc.loc[a:b_]
        tot = [(1 + w[c]).prod() - 1 for c in Mc.columns]
        wtot[nm] = tot
        print(f"{nm:<18}" + "".join(f"{t*100:>+8.1f}%" for t in tot))

    print("\nblends: 100k core + 30k sleeve (sleeve returns on 30k):")
    print(f"{'blend':<22}{'Sharpe':>8}{'maxDD':>8}{'2020-02..03':>12}")
    blends = {"core alone": None, "core + weekend": {"weekend": 1.0},
              "core + prern": {"prern": 1.0},
              "core + 50/50 mix": {"weekend": 0.5, "prern": 0.5}}
    rows = {}
    for lab, mix in blends.items():
        r = Mc.core * 100 / 130
        if mix:
            for c, wgt in mix.items():
                r = r + Mc[c] * wgt * 30 / 130
        mu, sd, sh = ann(r)
        cw = Mc.loc["2020-02-01":"2020-03-31"]
        c20 = (1 + (cw.core * 100 / 130
                    + (sum(cw[c] * wgt for c, wgt in mix.items()) * 30 / 130
                       if mix else 0))).prod() - 1
        rows[lab] = (sh, mdd(r), c20)
        print(f"{lab:<22}{sh:>8.2f}{mdd(r)*100:>7.0f}%{c20*100:>+11.1f}%")

    # ---------------------------------------------------------------- fig 20
    fig, axs = plt.subplots(1, 3, figsize=(14.5, 4.6))
    ax = axs[0]
    x = np.arange(len(WINDOWS))
    for off, i, c, lab in [(-0.25, 0, GRAY, "core"),
                           (0.0, 1, RED, "weekend short-vol"),
                           (0.25, 2, BLUE, "prern lag-2")]:
        vals = [wtot[nm][i] * 100 for nm, *_ in WINDOWS]
        ax.bar(x + off, vals, width=0.24, color=c, label=lab)
    ax.axhline(0, color="#444444", lw=0.8)
    ax.set_xticks(x)
    ax.set_xticklabels([nm.replace(" ", "\n", 1) for nm, *_ in WINDOWS],
                       fontsize=8)
    ax.set_ylabel("window total return, % (sleeves at 10% ann vol)",
                  fontsize=9)
    ax.legend(fontsize=8, frameon=False, loc="lower left")
    ax.set_title("a) named crash windows", fontsize=10, loc="left")

    ax = axs[1]
    ax.scatter(Mc.core * 100, Mc.prern * 100, s=14, color=BLUE, alpha=0.55)
    w = Mc[Mc.core <= Mc.core.quantile(0.10)]
    ax.scatter(w.core * 100, w.prern * 100, s=26, color=RED, zorder=3,
               label="worst-decile core months")
    ax.axhline(0, color="#bbbbbb", lw=0.8)
    ax.axvline(0, color="#bbbbbb", lw=0.8)
    ax.set_xlabel("core (world equity) monthly %", fontsize=9)
    ax.set_ylabel("prern lag-2 monthly %", fontsize=9)
    ax.legend(fontsize=8, frameon=False, loc="upper left")
    ax.set_title(f"b) no crash beta: dn-corr "
                 f"{Mc.loc[dn,'prern'].corr(Mc.loc[dn,'core']):+.2f}",
                 fontsize=10, loc="left")

    ax = axs[2]
    labs = list(rows)
    sh = [rows[k][0] for k in labs]
    bars = ax.bar(range(len(labs)), sh, width=0.6,
                  color=[GRAY, RED, BLUE, VIOLET])
    for i, k in enumerate(labs):
        ax.annotate(f"{rows[k][0]:.2f}", (i, rows[k][0] + 0.015),
                    ha="center", fontsize=9, color="#333333")
        ax.annotate(f"DD {rows[k][1]*100:.0f}%", (i, 0.03), ha="center",
                    fontsize=8, color="white")
    ax.set_xticks(range(len(labs)))
    ax.set_xticklabels([l.replace("core + ", "core+\n") for l in labs],
                       fontsize=8)
    ax.set_ylabel("blend Sharpe (100k core + 30k sleeve)", fontsize=9)
    ax.set_title("c) portfolio blends", fontsize=10, loc="left")

    for a in axs:
        a.spines[["top", "right"]].set_visible(False)
        a.grid(axis="y", color="#dddddd", lw=0.6)
        a.set_axisbelow(True)
    fig.suptitle("Crash test: the long-gamma pre-earnings book next to the "
                 "core and the short-vol sleeve (2013-2024)",
                 fontsize=12, y=0.99)
    fig.tight_layout()
    fig.savefig(f"{OUT}/20_crash_test.png", dpi=110)
    print(f"\nwrote {OUT}/20_crash_test.png")


if __name__ == "__main__":
    main()
