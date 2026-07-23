"""
Audit of the pre-earnings straddle backtest (2026-07-11, before further work).

Checks, in order:
 1. structural invariants of the trade set (dupes, date ordering, expiry
    spans the jump, filters actually enforced, nominal vs actual entry lag)
 2. event-calendar sanity (spacing between consecutive events per ticker)
 3. outlier sensitivity of the headline mean (winsorized / trimmed means)
 4. clustering: t-stat recomputed on event-month cluster means
 5. GATE LOOKAHEAD: the analysis ranked impErnMv vs the ticker's FULL-sample
    history (uses future data). Recompute with an expanding, PAST-ONLY rank
    (>=4 prior events) and compare the AMC+cheap combo.
 6. marginal-lag pairs: fraction of adjacent-lag pairs with the same entry
    date (weekends), which dilute the marginal bars toward zero.

Run from analysis/:  python3 audit_prern.py
"""
import numpy as np
import pandas as pd

R = "ret_spot_mid"


def t(r):
    r = np.asarray(r, float)
    return r.mean() / r.std() * np.sqrt(len(r))


def main():
    tr = pd.read_parquet("../cache/trades_prern.parquet")
    ls = pd.read_parquet("../cache/trades_prern_lagscan.parquet")
    ev = pd.read_parquet("../cache/events.parquet")

    print("== 1. structural invariants (both trade sets) ==")
    for name, d in [("prern", tr), ("lagscan", ls)]:
        dup = d.duplicated(["ticker", "ernDate", "lag"]).sum()
        bad_order = (d.entry >= d["exit"]).sum()
        # exit strictly before the jump session
        cut = np.where(d.when == "AMC",
                       d.ernDate + pd.Timedelta(days=1), d.ernDate)
        bad_exit = (d["exit"].values >= cut).sum()
        jump = np.where(d.when == "BMO", d.ernDate,
                        d.ernDate + pd.Timedelta(days=1))
        bad_exp = (d.expir.values < jump).sum()
        bad_flt = ((d.spot < 5) | (d.debit_mid < 0.3)
                   | ((d.strike - d.spot).abs() / d.spot > 0.10)).sum()
        exp_after_exit = (d.expir <= d["exit"]).sum()
        print(f"{name:8s} n={len(d):6d} dup={dup} entry>=exit={bad_order} "
              f"exit-in-jump={bad_exit} expiry<jump={bad_exp} "
              f"expiry<=exit={exp_after_exit} filter-violations={bad_flt}")

    print("\n== 1b. nominal lag vs actual calendar distance entry->event ==")
    ls["actual"] = (ls.ernDate - ls.entry).dt.days
    a = ls.groupby("lag").actual.agg(["mean", "median", "max"])
    print(a.round(2).T.to_string())

    print("\n== 2. event spacing per ticker (calendar days) ==")
    sp = ev.sort_values(["ticker", "ernDate"]).groupby("ticker").ernDate \
           .diff().dt.days.dropna()
    print(f"n={len(sp)}  median={sp.median():.0f}  "
          f"p5={sp.quantile(.05):.0f}  p95={sp.quantile(.95):.0f}  "
          f"<30d pairs={(sp < 30).sum()} ({(sp < 30).mean()*100:.1f}%)")

    print("\n== 3. outlier sensitivity, lag 15 ungated ==")
    r = tr.loc[tr.lag == 15, R]
    print(f"raw       mean {r.mean()*100:+.3f}%  t={t(r):+.2f}  "
          f"min {r.min()*100:.1f}%  max {r.max()*100:.1f}%")
    for c in [0.5, 0.2, 0.1]:
        rc = r.clip(-c, c)
        print(f"clip ±{int(c*100):3d}% mean {rc.mean()*100:+.3f}%  t={t(rc):+.2f}")
    rt = r[(r > r.quantile(0.01)) & (r < r.quantile(0.99))]
    print(f"trim 1/99  mean {rt.mean()*100:+.3f}%  t={t(rt):+.2f}")

    print("\n== 4. event-month clustering, lag 15 ungated ==")
    t15 = tr[tr.lag == 15].copy()
    cm = t15.groupby(t15.ernDate.dt.to_period("M"))[R].mean()
    print(f"pooled t={t(t15[R]):+.2f}  vs  {len(cm)} monthly cluster means: "
          f"mean {cm.mean()*100:+.3f}%  t={t(cm):+.2f}  "
          f"pos months {(cm > 0).mean()*100:.0f}%")

    print("\n== 5. GATE: full-sample rank (as analyzed) vs past-only rank ==")
    g = t15.dropna(subset=["impErnMv"]).copy()
    g["rk_full"] = g.groupby("ticker").impErnMv.rank(pct=True)
    g = g.sort_values(["ticker", "ernDate"])
    def past_rank(s):
        v = s.values
        out = np.full(len(v), np.nan)
        for i in range(len(v)):
            if i >= 4:
                out[i] = (v[:i] < v[i]).mean() + 0.5 * (v[:i] == v[i]).mean()
        return pd.Series(out, index=s.index)
    g["rk_past"] = g.groupby("ticker").impErnMv.transform(past_rank)
    for lab, rk in [("full-sample (lookahead)", "rk_full"),
                    ("past-only expanding", "rk_past")]:
        h = g.dropna(subset=[rk])
        cheap = h[h[rk] < 0.33]
        rich = h[h[rk] >= 0.67]
        amc = cheap[cheap.when == "AMC"]
        print(f"{lab:24s} cheap n={len(cheap):5d} {cheap[R].mean()*100:+.3f}% "
              f"t={t(cheap[R]):+.2f} | rich {rich[R].mean()*100:+.3f}% "
              f"t={t(rich[R]):+.2f} | AMC+cheap n={len(amc):4d} "
              f"{amc[R].mean()*100:+.3f}% t={t(amc[R]):+.2f}")

    print("\n== 6. adjacent-lag marginal pairs with identical entry date ==")
    w = ls.pivot_table(index=["ticker", "ernDate"], columns="lag",
                       values="entry", aggfunc="first")
    lags = sorted(w.columns)
    same = [(f"{b}-{a}", (w[a] == w[b]).mean())
            for a, b in zip(lags, lags[1:])]
    print("  ".join(f"{k}:{v*100:.0f}%" for k, v in same))


if __name__ == "__main__":
    main()
