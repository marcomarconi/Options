"""
Analysis of the LONG pre-earnings ATM straddle trades.

GROSS ONLY (mid marks) — house rule since 2026-07-11: costs are ignored
entirely (Marco doesn't trust EOD NBBO cost estimates).
Gate uses the PAST-ONLY expanding impErnMv rank from gate.py (the first-pass
full-sample rank had lookahead; caught by audit_prern.py).

Reads cache/trades_prern.parquet; prints per-lag headline, by-year, BMO/AMC,
impErnMv-gate, decomposition into quiet/moving underlyings.
Writes out/prern_summary.csv.  Run from analysis/:  python3 prern_analysis.py
"""
import numpy as np
import pandas as pd

from gate import add_mv_rank

R = "ret_spot_mid"


def block(s, label):
    r = s[R]
    if len(s) < 5:
        return None
    return dict(sel=label, n=len(s), mean_pct=r.mean() * 100,
                t=r.mean() / r.std() * np.sqrt(len(r)),
                win_pct=(r > 0).mean() * 100,
                med_pct=r.median() * 100)


def show(rows, title):
    df = pd.DataFrame([b for b in rows if b])
    print(f"\n== {title} ==")
    print(df.round(3).to_string(index=False))
    return df


def main():
    tr = pd.read_parquet("../cache/trades_prern.parquet")
    tr["year"] = tr.ernDate.dt.year
    print(f"{len(tr)} trades  {tr.ticker.nunique()} tickers  "
          f"{tr.ernDate.min().date()}..{tr.ernDate.max().date()}")

    out = []
    out.append(show([block(s, f"lag {lag}") for lag, s in tr.groupby("lag")],
                    "by entry lag (calendar days before event), gross mid"))

    t15 = tr[tr.lag == 15].copy()
    show([block(s, str(y)) for y, s in t15.groupby("year")], "lag 15 by year")
    show([block(s, w) for w, s in t15.groupby("when")], "lag 15 by announce session")

    # decomposition: straddle pnl = |spot move| capture vs premium change
    t15["absmove"] = t15.spot_ret.abs()
    lo, hi = t15.absmove.quantile([0.5, 0.9])
    show([block(t15[t15.absmove <= lo], f"|spot mv| <= med ({lo:.1%})"),
          block(t15[t15.absmove > hi], f"|spot mv| > p90 ({hi:.1%})")],
         "lag 15: quiet vs moving underlyings (is pnl just gamma?)")

    # rich/cheap gate, PAST-ONLY rank (no lookahead)
    g = add_mv_rank(t15).dropna(subset=["mv_rk"])
    if len(g) > 100:
        show([block(g[g.mv_rk < 0.33], "impErnMv low 1/3 (cheap)"),
              block(g[(g.mv_rk >= 0.33) & (g.mv_rk < 0.67)], "mid"),
              block(g[g.mv_rk >= 0.67], "impErnMv high 1/3 (rich)")],
             "lag 15: gate by impErnMv past-only rank (own history)")
        show([block(g[g.when == "AMC"], "AMC only"),
              block(g[(g.when == "AMC") & (g.mv_rk < 0.33)], "AMC + cheap 1/3"),
              block(g[(g.when == "AMC") & (g.mv_rk < 0.33)
                      & (g.ernDate.dt.year >= 2019)], "AMC + cheap, 2019+")],
             "lag 15: best combo (AMC session x cheap implied move)")

    print(f"\nheld days (lag 15): median {t15.held.median():.0f}; "
          f"debit median {(t15.debit_mid/t15.spot).median()*100:.1f}% of spot")

    pd.concat(out).to_csv("../out/prern_summary.csv", index=False)
    print("wrote ../out/prern_summary.csv")


if __name__ == "__main__":
    main()
