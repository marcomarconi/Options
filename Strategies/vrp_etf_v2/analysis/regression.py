#!/usr/bin/env python3
"""
Retest of the video's ENTRY MODEL: a linear regression on the three signals, gated at a
threshold, vs the obvious alternatives.

Walk-forward only. A random train/test split leaks here -- the target overlaps 21 sessions
and the panel is one big cross-section through time, so the same regime lands on both
sides of a random split and the model looks like it generalises when it has memorised.
Each year is predicted by a model fitted strictly on prior years.

Books are monthly and non-overlapping: one entry per ticker per month, equal-weighted,
compounding return-on-margin. Compounding P&L/premium instead would be wrong -- a single
delta-hedged straddle can lose more than the premium, so that series goes below -100%
and the product is meaningless.

Run: /home/marco/trading/.venv_orats/bin/python regression.py
Writes out/books.csv, out/walkforward.csv
"""
import numpy as np
import pandas as pd

from common import BASE, load

FEATS = ["f_ivrv", "f_ivpct", "f_ffr"]
START = 2010            # first predicted year; 2007-09 is the training seed
Y = "y_margin"          # the video's own metric


def fit(tr, feats, y):
    X = np.column_stack([np.ones(len(tr))] + [tr[f].values for f in feats])
    return np.linalg.lstsq(X, tr[y].values, rcond=None)[0]


def pred(d, feats, b):
    return np.column_stack([np.ones(len(d))] + [d[f].values for f in feats]) @ b


def walkforward(d):
    """Refit every year on everything before it. Returns d with pred_* columns filled
    only for years >= START."""
    d = d.copy()
    d["pred_full"] = np.nan
    d["pred_ivrv"] = np.nan
    coefs = []
    for yr in range(START, int(d.tradeDate.dt.year.max()) + 1):
        tr = d[d.tradeDate.dt.year < yr]
        te = d.tradeDate.dt.year == yr
        if len(tr) < 5000 or not te.any():
            continue
        bF, bS = fit(tr, FEATS, Y), fit(tr, ["f_ivrv"], Y)
        d.loc[te, "pred_full"] = pred(d[te], FEATS, bF)
        d.loc[te, "pred_ivrv"] = pred(d[te], ["f_ivrv"], bS)
        coefs.append(dict(year=yr, n_train=len(tr), const=bF[0], ivrv=bF[1],
                          ivpct=bF[2], ffr=bF[3]))
    return d[d.tradeDate.dt.year >= START].copy(), pd.DataFrame(coefs)


def monthly(d):
    """One entry per ticker per month -- the first signal date of that month."""
    d = d.copy()
    d["ym"] = d.tradeDate.values.astype("datetime64[M]")
    return d.sort_values("tradeDate").groupby(["ticker", "ym"], as_index=False).first()


def xs_rank(m, col):
    """Cross-sectional percentile WITHIN each month -- the screener's actual question."""
    return m.groupby("ym")[col].rank(pct=True)


def book(m, mask, label, out):
    sel = m[mask]
    if sel.empty:
        return None
    g = sel.groupby("ym")[Y].mean().sort_index()
    idx = pd.period_range(m.ym.min(), m.ym.max(), freq="M").to_timestamp()
    g = g.reindex(idx, fill_value=0.0)                 # idle months are flat, not skipped
    eq = (1 + g).cumprod()
    dd = (eq / eq.cummax() - 1).min()
    sh = g.mean() / g.std() * np.sqrt(12) if g.std() > 0 else np.nan
    cagr = eq.iloc[-1] ** (12 / len(g)) - 1
    r = dict(book=label, n=len(sel), per_mo=len(sel) / max((g != 0).sum(), 1),
             mean_mo=100 * g.mean(), sd_mo=100 * g.std(), sharpe=sh,
             cagr=100 * cagr, final=eq.iloc[-1], maxdd=100 * dd,
             pos_mo=100 * (g[g != 0] > 0).mean(), worst=100 * g.min())
    out.append(r)
    return eq


def run(d, label, out):
    wf, coefs = walkforward(d)
    m = monthly(wf)
    print(f"\n{'='*100}\n  {label}   {m.ym.min():%Y-%m}..{m.ym.max():%Y-%m}   "
          f"{len(m):,} monthly entries, {m.ticker.nunique()} tickers\n{'='*100}")
    print("  walk-forward OLS coefficients (refit each year, on prior years only)")
    print("  " + coefs.round(4).tail(6).to_string(index=False).replace("\n", "\n  "))

    m["r_full"] = xs_rank(m, "pred_full")
    m["r_ivrv"] = xs_rank(m, "f_ivrv")
    m["r_vov"] = xs_rank(m, "f_vov")
    m["r_ffr"] = 1 - xs_rank(m, "f_ffr")          # inverted: LOW ratio = backwardation = good
    m["r_blend"] = (m.r_ivrv + m.r_vov) / 2
    m["r_three"] = (m.r_ivrv + m.r_vov + m.r_ffr) / 3

    rows, eqs = [], {}
    top = 0.9                                      # top decile, as in the old study
    for col, name in [(None, "blind: sell everything"),
                      ("r_full", "video OLS (3 feat), top 10%"),
                      ("r_ivrv", "IV/RV rank, top 10%"),
                      ("r_vov", "volOfVol rank, top 10%"),
                      ("r_ffr", "backwardation rank, top 10%"),
                      ("r_blend", "IV/RV + VoV blend, top 10%"),
                      ("r_three", "IV/RV + VoV + backwardation, top 10%")]:
        mask = pd.Series(True, index=m.index) if col is None else m[col] >= top
        eq = book(m, mask, name, rows)
        if eq is not None:
            eqs[name] = eq
    t = pd.DataFrame(rows)
    print()
    print(t.round(2).to_string(index=False))
    for r in rows:
        r["universe"] = label
    out.extend(rows)
    return m, eqs, coefs


def main():
    out = []
    for kind, label in [("etf", "clean liquid ETFs"), ("stock", "single names")]:
        d = load(kind)
        run(d, label, out)
    pd.DataFrame(out).to_csv(f"{BASE}/out/books.csv", index=False)
    print(f"\nwrote out/books.csv")


if __name__ == "__main__":
    main()
