#!/usr/bin/env python3
"""
Faster vol-of-vol derivations vs ORATS's slow volOfVol/volOfIvol.

Marco's point: ORATS volOfVol/volOfIvol are heavily smoothed and lag turning points --
bad for a signal whose whole job is to time a short-vol entry. Try faster derivations
built straight off the panel's own IV/RV series (permitted ORATS columns only):

  iv_std{W}  = rolling W-session std of iv30d          (abs)   and /mean  (CV, scale-free)
  rv_std{W}  = rolling W-session std of clsHv20d       (abs)   and /mean  (CV)
  ivchg_std{W} = rolling W-session std of daily d(iv30d)        -- "vol of IV *changes*", fastest

scored the HONEST way -- against the REAL delta-hedged trade P&L (r_mid), one
non-overlapping trade per name per month, so monthly ICs are independent (no sqrt-21
deflation; that was only for the overlapping proxy). Every candidate is judged on:
  * monthly cross-sectional rank-IC vs r_mid, and its t
  * PARTIAL IC controlling {IV level, IV/RV} -- the test that justified vov as a 2nd signal
    (a vov that only re-encodes "high IV name" adds nothing the blend doesn't already have)
  * quintile Q5-Q1 on the real trade, and top-decile Sharpe
  * does swapping it into the 3-signal blend beat the volOfVol blend, gross and liquid-net

Run: /home/marco/trading/.venv_orats/bin/python fast_vov.py
"""
import numpy as np
import pandas as pd
from scipy import stats as sps

from common import BASE

LIQ = 2.5e5
WINDOWS = [10, 21, 42, 63]


def build_fast(tickers):
    """Rolling vol-of-vol candidates per ticker off the panel's IV/RV series."""
    p = pd.read_parquet(f"{BASE}/cache/panel.parquet",
                        columns=["ticker", "tradeDate", "iv30d", "clsHv20d"])
    p = p[p.ticker.isin(tickers)].sort_values(["ticker", "tradeDate"]).reset_index(drop=True)
    g = p.groupby("ticker", sort=False)
    p["div"] = g.iv30d.diff()
    out = {}
    for W in WINDOWS:
        iv_std = g.iv30d.transform(lambda s: s.rolling(W, min_periods=W // 2).std())
        rv_std = g.clsHv20d.transform(lambda s: s.rolling(W, min_periods=W // 2).std())
        iv_mean = g.iv30d.transform(lambda s: s.rolling(W, min_periods=W // 2).mean())
        rv_mean = g.clsHv20d.transform(lambda s: s.rolling(W, min_periods=W // 2).mean())
        ivchg = g.div.transform(lambda s: s.rolling(W, min_periods=W // 2).std())
        out[f"iv_std{W}"] = iv_std
        out[f"iv_cv{W}"] = iv_std / iv_mean.replace(0, np.nan)
        out[f"rv_std{W}"] = rv_std
        out[f"rv_cv{W}"] = rv_std / rv_mean.replace(0, np.nan)
        out[f"ivchg_std{W}"] = ivchg
    for k, v in out.items():
        p[k] = v.values
    return p[["ticker", "tradeDate"] + list(out.keys())]


def _rank(g, col):
    return g.groupby("ym")[col].rank(pct=True)


def score(t, col):
    """monthly cross-sectional rank-IC of `col` vs the real trade r_mid, raw and partial."""
    raw, par, pos = [], [], []
    for ym, g in t.groupby("ym"):
        d = g[[col, "r_mid", "f_ivlvl", "f_ivrv"]].dropna()
        if len(d) < 8:
            continue
        R = d.rank(pct=True)
        s, y = R[col].values, R.r_mid.values
        raw.append(np.corrcoef(s, y)[0, 1])
        Z = np.column_stack([np.ones(len(d)), R.f_ivlvl.values, R.f_ivrv.values])
        sr = s - Z @ np.linalg.lstsq(Z, s, rcond=None)[0]
        yr = y - Z @ np.linalg.lstsq(Z, y, rcond=None)[0]
        if sr.std() > 1e-9 and yr.std() > 1e-9:
            par.append(np.corrcoef(sr, yr)[0, 1])
        pos.append(raw[-1] > 0)
    raw, par = np.array(raw), np.array(par)
    tstat = lambda a: a.mean() / (a.std(ddof=1) / np.sqrt(len(a))) if len(a) > 1 else np.nan
    # quintile Q5-Q1 and top-decile on the real trade
    q = t[[col, "r_mid", "ym"]].dropna().copy()
    q["Q"] = q.groupby("ym")[col].transform(
        lambda s: pd.qcut(s.rank(method="first"), 5, labels=False, duplicates="drop")
        if s.nunique() >= 5 else np.nan)
    m = q.dropna(subset=["Q"]).groupby("Q").r_mid.mean()
    q5q1 = (m.iloc[-1] - m.iloc[0]) * 100 if len(m) == 5 else np.nan
    dec = q.assign(D=q.groupby("ym")[col].transform(
        lambda s: pd.qcut(s.rank(method="first"), 10, labels=False, duplicates="drop")
        if s.nunique() >= 10 else np.nan))
    top = dec[dec.D == 9].groupby("ym").r_mid.mean()
    top_sh = top.mean() / top.std() * np.sqrt(12) if top.std() > 0 else np.nan
    return dict(ic=raw.mean(), t=tstat(raw), pic=par.mean(), pt=tstat(par),
                pos=raw.mean() > 0 and np.mean(pos), q5q1=q5q1,
                top_mean=top.mean() * 100, top_sh=top_sh, n=len(raw))


def blend_perf(t, vov_col):
    """3-signal blend with `vov_col` as the vov leg: gross top-20% and liquid-net top-50%."""
    t = t.copy()
    t["q_ivrv"] = _rank(t, "f_ivrv")
    t["q_vov"] = _rank(t, vov_col)
    t["q_ffr"] = 1 - _rank(t, "f_ffr")
    t["blend"] = (t.q_ivrv + t.q_vov + t.q_ffr) / 3
    g = t.dropna(subset=["blend"])
    top = g[g.blend >= 0.8].groupby("ym").r_mid.mean()
    gsh = top.mean() / top.std() * np.sqrt(12) if top.std() > 0 else np.nan
    L = g[g.avgOptVolu20d >= LIQ].copy()
    L["ql"] = ((L.groupby("ym").f_ivrv.rank(pct=True) + L.groupby("ym")[vov_col].rank(pct=True)
                + (1 - L.groupby("ym").f_ffr.rank(pct=True))) / 3)
    net = L[L.ql >= 0.5].groupby("ym").r_taker.mean()
    nsh = net.mean() / net.std() * np.sqrt(12) if net.std() > 0 else np.nan
    return dict(gross_mean=top.mean() * 100, gross_sh=gsh,
                net_mean=net.mean() * 100, net_sh=nsh)


def main():
    t = pd.read_parquet(f"{BASE}/cache/real_trades_etf.parquet")
    t["ym"] = t.tradeDate.values.astype("datetime64[M]")
    fast = build_fast(set(t.ticker.unique()))
    t = t.merge(fast, on=["ticker", "tradeDate"], how="left")
    print(f"{len(t):,} real ETF trades, {t.ticker.nunique()} names, "
          f"{t.tradeDate.min().date()}..{t.tradeDate.max().date()}\n")

    cands = ["volOfVol", "volOfIvol"] + [c for c in fast.columns if c not in ("ticker", "tradeDate")]
    # rename ORATS cols to the merged f_ names
    ren = {"volOfVol": "f_vov", "volOfIvol": "f_voiv"}
    rows = []
    print(f"{'signal':13s}{'IC':>8s}{'t':>6s}{'partial|{lvl,ivrv}':>20s}{'t':>6s}"
          f"{'pos':>6s}{'Q5-Q1pp':>9s}{'topD %/mo':>11s}{'topD Sh':>9s}")
    print("-" * 88)
    for c in cands:
        col = ren.get(c, c)
        r = score(t, col)
        rows.append(dict(signal=c, **r))
        star = " <" if (c not in ("volOfVol", "volOfIvol") and r["pt"] > 2.0) else ""
        print(f"{c:13s}{r['ic']:>+8.3f}{r['t']:>6.1f}{r['pic']:>15.3f}{r['pt']:>11.1f}"
              f"{100*r['pos']:>5.0f}%{r['q5q1']:>+9.2f}{r['top_mean']:>+11.2f}{r['top_sh']:>9.2f}{star}")

    res = pd.DataFrame(rows)
    print("\n  IC/partial vs the REAL trade r_mid. partial controls {IV level, IV/RV}: a vov that")
    print("  survives it adds orthogonal timing info. '<' = partial t>2 (beats the bar volOfVol set).")
    print(f"\n  baseline volOfVol partial t = {res[res.signal=='volOfVol'].pt.values[0]:.1f}")

    # ---- blend swap: does the best fast candidate improve the tradeable book? ----
    best = res[~res.signal.isin(["volOfVol", "volOfIvol"])].sort_values("pt", ascending=False)
    print("\n=== blend swap (3-signal blend, vov leg replaced) ===")
    print(f"{'vov leg':13s}{'gross top20 %/mo':>18s}{'gross Sh':>10s}{'net top50 %/mo':>16s}{'net Sh':>9s}")
    print("-" * 66)
    t2 = t.rename(columns={"volOfVol": "f_vov"})
    for leg in ["f_vov"] + list(best.signal.head(4)):
        col = leg if leg in t2.columns else leg
        b = blend_perf(t2, col)
        tag = "  (ORATS baseline)" if leg == "f_vov" else ""
        print(f"{leg:13s}{b['gross_mean']:>+18.2f}{b['gross_sh']:>10.2f}"
              f"{b['net_mean']:>+16.2f}{b['net_sh']:>9.2f}{tag}")

    res.to_csv(f"{BASE}/out/fast_vov_scores.csv", index=False)
    print(f"\nwrote out/fast_vov_scores.csv")


if __name__ == "__main__":
    main()
