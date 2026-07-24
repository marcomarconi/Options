#!/usr/bin/env python3
"""
Shared universe, features, targets and IC machinery for the VRP retest.

Self-contained on purpose: nothing here imports from sharpe_two or vrp_etf_model, both of
which sit on `s2_dataset.pq`. The only inputs are cache/panel.parquet (built from ORATS
core day-files) and the ticker list below.
"""
import numpy as np
import pandas as pd
from scipy import stats as sps

BASE = "/home/marco/trading/Systems/Options/Strategies/vrp_etf_v2"
PANEL = f"{BASE}/cache/panel.parquet"

# ORATS assetType: 0/1/2/3 = single-name stocks, 5/7/8/10 = ETF/ETN/ETP,
# 4/9 = cash index, 6 = VIX, 11 = misc.
ETF_TYPES = [5, 7, 8, 10]
STOCK_TYPES = [0, 1, 2, 3]

# Leveraged / inverse / vol / commodity ETPs. A 2x fund's straddle is not a bet on the
# same variance risk premium -- its RV is mechanically ~2x its constituents' -- and the
# old study found ~1/3 of the raw ETF edge came from exactly these names.
LEVERED = {
    'TQQQ','SQQQ','UPRO','SPXU','SPXS','SPXL','SDS','SSO','SH','UDOW','SDOW','DDM','DXD',
    'TNA','TZA','URTY','SRTY','QID','QLD','FAS','FAZ','LABU','LABD','SOXL','SOXS','TECL','TECS',
    'YINN','YANG','BRZU','EDC','EDZ','DPST','CURE','RXL','BIB','BIS','UWM','TWM','MIDU',
    'AGQ','ZSL','BOIL','KOLD','UNG','UCO','SCO','UGL','GLL','NUGT','DUST','GUSH','DRIP','ERX','ERY',
    'BNO','UNL','UGA','DGAZ','UGAZ','OILU','OILD',
    'UVXY','SVXY','VIXY','VXX','VXZ','UVIX','SVIX',
    'BITX','BITU','BITI','CONY','AMDL','AMDY','NVDL','TSLL','MSTU','MSTX','AMDU',
    'RSX','ETHU','XELA','MNTV',
}

OVERLAP = 21          # sessions of overlap in the target -> deflate t-stats by sqrt(21)
DEFLATE = np.sqrt(OVERLAP)


def load(kind="etf", minvol=2500.0, drop_levered=True):
    """The study's own filters, restated on the clean panel."""
    d = pd.read_parquet(PANEL)
    types = ETF_TYPES if kind == "etf" else STOCK_TYPES
    d = d[d.assetType.isin(types) & (d.avgOptVolu20d >= minvol)]
    if kind == "etf" and drop_levered:
        d = d[~d.ticker.isin(LEVERED)]
    ok = lambda c, lo, hi: (d[c] > lo) & (d[c] < hi)
    d = d[ok("iv30d", 5, 250) & ok("iv60d", 5, 250) & ok("clsHv20d", 2, 250)
          & ok("fwd_rv", 2, 250) & (d.pxCls > 1) & (d.straPxM1 > 0)
          & (d.confidence.fillna(100) >= 30)]
    d = d.dropna(subset=["iv30d", "iv60d", "clsHv20d", "ivPctile1y", "fwd_rv",
                         "straPxM1", "pxCls"]).copy()
    return feats(targets(d)).replace([np.inf, -np.inf], np.nan).dropna(
        subset=["f_ivrv", "f_ivpct", "f_ffr", "y_risk", "y_margin"])


def targets(d):
    """The video's own metric, kept verbatim so the comparison is like-for-like.

    y_risk is the variance-swap approximation of a continuously delta-hedged short
    straddle: P&L/premium ~ 1 - (RV/IV)^2. It is an APPROXIMATION, not a traded P&L --
    it assumes a constant dollar-gamma and a costless daily rehedge. `real_pnl.py`
    replaces it with actual quotes; keep that distinction in mind reading any IC here.
    """
    ppp = 1.0 - (d.fwd_rv / d.iv30d) ** 2
    d["y_risk"] = ppp
    d["y_margin"] = (0.25 * d.straPxM1 * ppp) / (0.20 * d.pxCls + d.straPxM1)
    return d


def feats(d):
    d["f_ivrv"] = np.log(d.iv30d / d.clsHv20d)          # signal 1
    d["f_ivpct"] = d.ivPctile1y / 100.0                  # signal 2
    t1, t2 = 30.0, 60.0
    v1, v2 = d.iv30d, d.iv60d
    fstd = np.sqrt(((v2 ** 2 * t2 - v1 ** 2 * t1) / (t2 - t1)).clip(lower=1e-6))
    fflat = (v2 * np.sqrt(t2) - v1 * np.sqrt(t1)) / (np.sqrt(t2) - np.sqrt(t1))
    d["f_ffr"] = (fflat / fstd).clip(-10, 10)            # signal 3
    d["f_ivlvl"] = d.iv30d                               # the control
    d["f_vov"] = d.volOfVol
    d["f_voiv"] = d.volOfIvol
    return d


def _rank(a):
    return sps.rankdata(a) / (len(a) + 1.0)


def daily_ic(d, sig, y, controls=(), min_n=8):
    """Per-day cross-sectional rank-IC, optionally partialling out controls.

    Cross-sectional and rank-based on purpose: it is immune to the level outliers that
    poison a pooled OLS, and it answers the question a screener actually asks -- among
    today's candidates, does this signal order them?
    """
    out = []
    cols = [sig, y] + list(controls)
    for day, g in d.groupby("tradeDate", sort=True):
        g = g[cols].dropna()
        if len(g) < min_n:
            continue
        x, t = _rank(g[sig].values), _rank(g[y].values)
        if controls:
            Z = np.column_stack([np.ones(len(g))] + [_rank(g[c].values) for c in controls])
            x = x - Z @ np.linalg.lstsq(Z, x, rcond=None)[0]
            t = t - Z @ np.linalg.lstsq(Z, t, rcond=None)[0]
        if np.std(x) < 1e-12 or np.std(t) < 1e-12:
            continue
        out.append((day, np.corrcoef(x, t)[0, 1], len(g)))
    return pd.DataFrame(out, columns=["tradeDate", "ic", "n"])


def ic_summary(ics):
    """t deflated by sqrt(21): the target overlaps 21 sessions, so consecutive daily ICs
    are not independent draws. Undeflated t-stats here run into the tens and mean nothing."""
    ic = ics.ic.values
    n = len(ic)
    t_raw = ic.mean() / (ic.std(ddof=1) / np.sqrt(n)) if n > 1 else np.nan
    return dict(days=n, ic=ic.mean(), t_raw=t_raw, t=t_raw / DEFLATE,
                pos=(ic > 0).mean(), names=ics.n.mean())
