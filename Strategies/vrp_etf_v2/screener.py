#!/usr/bin/env python3
"""
VRP short-straddle SCREENER (vrp_etf_v2) — the single daily tool.

Reads the latest ORATS core EOD file and ranks liquid ETFs as short 30d ATM
delta-hedged straddle candidates, exactly as the study defined the tradeable book:

  blend = mean of three cross-sectional percentile ranks, high = stronger SELL
     q_ivrv  = rank( log(iv30d / clsHv20d) )        IV rich vs the name's own RV
     q_vov   = rank( volOfVol )                      vol-of-vol elevated
     q_ffr   = 1 − rank( flat/std 30-60d fwd ratio ) front rich vs forward (backwardation)
  (same construction as analysis/real_plots.py prep(); ivPctile1y is NOT in the blend —
   it did not survive on the real trade.)

WHAT THE STUDY SAID (8,213 real delta-hedged straddles, 2013-2024) — baked into flags:
  * The trade is REAL. Gross positive in 12/12 years. Blend top-20% ~ +2.98%/mo on margin,
    Sharpe 2.23. Backwardation is the strongest single sort (+2.34%/mo).
  * NET is LIQUIDITY-BOUND. Break-even fill f* = gross ÷ spread rises with volume and only
    clears 1.0 above ~250k contracts/day. On that liquid set the honest net is ~+0.61%/mo,
    Sharpe ~0.65 (blend top-50%). Below 250k the spread takes all of it — GROSS-ONLY.
    In practice only SPY/QQQ/IWM/EEM carry the net edge.
  * This is SHORT VOL: broad SPY backwardation = elevated tail risk -> size DOWN (banner).
  * Held to expiry, settles at |S−K|; American ITM-call assignment risk not modelled.

Usage:
  python3 screener.py                         # latest date, ETFs, blend rank
  python3 screener.py --date 20260720 --top 40
  python3 screener.py --liquid-only           # only names that clear net (>=250k/day)
  python3 screener.py --min-vol 10000 --sell-pct 0.8
"""
import argparse, glob, os, math
import numpy as np
import pandas as pd

BASE = "/home/marco/trading/Systems/Options/Strategies/vrp_etf_v2"
CORE = "/home/marco/trading/HistoricalData/ORATS/core"
OUT = f"{BASE}/out/screens"

# ORATS assetType: 5/7/8/10 = ETF/ETN/ETP. (source of truth: analysis/common.py)
ETF_TYPES = [5, 7, 8, 10]

# Leveraged/inverse/vol/commodity ETPs — a 2x fund's RV is mechanically ~2x its
# constituents', not the same variance risk premium. Kept verbatim from common.LEVERED.
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

# The study's cost pass put the break-even fill fraction f* above 1 somewhere around
# 130-150k contracts/day (README's coarse buckets said 250k; the finer grid says ~150k, and
# net Sharpe is flat over 150k-400k). It is a NOISY PROXY for the thing that matters, the
# quoted half-spread. So it is NOT a gate here -- it is a parameter, defaulting to the same
# 2.5k as the universe, i.e. off. Pass --net-vol 250000 to restore the old behaviour.
LIQ_DEFAULT = 10000.0
COLS = ["ticker", "tradeDate", "assetType", "pxCls", "iv30d", "iv60d", "clsHv20d",
        "volOfVol", "volOfIvol", "ivPctile1y", "avgOptVolu20d", "straPxM1", "hiStrikeM1",
        "confidence", "sector", "mktCap", "exErnIv30d", "exErnIv60d"]

IVCV_WIN, IVCV_MINP, IVCV_HIST = 42, 21, 60   # iv_cv42 = 42-session std/mean of iv30d


def load_iv_cv42(fpath, tickers):
    """iv_cv42 = trailing 42-session std(iv30d)/mean(iv30d), as of `fpath`'s date.

    A faster, scale-free vol-of-vol than ORATS volOfVol (analysis/fast_vov.py). Needs
    history the single EOD file lacks, so it reads the last IVCV_HIST core files up to and
    including this date. DISPLAY ONLY -- it is NOT in the blend (fast_vov showed swapping it
    in does not improve the net edge; it earns its place as a standalone top-decile sort).
    """
    allf = sorted(glob.glob(f"{CORE}/orats_core_*.csv.gz"))
    hist = [g for g in allf if os.path.basename(g)[11:19] <= os.path.basename(fpath)[11:19]][-IVCV_HIST:]
    parts = []
    for g in hist:
        h = pd.read_csv(g, usecols=["ticker", "tradeDate", "iv30d"], compression="gzip")
        parts.append(h[h.ticker.isin(tickers)])
    s = pd.concat(parts).sort_values(["ticker", "tradeDate"])
    s["iv30d"] = pd.to_numeric(s.iv30d, errors="coerce")
    grp = s.groupby("ticker", sort=False).iv30d
    std = grp.transform(lambda v: v.rolling(IVCV_WIN, min_periods=IVCV_MINP).std())
    mean = grp.transform(lambda v: v.rolling(IVCV_WIN, min_periods=IVCV_MINP).mean())
    s["iv_cv42"] = std / mean.replace(0, np.nan)
    return s.groupby("ticker").iv_cv42.last()    # latest value per ticker


def fwd_ratio(v1, v2, t1=30.0, t2=60.0):
    """flat-forward / standard-forward vol ratio (scale-free; iv in percent is fine)."""
    fstd = np.sqrt(np.clip((v2**2 * t2 - v1**2 * t1) / (t2 - t1), 1e-6, None))
    fflat = (v2 * np.sqrt(t2) - v1 * np.sqrt(t1)) / (np.sqrt(t2) - np.sqrt(t1))
    return np.clip(fflat / fstd, -10, 10)


def nice_strike(s):
    step = 5.0 if s >= 100 else (1.0 if s >= 25 else 0.5)
    return round(round(s / step) * step, 2)


def regime_banner(raw):
    """SPY 30/60 term structure. Broad backwardation = tail risk elevated for a short-vol book."""
    spy = raw[raw.ticker == "SPY"]
    if spy.empty:
        return "  regime: SPY not in core (no banner)."
    r = spy.iloc[0]
    i1, i2 = r.exErnIv30d, r.exErnIv60d
    if not (i1 == i1 and i2 == i2):
        i1, i2 = r.iv30d, r.iv60d
    if not (i1 > 2 and i2 > 2):
        return "  regime: SPY IV unavailable."
    ff = fwd_ratio(i1, i2)
    # ratio<1 = backwardation (front rich). Translate to a plain FF-style read.
    back = i1 > i2
    tag = ("BACKWARDATION — broad tail risk elevated, SIZE DOWN the short book" if (back and i1 - i2 > 0.7)
           else "mild backwardation — watch tails" if back
           else "contango — calm tape, the benign regime for selling vol")
    return (f"  regime: SPY 30d {i1:.1f}% / 60d {i2:.1f}%  fwd-ratio {ff:.2f}  "
            f"[{tag}]")


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--date")
    ap.add_argument("--top", type=int, default=30)
    ap.add_argument("--detail", type=int, default=6)
    ap.add_argument("--min-vol", type=float, default=10000)
    ap.add_argument("--min-px", type=float, default=5.0)
    ap.add_argument("--min-prem", type=float, default=0.30, help="min straddle credit ($)")
    ap.add_argument("--sell-pct", type=float, default=0.80, help="blend pctile for the SELL tier")
    ap.add_argument("--drop-levered", action="store_true",
                    help="exclude leveraged/inverse ETPs (ORATS sector + the study's list). "
                         "OFF by default: the study dropped them because a 2x fund's RV is "
                         "mechanically ~2x its constituents', but they are tradeable.")
    ap.add_argument("--net-vol", type=float, default=LIQ_DEFAULT,
                    help="avgOptVolu20d gate for the actionable book (default: off = same as "
                         "--min-vol; use 250000 for the study's original liquid-only net book)")
    ap.add_argument("--liquid-only", action="store_true",
                    help="restrict the whole universe to names at/above --net-vol")
    ap.add_argument("--no-fast", action="store_true",
                    help="skip the iv_cv42 diagnostic column (avoids reading trailing core files)")
    a = ap.parse_args()

    f = (f"{CORE}/orats_core_{a.date}.csv.gz" if a.date
         else sorted(glob.glob(f"{CORE}/orats_core_*.csv.gz"))[-1])
    date = os.path.basename(f)[11:19]; tdate = pd.Timestamp(date)
    raw = pd.read_csv(f, usecols=lambda c: c in COLS, compression="gzip")
    for c in COLS:
        if c not in ("ticker", "tradeDate", "sector"):
            raw[c] = pd.to_numeric(raw[c], errors="coerce")
    banner = regime_banner(raw)

    # ---- the study's own ETF universe + quality filters (common.load, etf) ----
    # LEVERED is a hardcoded list from the 2013-2024 study and is now STALE: the 2022+ wave of
    # single-stock leveraged ETFs (AMZU, MSFU, CONL, METU, NVDX...) post-dates it entirely, and
    # with no liquidity gate they rank straight into the top of the book. ORATS' own sector field
    # catches the whole family, so use it as the primary test and keep the list as a backstop.
    lev = raw.sector.fillna('').str.contains('Leverag', case=False) | raw.ticker.isin(LEVERED)
    keep = raw.assetType.isin(ETF_TYPES) & (~lev if a.drop_levered else True)
    d = raw[keep].copy()
    n_lev = int((lev & raw.assetType.isin(ETF_TYPES)).sum())
    d = d[(d.avgOptVolu20d >= a.min_vol)
          & d.iv30d.between(5, 250, "neither") & d.iv60d.between(5, 250, "neither")
          & d.clsHv20d.between(2, 250, "neither")
          & (d.pxCls > a.min_px) & (d.straPxM1 > a.min_prem)
          & (d.confidence.fillna(100) >= 30)]
    d = d.dropna(subset=["iv30d", "iv60d", "clsHv20d", "volOfVol", "straPxM1", "pxCls"])
    if a.liquid_only:
        d = d[d.avgOptVolu20d >= a.net_vol]

    if len(d) < 8:
        print(banner)
        print(f"\n  only {len(d)} ETFs pass filters on {date} — too thin to rank a cross-section.\n")
        return

    # ---- the three signals + blend, cross-sectional pct-ranks over TODAY's universe ----
    d["f_ivrv"] = np.log(d.iv30d / d.clsHv20d)
    d["f_ffr"] = fwd_ratio(d.iv30d.values, d.iv60d.values)
    d["q_ivrv"] = d.f_ivrv.rank(pct=True)
    d["q_vov"] = d.volOfVol.rank(pct=True)
    d["q_ffr"] = 1 - d.f_ffr.rank(pct=True)                 # low ratio = backwardation
    d["blend"] = (d.q_ivrv + d.q_vov + d.q_ffr) / 3
    d["liquid"] = d.avgOptVolu20d >= a.net_vol

    # iv_cv42 — faster scale-free vol-of-vol, DIAGNOSTIC ONLY (not in the blend above)
    d["iv_cv42"], d["q_ivcv"] = np.nan, np.nan
    if not a.no_fast:
        try:
            cv = load_iv_cv42(f, set(d.ticker))
            d["iv_cv42"] = d.ticker.map(cv)
            d["q_ivcv"] = d.iv_cv42.rank(pct=True)     # cross-sectional rank, for reading only
        except Exception as e:
            print(f"  (iv_cv42 skipped: {e})")

    # NET is only defined on the liquid set, so the ACTIONABLE book re-ranks WITHIN it
    # (cost_fig panel B: L["q"] = ranks recomputed on the liquid subset). blend_liq is the
    # number the "net, blend top 50%" verdict was built on; blend above is gross context.
    liq = d[d.liquid]
    if len(liq) >= 4:
        bl = ((liq.f_ivrv.rank(pct=True) + liq.volOfVol.rank(pct=True)
               + (1 - liq.f_ffr.rank(pct=True))) / 3)
        d.loc[bl.index, "blend_liq"] = bl
    else:
        d["blend_liq"] = np.nan

    d["strike"] = d.pxCls.map(nice_strike)
    d["credit"] = d.straPxM1                                  # ATM straddle mid (validated = NBBO mid)
    d["margin"] = 0.20 * d.pxCls + d.straPxM1
    d["credit_pctM"] = 100 * d.credit / d.margin
    d["breakeven_pct"] = 100 * d.straPxM1 / d.pxCls          # ± move to zero P&L at expiry

    gated = a.net_vol > a.min_vol
    def flags(r):
        fl = []
        if gated:
            fl.append("LIQUID(net-ok)" if r.liquid else "GROSS-ONLY")
        elif r.avgOptVolu20d < 1e5:
            # not a gate — just the measured fact: below ~100k/day the half-spread ran
            # 5-10% of premium and the net book was Sharpe-negative in the backtest.
            fl.append("SPREAD-RISK")
        if r.liquid and r.blend_liq >= 0.50: fl.append("SELL-NET")   # top half of the book
        if r.blend >= a.sell_pct: fl.append("SELL")
        if r.q_ffr >= 0.90: fl.append("BACKWARD")
        if r.q_ivrv >= 0.90: fl.append("RICH")
        if r.q_vov >= 0.90: fl.append("VOV")
        if r.avgOptVolu20d < 1e4: fl.append("THIN")
        return ",".join(fl)

    d["flags"] = d.apply(flags, axis=1)
    d["exp30"] = str((tdate + pd.Timedelta(days=30)).date())
    out = d.sort_values("blend", ascending=False).reset_index(drop=True)

    # ------------------------------------------------------------------ print
    print(f"\n{'='*84}\n  VRP SHORT-STRADDLE SCREENER (v2) — {date}\n{'='*84}")
    print(banner)
    nliq = int(d.liquid.sum())
    print(f"  universe: {len(d)} ETFs (optVol>={int(a.min_vol)}, levered {'EXCLUDED' if a.drop_levered else f'INCLUDED ({n_lev})'}) | "
          f"{nliq} at/above --net-vol ({a.net_vol/1e3:.1f}k)"
          f"{'  [--liquid-only]' if a.liquid_only else ''}")
    print(f"  SELL tier = blend >= {a.sell_pct:.0%} pctile. TRADE = SELL ~30d ATM straddle "
          f"(call+put @ strike~spot), delta-hedge daily, hold to expiry.")
    print(f"  blend = mean rank(IV/RV, volOfVol, backwardation).  q_ivcv = iv_cv42 rank "
          f"(faster vov) — DIAGNOSTIC, NOT in the blend.")
    print(f"  {'-'*80}")
    cols = ["ticker", "blend", "q_ivrv", "q_vov", "q_ffr", "q_ivcv", "spot", "strike",
            "credit", "credit_pctM", "breakeven_pct", "optVol_k", "flags"]
    show = out.head(a.top).assign(
        blend=out.blend.round(2), q_ivrv=out.q_ivrv.round(2), q_vov=out.q_vov.round(2),
        q_ffr=out.q_ffr.round(2), q_ivcv=out.q_ivcv.round(2), spot=out.pxCls.round(2),
        strike=out.strike, credit=out.credit.round(2), credit_pctM=out.credit_pctM.round(1),
        breakeven_pct=out.breakeven_pct.round(1),
        optVol_k=(out.avgOptVolu20d / 1e3).round(0).astype(int))
    print(show[cols].to_string(index=False))

    os.makedirs(OUT, exist_ok=True)
    csv = f"{OUT}/screen_{date}.csv"
    out.to_csv(csv, index=False)

    # ---- detail: the ACTIONABLE net book — liquid names, re-ranked within the liquid set,
    #      top-50% (blend_liq >= 0.5). This is the only book with a positive net verdict. ----
    tradeable = out[out.liquid & (out.blend_liq >= 0.50)].sort_values(
        "blend_liq", ascending=False).head(a.detail)
    gate = (f"names >={a.net_vol/1e3:.0f}k/day, " if a.net_vol > a.min_vol else "full universe, ")
    print(f"\n{'-'*84}\n  DETAIL — the actionable book: {gate}top-50% re-ranked within that set\n{'-'*84}")
    if tradeable.empty:
        print("  nothing is in the top half of the cross-section today — no entry.")
    else:
        for _, x in tradeable.iterrows():
            print(f"\n  {x.ticker} [{x.sector}]  blend_liq={x.blend_liq:.2f} (full-univ blend {x.blend:.2f})  flags: {x['flags']}")
            print(f"    SELL ~30d ATM straddle  {x.strike}C + {x.strike}P  (exp ~{x.exp30}, spot {x.pxCls:.2f})")
            print(f"    credit ~${x.credit:.2f}  |  margin ~${x.margin:.2f} (0.20·spot+credit)  |  "
                  f"credit {x.credit_pctM:.1f}% of margin")
            print(f"    breakeven at expiry: spot ±{x.breakeven_pct:.1f}%   |   optVol {int(x.avgOptVolu20d):,}/day")
            ivcv = f"{x.q_ivcv:.2f}" if x.q_ivcv == x.q_ivcv else "n/a"
            print(f"    signal: IV/RV rank {x.q_ivrv:.2f}  volOfVol rank {x.q_vov:.2f}  "
                  f"backwardation rank {x.q_ffr:.2f}  [iv_cv42 rank {ivcv}, diag]   "
                  f"(iv30 {x.iv30d:.1f}% vs RV20 {x.clsHv20d:.1f}%)")

    print(f"\n  full {len(out)}-row list -> {csv}")
    print("  ── how to trade it (from the vrp_etf_v2 study) ──")
    print("  • COST, measured: f* = gross/cost crosses 1 near ~150k contracts/day. Net Sharpe ~0.4")
    print("    over the 150k-400k range (blend top-50% there ~ +0.6%/mo, Sharpe ~0.65); it is NEGATIVE")
    print("    below ~100k, where the half-spread runs 5-10% of premium. optVol_k is shown per name —")
    print("    judge each on its own quoted spread, not on a threshold. --net-vol re-imposes a gate.")
    print("  • This is an EXECUTION game near the margin — fill at/inside mid; a full-spread cross loses.")
    print("  • SHORT VOL: watch the regime banner. Broad SPY backwardation = tail risk -> SIZE DOWN.")
    print("  • Held to expiry, settles at |S−K|. American ITM-call assignment (SPY/QQQ/IWM/EEM pay divs)")
    print("    is NOT modelled and would reduce net further — roll before ex-div if short the ITM leg.\n")


if __name__ == "__main__":
    main()
