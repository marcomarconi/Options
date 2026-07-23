"""
Entry-signal screen for the LAG-2 book: candidate predictors built from raw
ORATS core day-files (permitted; see build_signal_panel), measured at the ENTRY
close, vs ret_spot_mid.

!! LOOKAHEAD POSTMORTEM (2026-07-12): the first pass of this screen crowned
logVRP (IC -0.145, tilt +43% pnl). RETRACTED — Options.R computes
   logVRP     = log(iv30d / lead(orHv20d, 20))      <- FUTURE realized vol
   logVRPXern = log(exErnIv30d / lead(orHvXern20d, 20))
i.e. both compare today's IV to the vol subsequently realized over a window
CONTAINING our holding period and the earnings jump. They are labels, not
features (straPro*/straRet*/pxAtmIvM1/M2 likewise forward). The honest
point-in-time version log(iv30d / trailing orHv20d) has IC -0.001 = DEAD.
Field semantics in this extract: pxAtmIv = the STOCK PRICE (not an IV);
retAtmIv = clipped daily STOCK return.

Protocol (multiple-testing honest):
  - pooled Spearman IC (n≈6k -> 2σ ≈ 0.013; with ~26 tests demand
    |IC| ≳ 0.04 AND same sign in both halves)
  - within-ticker IC (signal & return ranked inside each ticker) to kill
    name fixed effects on level-type signals
  - Q5-Q1 quintile spread with t
  - split-half ICs: exits 2013-2018 vs 2019-2024
Run from analysis/:  python3 lag2_signals.py
"""
import glob
import os
from multiprocessing import Pool

import numpy as np
import pandas as pd
from scipy import stats

R = "ret_spot_mid"
# PERMITTED-DATA fix (2026-07-23): was pd.read_parquet(ORATS_core.pq) — Marco's R rebuild,
# not a permitted input. Rebuilt from raw ORATS core day-files. Two fields dropped because
# they are NOT raw columns: retAtmIv (a clipped daily stock return) and logVRPXern; and
# logVRP (log(iv30d / FUTURE realized orHv20d)) is recomputed here from raw iv30d/orHv20d so
# the deep_dive lookahead-anatomy plot still works. logVRP/logVRPXern were already excluded
# from SIGS as forward-looking, so the honest screen is unaffected.
CORE_DIR = os.path.expanduser("~/trading/HistoricalData/ORATS/core")
PANEL = "../cache/lag2_signal_panel.parquet"

# raw-core columns the screen needs (all verified present in the raw core header)
CORE_FIELDS = ["pxAtmIv", "mktCap", "beta1y", "correlSpy1y", "ivHvXernRatio", "ivEtfRatio",
               "iv10d", "iv30d", "iv90d", "exErnIv30d", "volOfIvol", "orHv20d",
               "stkPxChng1wk", "stkPxChng1m", "stkPxChng6m", "avgOptVolu20d",
               "slope", "contango", "deriv", "fexErn60_30", "ffexErn60_30"]
FIELDS = CORE_FIELDS + ["logVRP"]        # logVRP recomputed below (forward; teaching plot only)
FWD = 20                                  # sessions: logVRP compares iv30d to realized 20d ahead


def t(r):
    r = np.asarray(r, float)
    return r.mean() / r.std() * np.sqrt(len(r))


def _one_core(args):
    path, tickers = args
    try:
        df = pd.read_csv(path, usecols=lambda c: c in (["ticker", "tradeDate"] + CORE_FIELDS))
    except Exception:
        return None
    df = df[df.ticker.isin(tickers)]
    return df if len(df) else None


def build_signal_panel(tickers):
    """Point-in-time signal panel from RAW core day-files (permitted). Cached.
    logVRP = log(iv30d / orHv20d shifted 20 sessions FORWARD) — deliberately the
    forward-looking version, reproduced from raw columns solely so deep_dive can show
    the lookahead it is warning against."""
    if os.path.exists(PANEL):
        return pd.read_parquet(PANEL)
    tickers = set(tickers)
    files = sorted(f for f in glob.glob(f"{CORE_DIR}/orats_core_*.csv.gz")
                   if 2013 <= int(os.path.basename(f)[11:15]) <= 2024)
    with Pool(10) as p:
        parts = [d for d in p.map(_one_core, [(f, tickers) for f in files], chunksize=20)
                 if d is not None]
    panel = pd.concat(parts, ignore_index=True)
    for c in CORE_FIELDS:
        panel[c] = pd.to_numeric(panel[c], errors="coerce")
    panel["tradeDate"] = pd.to_datetime(panel.tradeDate)
    panel = panel.sort_values(["ticker", "tradeDate"]).reset_index(drop=True)
    fwd_rlz = panel.groupby("ticker", sort=False).orHv20d.shift(-FWD)
    panel["logVRP"] = np.log(panel.iv30d / fwd_rlz.replace(0, np.nan))
    panel.to_parquet(PANEL)
    return panel


def build():
    ls = pd.read_parquet("../cache/trades_prern_lagscan.parquet")
    d = ls[ls.lag == 2].copy()
    core = build_signal_panel(d.ticker.unique())[["ticker", "tradeDate"] + FIELDS]
    d = d.merge(core.rename(columns={"tradeDate": "entry"}),
                on=["ticker", "entry"], how="left")

    # derived signals
    d["evt_iv_prem"] = d.iv30d - d.exErnIv30d      # event premium in IV pts
    d["evt_iv_ratio"] = d.iv30d / d.exErnIv30d.replace(0, np.nan)
    d["iv10_iv90"] = d.iv10d / d.iv90d.replace(0, np.nan)
    d["debit_frac"] = d.debit_mid / d.spot          # straddle cost /spot
    d["log_mktcap"] = np.log(d.mktCap.replace(0, np.nan))
    d["log_optvol"] = np.log1p(d.avgOptVolu20d)
    # HONEST point-in-time VRPs (trailing realized; the extract's logVRP*
    # use FUTURE realized — see module docstring)
    d["vrp_trail"] = np.log(d.iv30d / d.orHv20d.replace(0, np.nan))
    d["vrp_xern"] = np.log(d.ivHvXernRatio.replace(0, np.nan))
    return d


SIGS = ["evt_iv_prem", "evt_iv_ratio", "iv10_iv90", "debit_frac",
        "impErnMv", "vrp_trail", "vrp_xern", "ivHvXernRatio", "ivEtfRatio",
        "pxAtmIv", "iv30d", "volOfIvol", "orHv20d",
        "stkPxChng1wk", "stkPxChng1m", "stkPxChng6m",
        "log_mktcap", "beta1y", "correlSpy1y", "log_optvol",
        "slope", "contango", "deriv", "fexErn60_30", "ffexErn60_30"]
# excluded as FORWARD-looking: logVRP, logVRPXern (see docstring).
# dropped in the permitted-data rebuild: retAtmIv (not a raw-core column).


def screen(d):
    rows = []
    half = d.exit.dt.year >= 2019
    for sig in SIGS:
        s = d.dropna(subset=[sig, R])
        s = s[np.isfinite(s[sig])]
        if len(s) < 2000:
            rows.append((sig, len(s), np.nan, np.nan, np.nan, np.nan,
                         np.nan, np.nan))
            continue
        ic = stats.spearmanr(s[sig], s[R])[0]
        # within-ticker IC
        gs = s.groupby("ticker")[sig].rank(pct=True)
        gr = s.groupby("ticker")[R].rank(pct=True)
        ic_w = stats.spearmanr(gs, gr)[0]
        q = pd.qcut(s[sig].rank(method="first"), 5, labels=False)
        q1, q5 = s.loc[q == 0, R], s.loc[q == 4, R]
        spread = (q5.mean() - q1.mean()) * 100
        tsp = ((q5.mean() - q1.mean())
               / np.sqrt(q5.var() / len(q5) + q1.var() / len(q1)))
        h1 = s[~half.reindex(s.index, fill_value=False)]
        h2 = s[half.reindex(s.index, fill_value=False)]
        ic1 = stats.spearmanr(h1[sig], h1[R])[0] if len(h1) > 500 else np.nan
        ic2 = stats.spearmanr(h2[sig], h2[R])[0] if len(h2) > 500 else np.nan
        rows.append((sig, len(s), ic, ic_w, spread, tsp, ic1, ic2))
    out = pd.DataFrame(rows, columns=["signal", "n", "IC", "IC_within",
                                      "Q5-Q1%", "t_spread", "IC_13-18",
                                      "IC_19-24"])
    return out.sort_values("IC", key=abs, ascending=False)


def deep_dive(d):
    """Post-retraction deep dive. Left: anatomy of the lookahead — the
    forward-computed logVRP vs the honest trailing vrp_trail, same
    quintile cut. Right: the best CLEAN signal (1m momentum). Prints tilt
    tests (none survive the Sharpe test). Writes fig 15."""
    import os
    import matplotlib
    matplotlib.use("Agg")
    import matplotlib.pyplot as plt

    BLUE, RED, GRAY = "#4878cf", "#d1615d", "#aaaaaa"
    s = d.sort_values("entry").copy()

    def msh(w):
        x = s.assign(wp=s[R] * w)
        idx = pd.date_range("2013-01-31", "2024-12-31", freq="ME")
        m = x.set_index("exit").wp.resample("ME").sum().reindex(
            idx, fill_value=0.0)
        return m.mean() / m.std() * np.sqrt(12), x.wp.sum() * 100

    def qmeans(sig):
        v = s.dropna(subset=[sig])
        v = v[np.isfinite(v[sig])]
        q = pd.qcut(v[sig].rank(method="first"), 5, labels=False)
        return [(v.loc[q == k, R].mean() * 100, t(v.loc[q == k, R]))
                for k in range(5)]

    print("\n== tilt tests on CLEAN signals (1.5x/0.5x, expanding median) ==")
    sh, tot = msh(np.ones(len(s)))
    print(f"  ungated                Sharpe {sh:.2f}  pnl {tot:+.0f}%")
    for sig in ["vrp_trail", "vrp_xern", "stkPxChng1m"]:
        pct = s[sig].expanding(min_periods=250).apply(
            lambda v: (v[:-1] < v[-1]).mean(), raw=True)
        w = np.where(pct.isna(), 1.0, np.where(pct < 0.5, 1.5, 0.5))
        sh, tot = msh(w)
        print(f"  tilt {sig:16s}  Sharpe {sh:.2f}  pnl {tot:+.0f}%")
    print("  -> none keep Sharpe; lag-2 stays ungated, unweighted")

    os.makedirs("../figures", exist_ok=True)
    fig, (ax1, ax2) = plt.subplots(1, 2, figsize=(12, 4.5))
    fake, honest = qmeans("logVRP"), qmeans("vrp_trail")
    x = np.arange(5)
    ax1.bar(x - 0.2, [m for m, _ in fake], width=0.4, color=GRAY,
            label="logVRP (IV / FUTURE realized) — lookahead")
    ax1.bar(x + 0.2, [m for m, _ in honest], width=0.4, color=BLUE,
            label="log(IV / trailing realized) — honest")
    ax1.set_xticks(x, [f"Q{k+1}" for k in range(5)])
    ax1.axhline(0, color="k", lw=0.7)
    ax1.set_ylabel("mean %/spot (gross mid)")
    ax1.set_title("anatomy of a lookahead: same signal,\n"
                  "future vs trailing realized vol", fontsize=10.5)
    ax1.legend(fontsize=8)

    mom = qmeans("stkPxChng1m")
    ax2.bar(x, [m for m, _ in mom],
            color=[BLUE if m >= 0 else RED for m, _ in mom])
    for k, (m, tt) in enumerate(mom):
        ax2.text(k, m, f"t={tt:.1f}", ha="center",
                 va="bottom" if m >= 0 else "top", fontsize=8,
                 color="#52514e")
    ax2.set_xticks(x, [f"Q{k+1}" for k in range(5)])
    ax2.axhline(0, color="k", lw=0.7)
    ax2.set_ylabel("mean %/spot (gross mid)")
    ax2.set_title("best CLEAN signal: 1-month stock return\n"
                  "(Q1 = biggest losers) — real but tilt costs Sharpe",
                  fontsize=10.5)
    fig.tight_layout()
    fig.savefig("../figures/15_logvrp.png", dpi=110)
    print("wrote 15_logvrp.png to ../figures/")


def main():
    d = build()
    print(f"lag-2 book with core fields: {len(d)} trades, "
          f"core match {d.iv30d.notna().mean()*100:.0f}%")
    out = screen(d)
    pd.set_option("display.width", 140)
    print(out.round(3).to_string(index=False))
    print("\nbar: |IC| >= 0.04 with same sign in both halves "
          "(~Bonferroni for 26 tests at n≈6k)")
    deep_dive(d)


if __name__ == "__main__":
    main()
