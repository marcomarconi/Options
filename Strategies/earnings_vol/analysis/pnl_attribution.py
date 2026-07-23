"""
PnL attribution for both books: is the profit GAMMA (spot movement) or
VEGA (IV ramp into the event)?

Method (exact, no residual): back out the BS implied vol of the straddle
from its observed mid at entry (S0, T0) and at exit (S1, T1), r=q=0.
Walk entry -> exit in three steps; each leg is a repricing difference so
the three sum to the observed mid-to-mid PnL by construction:

    theta = V(S0, iv0, T1) - V(S0, iv0, T0)     time passes, all else fixed
    gamma = V(S1, iv0, T1) - V(S0, iv0, T1)     spot moves at ENTRY vol
    vega  = V(S1, iv1, T1) - V(S1, iv0, T1)     vol re-marks at exit spot

Ordering matters slightly (gamma priced at iv0 vs iv1); we also compute the
reverse order (vega first) and report the average of the two orderings for
gamma/vega — the cross term is small.

Books: A. lag-2 ungated   B. lag-15 + 4-signal walk-forward model (OOS 2016+)
Data: cache/trades_prern_lagscan.parquet only (smv strike mids + smv stk);
no ORATS_core.pq involved. Writes fig 16. Run from analysis/:
    python3 pnl_attribution.py
"""
import os

import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
from scipy.stats import norm

from oq_model import build_model_book, FIRST_TEST_YEAR

R = "ret_spot_mid"
OUT = "../figures"
BLUE, VIOLET, RED, GRAY = "#4878cf", "#4a3aa7", "#d1615d", "#aaaaaa"


def straddle_bs(S, K, T, iv):
    """BS straddle value, r=q=0. Vectorized."""
    S, K, T, iv = (np.asarray(x, float) for x in (S, K, T, iv))
    v = iv * np.sqrt(T)
    with np.errstate(divide="ignore", invalid="ignore"):
        d1 = np.log(S / K) / v + 0.5 * v
    d2 = d1 - v
    call = S * norm.cdf(d1) - K * norm.cdf(d2)
    put = K * norm.cdf(-d2) - S * norm.cdf(-d1)
    return call + put


def implied_vol(price, S, K, T):
    """Bisection IV solve on the straddle (monotone in vol). NaN where the
    mid is below intrinsic (no solution)."""
    price, S, K, T = (np.asarray(x, float) for x in (price, S, K, T))
    lo = np.full_like(price, 1e-4)
    hi = np.full_like(price, 6.0)
    bad = (price <= straddle_bs(S, K, T, lo)) | \
          (price >= straddle_bs(S, K, T, hi)) | (T <= 0)
    for _ in range(60):
        mid = 0.5 * (lo + hi)
        too_low = straddle_bs(S, K, T, mid) < price
        lo = np.where(too_low, mid, lo)
        hi = np.where(too_low, hi, mid)
    iv = 0.5 * (lo + hi)
    iv[bad] = np.nan
    return iv


def attribute(d, lab):
    d = d.copy()
    d["S0"] = d.spot
    d["S1"] = d.spot * (1 + d.spot_ret)
    d["T0"] = (d.expir - d.entry).dt.days / 365.0
    d["T1"] = (d.expir - d.exit).dt.days / 365.0
    d["iv0"] = implied_vol(d.debit_mid, d.S0, d.strike, d.T0)
    d["iv1"] = implied_vol(d.exit_mid, d.S1, d.strike, d.T1)
    ok = d.iv0.notna() & d.iv1.notna()
    print(f"\n== {lab} ==  n={len(d)}, IV solved both ends "
          f"{ok.mean()*100:.1f}% (dropped {(~ok).sum()})")
    d = d[ok].copy()

    # legs at entry vol
    v_S0_iv0_T1 = straddle_bs(d.S0, d.strike, d.T1, d.iv0)
    v_S1_iv0_T1 = straddle_bs(d.S1, d.strike, d.T1, d.iv0)
    # legs at exit vol (for reverse ordering)
    v_S0_iv1_T1 = straddle_bs(d.S0, d.strike, d.T1, d.iv1)

    theta = v_S0_iv0_T1 - d.debit_mid
    gamma_a = v_S1_iv0_T1 - v_S0_iv0_T1          # spot leg at iv0
    vega_a = d.exit_mid - v_S1_iv0_T1            # vol leg at S1
    vega_b = v_S0_iv1_T1 - v_S0_iv0_T1           # vol leg at S0
    gamma_b = d.exit_mid - v_S0_iv1_T1           # spot leg at iv1
    d["theta"] = theta / d.S0
    d["gamma"] = 0.5 * (gamma_a + gamma_b) / d.S0
    d["vega"] = 0.5 * (vega_a + vega_b) / d.S0
    d["total"] = (d.exit_mid - d.debit_mid) / d.S0

    resid = (d.total - d.theta - d.gamma - d.vega).abs().max()
    assert resid < 1e-10, resid
    cross = 0.5 * (gamma_a - gamma_b).abs() / d.S0
    print(f"ordering cross-term: median {cross.median()*100:.3f}%/spot, "
          f"p95 {cross.quantile(.95)*100:.3f}%")

    # theta+vega = straddle re-mark at FIXED spot. For an event straddle a
    # rising IV is largely MECHANICAL (constant event premium spread over
    # fewer calendar days), so this sum — not the raw vega leg — measures
    # whether the IV ramp actually pays for the decay.
    d["fixed_spot"] = d.theta + d.vega
    for c in ["total", "gamma", "vega", "theta", "fixed_spot"]:
        r = d[c]
        print(f"  {c:10s} {r.mean()*100:+.3f}%/spot  "
              f"t={r.mean()/r.std()*np.sqrt(len(r)):+.2f}  "
              f"(median {r.median()*100:+.3f}%)")
    up = d.iv1 > d.iv0
    print(f"  IV rose entry->exit in {up.mean()*100:.0f}% of trades; "
          f"median iv0 {d.iv0.median()*100:.0f}% -> iv1 {d.iv1.median()*100:.0f}%"
          f"  |  median |spot move| {d.spot_ret.abs().median()*100:.2f}%")
    return d


def yearly(d):
    g = d.groupby(d.exit.dt.year)[["gamma", "vega", "theta"]].mean() * 100
    return g


def main():
    os.makedirs(OUT, exist_ok=True)
    ls = pd.read_parquet("../cache/trades_prern_lagscan.parquet")
    lag2 = attribute(ls[ls.lag == 2], "lag 2 ungated")

    model, _ = build_model_book()
    need = ["ticker", "entry", "exit", "expir", "strike", "spot", "spot_ret",
            "debit_mid", "exit_mid", R]
    missing = [c for c in need if c not in model.columns]
    if missing:  # recover trade legs from the lag-15 slice of the cache
        l15 = ls[ls.lag == 15][["ticker", "entry"] +
                               [c for c in need[2:] if c not in model.columns]]
        model = model.merge(l15, on=["ticker", "entry"], how="left")
    model = attribute(model, f"lag 15 + 4-signal model (OOS {FIRST_TEST_YEAR}+)")

    fig, axes = plt.subplots(1, 3, figsize=(14.5, 4.6),
                             gridspec_kw={"width_ratios": [1, 1, 1.35]})
    for ax, d, title, c in [(axes[0], lag2, "lag 2 ungated", BLUE),
                            (axes[1], model,
                             "lag 15 + 4-signal model (OOS 2016+)", VIOLET)]:
        comps = ["total", "gamma", "vega", "theta", "fixed_spot"]
        means = [d[cc].mean() * 100 for cc in comps]
        cols = [c, "#5ba053", "#e49444", RED, GRAY]
        ax.bar(np.arange(5), means, color=cols, width=0.6)
        for xi, m in enumerate(means):
            tt = d[comps[xi]].mean() / d[comps[xi]].std() * np.sqrt(len(d))
            ax.text(xi, m, f"{m:+.2f}\nt={tt:.1f}", ha="center",
                    va="bottom" if m >= 0 else "top", fontsize=8.5,
                    color="#52514e")
        ax.set_xticks(np.arange(5),
                      ["total", "gamma\n(spot move)", "vega\n(IV change)",
                       "theta\n(time)", "theta+vega\n(fixed spot)"],
                      fontsize=8.5)
        lo, hi = min(0, min(means)), max(0, max(means))
        pad = 0.35 * (hi - lo)
        ax.set_ylim(lo - pad, hi + pad)
        ax.axhline(0, color="k", lw=0.7)
        ax.set_ylabel("mean %/spot (gross mid)")
        ax.set_title(f"{title}  n={len(d)}", fontsize=10.5)

    ax = axes[2]
    y2, y15 = yearly(lag2), yearly(model)
    ax.plot(y2.index, y2.gamma, color="#5ba053", lw=1.8, label="lag2 gamma")
    ax.plot(y2.index, y2.vega, color="#e49444", lw=1.8, label="lag2 vega")
    ax.plot(y15.index, y15.gamma, color="#5ba053", lw=1.4, ls="--",
            label="model gamma")
    ax.plot(y15.index, y15.vega, color="#e49444", lw=1.4, ls="--",
            label="model vega")
    ax.axhline(0, color="k", lw=0.7)
    ax.set_ylabel("mean %/spot per trade")
    ax.set_title("gamma vs vega component by exit year", fontsize=10.5)
    ax.legend(fontsize=8, ncol=2)
    fig.suptitle("PnL attribution: theta + gamma (spot move) + vega (IV "
                 "re-mark) = mid-to-mid PnL, per trade", fontsize=12)
    fig.tight_layout(rect=[0, 0, 1, 0.92])
    fig.savefig(f"{OUT}/16_attribution.png", dpi=110)
    print(f"\nwrote 16_attribution.png to {OUT}/")


if __name__ == "__main__":
    main()
