"""
Vol-regime classifier replicating the Sharpe Two "Regime Timeline" charts.

Pipeline:
  1. Per-ticker daily features from ORATS core data (30DTE tenor by default):
     iv_pct, rv_pct, vrp = log(iv/rv), iv_off_peak, iv_mom, stress_recency.
  2. Diagonal-covariance Gaussian mixture (EM, k-means++ init) fit on the
     pooled, standardized feature panel across a basket of liquid ETFs.
  3. Clusters auto-labeled from their centroids into named regimes
     (Low RV, Positive VRP, Calm, Post Stress, Stress, Extreme Stress, Vol Crush).

Usage:
  python regime_model.py            # fit + save labeled panel to results/
  Import fit_regimes() from plotting / screener code.
"""
import numpy as np
import pandas as pd
from scipy.cluster.vq import kmeans2

ORATS_CORE = "/home/marco/trading/HistoricalData/ORATS/ORATS_core.pq"

# liquid, structurally different ETFs so the mixture sees equity, rates,
# metals, energy and EM vol cycles
BASKET = ["SPY", "QQQ", "IWM", "DIA", "GLD", "TLT", "USO",
          "XLE", "EEM", "FXI", "SLV", "HYG", "EFA", "XLF", "SMH"]

FEATS = ["iv_pct", "rv_pct", "vrp", "iv_off_peak", "iv_mom", "stress_recency"]

# tenor -> (implied vol column, realized vol column)
TENORS = {"30dte": ("iv30d", "orHv20d"), "9dte": ("iv10d", "orHv5d")}


# ---------------------------------------------------------------- features
def load_features(tenor="30dte", tickers=BASKET):
    iv_col, rv_col = TENORS[tenor]
    cols = ["ticker", "tradeDate", iv_col, rv_col, "iv30d", "volOfIvol"]
    df = pd.read_parquet(ORATS_CORE, columns=list(dict.fromkeys(cols)),
                         filters=[("ticker", "in", list(tickers))])
    df["tradeDate"] = pd.to_datetime(df["tradeDate"])
    df = df.sort_values(["ticker", "tradeDate"]).reset_index(drop=True)

    def per_ticker(g):
        g = g.copy()
        liv = np.log(g[iv_col].replace(0, np.nan))
        lrv = np.log(g[rv_col].replace(0, np.nan))
        g["iv"] = g[iv_col]
        g["vrp"] = liv - lrv
        # percentile ranks over a trailing 2y window: keeps features
        # comparable across tickers with very different vol levels
        g["iv_pct"] = liv.rolling(504, min_periods=252).rank(pct=True)
        g["rv_pct"] = lrv.rolling(504, min_periods=252).rank(pct=True)
        # distance from the trailing 1m IV peak (0 = at the peak);
        # very negative = IV just collapsed (vol crush)
        g["iv_off_peak"] = liv - liv.rolling(21, min_periods=10).max()
        g["iv_mom"] = liv.diff(10)
        # was there a stress episode in the last 3 months?
        g["stress_recency"] = g["iv_pct"].rolling(63, min_periods=21).max()
        return g

    df = df.groupby("ticker", group_keys=False).apply(per_ticker)
    return df.dropna(subset=FEATS).reset_index(drop=True)


# ---------------------------------------------------------------- GMM (EM)
def gmm_fit(X, k, seed=42, iters=300, tol=1e-6):
    n, p = X.shape
    mu, lab = kmeans2(X, k, minit="++", seed=seed)
    var = np.array([X[lab == j].var(axis=0) + 1e-4 for j in range(k)])
    w = np.array([(lab == j).mean() for j in range(k)])
    ll_old = -np.inf
    for _ in range(iters):
        logp = _log_resp(X, mu, var, w)
        m = logp.max(axis=1, keepdims=True)
        lse = m[:, 0] + np.log(np.exp(logp - m).sum(axis=1))
        r = np.exp(logp - lse[:, None])
        ll = lse.mean()
        nk = r.sum(axis=0) + 1e-10
        w = nk / n
        mu = (r.T @ X) / nk[:, None]
        var = (r.T @ X ** 2) / nk[:, None] - mu ** 2 + 1e-4
        if ll - ll_old < tol:
            break
        ll_old = ll
    return dict(mu=mu, var=var, w=w, ll=ll)


def _log_resp(X, mu, var, w):
    p = X.shape[1]
    logp = -0.5 * (((X[:, None, :] - mu[None]) ** 2 / var[None]).sum(-1)
                   + np.log(var).sum(-1)[None] + p * np.log(2 * np.pi))
    return logp + np.log(w)[None]


def gmm_predict(model, X):
    return _log_resp(X, model["mu"], model["var"], model["w"]).argmax(axis=1)


# ---------------------------------------------------------------- labeling
def label_centroids(cent):
    """cent: DataFrame of cluster centroids in original feature units."""
    labels = {}
    for j, r in cent.iterrows():
        if r.iv_off_peak < -1.0:
            lab = "Vol Crush"
        elif r.iv_pct > 0.85 and r.rv_pct > 0.85:
            lab = "High Stress"
        elif r.iv_pct > 0.5 and (r.vrp < 0.05 or r.iv_mom < -0.02):
            lab = "Post Stress"
        elif r.iv_pct > 0.6:
            lab = "Stress"
        elif r.iv_pct < 0.15 and r.rv_pct < 0.15:
            lab = "Low RV"
        elif r.vrp > 0.15:
            lab = "Positive VRP"
        else:
            lab = "Calm"
        labels[j] = lab
    return labels


def fit_regimes(tenor="30dte", k=7, seed=42, verbose=True):
    d = load_features(tenor)
    X = d[FEATS].to_numpy()
    mean, sd = X.mean(0), X.std(0)
    model = gmm_fit((X - mean) / sd, k, seed=seed)
    d["cluster"] = gmm_predict(model, (X - mean) / sd)
    cent = pd.DataFrame(model["mu"] * sd + mean, columns=FEATS)
    labels = label_centroids(cent)
    d["regime"] = d["cluster"].map(labels)
    # day-level refinement: within the stress-family regimes, reserve
    # "Extreme Stress" for genuine tail days (top ~1-3% of the 2y IV range
    # with realized vol confirming)
    extreme = d.regime.isin(["Stress", "High Stress", "Post Stress"]) & (
        ((d.iv_pct > 0.99) & (d.rv_pct > 0.80))
        | ((d.iv_pct > 0.97) & (d.rv_pct > 0.90)))
    d.loc[extreme, "regime"] = "Extreme Stress"
    if verbose:
        cent["share%"] = np.round(100 * model["w"], 1)
        cent["label"] = pd.Series(labels)
        print(cent.round(3))
    return d, model, (mean, sd), labels


if __name__ == "__main__":
    import os
    here = os.path.dirname(os.path.abspath(__file__))
    out = os.path.join(here, "..", "results")
    os.makedirs(out, exist_ok=True)
    for tenor in ["30dte", "9dte"]:
        print(f"\n================ {tenor} ================")
        d, *_ = fit_regimes(tenor)
        d.to_parquet(os.path.join(out, f"regimes_{tenor}.pq"))
        print(d.groupby("regime")["iv"].agg(["count", "median"]).round(2))
