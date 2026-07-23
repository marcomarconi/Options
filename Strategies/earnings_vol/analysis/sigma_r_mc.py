"""
Ex-ante (no-backtest) estimate of sigma_r = per-trade std of pnl/debit for
the pre-earnings straddle, by Monte-Carlo of a forward pricing model whose
inputs are all observable at entry:

  j      implied earnings jump (impErnMv from the surface)
  sig_d  the name's daily diffusive vol (trailing HV20)
  n0     trading days to expiry at entry
  h      holding days (2 for the lag-2 book)

Total implied variance in the straddle = j^2 + sig_d^2 * n0 (event + diffusive);
consistency check: model debit 0.8*sqrt(totvar) should match observed debit/spot.
Over the hold, the event variance carries (its IV ramp is mechanical); risk =
2 days of fat-tailed diffusion (gamma) + re-pricing of the implied jump (vega).

The two non-observable ASSUMPTIONS (dominate the answer, see sensitivity):
  NU     fat-tail df of daily returns (Gaussian -> sigma_r ~0.11, t4 -> ~0.19)
  VOL_J  relative vol of the implied-jump re-mark over the hold (~0.10)

Result with market-consistent inputs (j=6%, sig_d=2%/d, 4 DTE, 2-day hold):
sigma_r = 0.187 vs 0.189 measured on the 6,146 backtest trades; p5 matches too.
Run from analysis/:  python3 sigma_r_mc.py
"""
import numpy as np
from scipy.stats import norm, t as tdist

M = 200_000
j, sig_d, n0, h = 0.06, 0.02, 4, 2
NU, VOL_J = 4, 0.10


def bs_straddle(S, K, v):  # v = total remaining variance (sigma^2 * T)
    sv = np.sqrt(np.maximum(v, 1e-12))
    d1 = (np.log(S / K) + v / 2) / sv
    d2 = d1 - sv
    return S * (2 * norm.cdf(d1) - 1) - K * (2 * norm.cdf(d2) - 1)


def sim(nu, vol_j, seed=1):
    rng = np.random.default_rng(seed)
    S0 = K = 100.0
    P0 = bs_straddle(S0, K, j**2 + sig_d**2 * n0)
    r = tdist.rvs(nu, size=(M, h), random_state=seed) * np.sqrt((nu - 2) / nu) * sig_d
    S1 = S0 * np.exp(r.sum(1) - 0.5 * sig_d**2 * h)
    j1 = j * np.exp(rng.normal(0, vol_j, M))
    return (bs_straddle(S1, K, j1**2 + sig_d**2 * (n0 - h)) - P0) / P0


def main():
    totvar = j**2 + sig_d**2 * n0
    print(f"model-implied debit {0.8*np.sqrt(totvar)*100:.1f}% of spot "
          f"(lag-2 observed median 5.4%)")
    p = sim(NU, VOL_J)
    print(f"sigma_r = {p.std():.3f}  p5 {np.quantile(p,.05):+.2f}  "
          f"med {np.quantile(p,.5):+.2f}  p95 {np.quantile(p,.95):+.2f}  "
          f"(backtest: 0.189 / -0.16 / +0.01 / +0.31)")
    print("\nsensitivity (the assumptions carry the answer):")
    for nu, vj in [(1000, 0.0), (1000, 0.10), (4, 0.0), (4, 0.10),
                   (4, 0.20), (3, 0.10)]:
        print(f"  tails df={nu:>4}, vol-of-jump {vj:.2f}: "
              f"sigma_r = {sim(nu, vj).std():.3f}")


if __name__ == "__main__":
    main()
