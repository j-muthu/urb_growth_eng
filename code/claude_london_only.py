"""
EXTENDED ANALYSIS: Welfare gains at specific London population levels
Since the two-location model hits a corner solution (optimal = nearly all-London),
compute welfare at realistic intermediate population levels.
"""

import numpy as np

# Parameters
PARAMS_US = {'gamma': 0.07, 'theta': 0.04, 'sigma': 0.04, 'beta': 0.04, 'lambda': 0.18}
PARAMS_UK = {'gamma': 0.05, 'theta': 0.07, 'sigma': 0.025, 'beta': 0.025, 'lambda': 0.072}

N_TOTAL = 56_554_900
N_L_BASE = 9_002_500
N_L_2001 = 7_500_000
Z_L = 1.20
TAU = 1.0

def compute_welfare_at_N(params, N_L_hat, N_L_base, N_total, z_L, tau, omega):
    """Compute all welfare quantities for a given counterfactual London population."""
    g, th, s, b, lam = params['gamma'], params['theta'], params['sigma'], params['beta'], params['lambda']
    sb = s + b; gt = g + th; nt = gt - sb
    
    N_r_base = N_total - N_L_base
    N_r_hat = N_total - N_L_hat
    
    inc_coeff = gt / (sb * (g + 1))
    cons_coeff = nt / (sb * (g + 1))
    
    # Baseline
    c_L_base = cons_coeff * tau * z_L**g * N_L_base**gt
    y_L_base = inc_coeff * tau * z_L**g * N_L_base**gt
    c_r_base = omega * c_L_base
    A_r = c_r_base * N_r_base**lam
    
    # Backed-out A
    K_A = inc_coeff * z_L**g * N_L_base**nt
    K_cost = (1 / (g + 1)) * z_L**g
    
    # Counterfactual
    y_L_hat = K_A * tau * N_L_hat**sb
    c_L_hat = K_A * tau * N_L_hat**sb - K_cost * tau * N_L_hat**gt
    c_r_hat = A_r * max(N_r_hat, 1)**(-lam)
    
    # Aggregates (social planner metric: no permitting DWL)
    TC_L_base = c_L_base * N_L_base
    TC_r_base = A_r * N_r_base**(1 - lam)
    C_base = TC_L_base + TC_r_base
    
    TC_L_hat = c_L_hat * N_L_hat
    TC_r_hat = A_r * max(N_r_hat, 1)**(1 - lam) if N_r_hat > 0 else 0
    C_hat = TC_L_hat + TC_r_hat
    
    # D&P incumbent metric
    N_L_inc = N_L_2001
    C_base_DP = c_L_base * N_L_inc + c_r_base * (N_total - N_L_inc)
    N_L_inc_cf = min(N_L_inc, N_L_hat)
    C_hat_DP = c_L_hat * N_L_inc_cf + c_r_hat * (N_total - N_L_inc_cf)
    
    # Income
    Y_base = y_L_base * N_L_base + c_r_base * N_r_base
    Y_hat = y_L_hat * N_L_hat + c_r_hat * N_r_hat
    
    # Permitting cost at counterfactual
    p_L_hat = c_L_hat - c_r_hat
    
    # Marginal social gain from one more Londoner at this N
    dTC_L = (1 + sb) * K_A * tau * N_L_hat**sb - ((1 + gt) / (g + 1)) * z_L**g * tau * N_L_hat**gt
    dTC_r = (1 - lam) * c_r_hat if N_r_hat > 0 else 0
    marginal_gain = dTC_L - dTC_r
    
    return {
        'N_L': N_L_hat,
        'N_r': N_r_hat,
        'ratio': N_L_hat / N_L_base,
        'c_L': c_L_hat,
        'c_r': c_r_hat,
        'y_L': y_L_hat,
        'p_L': p_L_hat,
        'pct_c_L': 100 * (c_L_hat - c_L_base) / c_L_base,
        'pct_c_r': 100 * (c_r_hat - c_r_base) / c_r_base,
        'pct_y_L': 100 * (y_L_hat - y_L_base) / y_L_base,
        'pct_C_social': 100 * (C_hat - C_base) / C_base,
        'pct_C_DP': 100 * (C_hat_DP - C_base_DP) / C_base_DP,
        'pct_Y': 100 * (Y_hat - Y_base) / Y_base,
        'marginal_gain': marginal_gain,
        'p_L_share': 100 * p_L_hat / c_L_hat if c_L_hat > 0 else float('nan'),
        # Share of London in total
        'london_share': 100 * N_L_hat / N_total,
    }


omega = 0.50

print("=" * 100)
print("WELFARE GAINS AT SPECIFIC LONDON POPULATION LEVELS")
print("=" * 100)

for label, params in [("D&P US PARAMETERS", PARAMS_US), ("YOUR UK PARAMETERS", PARAMS_UK)]:
    sb = params['sigma'] + params['beta']
    gt = params['gamma'] + params['theta']
    nt = gt - sb
    
    print(f"\n{'='*100}")
    print(f"  {label}:  sigma+beta={sb:.3f}  gamma+theta={gt:.3f}  lambda={params['lambda']:.3f}")
    print(f"  cost/benefit = {gt/sb:.3f}   c/y = {nt/gt:.4f}   omega = {omega}")
    print(f"{'='*100}")
    
    # Population scenarios
    scenarios = [
        (N_L_BASE, "Baseline"),
        (int(N_L_BASE * 1.25), "London +25%"),
        (int(N_L_BASE * 1.50), "London +50%"),
        (int(N_L_BASE * 2.00), "London 2x"),
        (int(N_L_BASE * 3.00), "London 3x"),
        (int(N_L_BASE * 4.00), "London 4x"),
        (int(N_L_BASE * 5.00), "London 5x"),
        (int(N_TOTAL * 0.50), "50% urbanisation"),
        (int(N_TOTAL * 0.75), "75% urbanisation"),
        (int(N_TOTAL * 0.90), "90% urbanisation"),
        (int(N_TOTAL * 0.95), "95% urbanisation"),
    ]
    
    header = (f"{'Scenario':>22} | {'N_L(M)':>8} | {'N_r(M)':>8} | {'L share':>8} | "
              f"{'%dc_L':>8} | {'%dc_r':>8} | {'%dC_soc':>9} | {'%dC_DP':>9} | "
              f"{'%dY':>9} | {'p/c_L':>6} | {'MG>0?':>6}")
    print(f"\n{header}")
    print("-" * len(header))
    
    for N_L_cf, name in scenarios:
        r = compute_welfare_at_N(params, N_L_cf, N_L_BASE, N_TOTAL, Z_L, TAU, omega)
        mg_sign = "YES" if r['marginal_gain'] > 0 else "NO"
        print(f"{name:>22} | {r['N_L']/1e6:>8.2f} | {r['N_r']/1e6:>8.2f} | "
              f"{r['london_share']:>7.1f}% | "
              f"{r['pct_c_L']:>+8.3f} | {r['pct_c_r']:>+8.3f} | "
              f"{r['pct_C_social']:>+9.3f} | {r['pct_C_DP']:>+9.3f} | "
              f"{r['pct_Y']:>+9.3f} | {r['p_L_share']:>5.1f}% | {mg_sign:>6}")

# =============================================================================
# WHY z_L DROPS OUT
# =============================================================================

print(f"\n\n{'='*100}")
print("WHY z_L DOES NOT AFFECT THE SOCIAL OPTIMUM")
print(f"{'='*100}")
print("""
The sensitivity table showed zero variation with z_L. This is NOT a bug.

The social planner's FOC is:

  dTC_L/dN = (1+sigma+beta) * K_A * tau * N^{sigma+beta} 
           - ((1+gamma+theta)/(gamma+1)) * z^gamma * tau * N^{gamma+theta}
           
where K_A = [(gamma+theta)/((sigma+beta)(gamma+1))] * z^gamma * N_base^{net}

Notice that z^gamma appears in BOTH terms. It factors out of the FOC entirely.
The social planner's optimal population ratio N_hat/N_base is independent of z.

Economic interpretation: z_L affects BOTH the costs of London expansion (directly, 
through commuting/housing costs) AND the backed-out productivity A_L (indirectly, 
via eq 21: a city with high geographic constraints must have high A to explain 
why it's as large as it is despite those constraints). These two effects cancel
exactly in the ratio N_hat/N_base.

This is a knife-edge result of the Cobb-Douglas functional forms in D&P.
""")

# =============================================================================
# WHY THE CORNER SOLUTION
# =============================================================================

print(f"\n{'='*100}")
print("WHY THE SOCIAL PLANNER HITS THE CORNER: ECONOMIC INTERPRETATION")
print(f"{'='*100}")
print("""
In the two-location model (one city + rest), the social optimum puts ~95% of 
England in London. This is extreme but logically correct within the model, 
for the following reason:

The marginal social gain from moving one person from rural to London is:

  Delta = [c_L(N) + N * c_L'(N)] - (1-lambda) * c_r(N_r)

At the incumbent optimum, c_L'(N) = 0, so Delta = c_L - (1-lambda)*c_r > 0.
As London expands past the incumbent optimum:
  - c_L(N) falls (past the peak of the consumption curve)
  - But c_L'(N) * N is now NEGATIVE (city is too big for incumbents)
  - The agglomeration externality (positive spillover on all N residents) keeps
    the SOCIAL return positive much longer than the PRIVATE return

With D&P's US exponents (sigma+beta=0.08, gamma+theta=0.11), both are very 
small, meaning the consumption curve is extremely FLAT. The private peak is 
sharp, but the social gain erodes very slowly. It takes enormous population 
expansions before the social marginal product falls to the rural level.

This is the fundamental insight: THE TWO-LOCATION MODEL IS TOO STARK.
D&P's moderate results (2.16% consumption gain, 7.95% income gain) come from 
having MANY cities. The social planner redistributes people across multiple 
productive cities, not just one. With many cities:
  - Each city's expansion is bounded by other cities' competition for workers
  - The extensive margin (vacating small cities) provides moderate gains
  - No single city needs to absorb the entire rural population

For your UK application: the multi-city framework in your code is essential.
The two-location thought experiment illustrates the DIRECTION and SOURCES of 
gains, but the magnitude is only meaningful with the full city-size distribution.
""")

# =============================================================================
# REALISTIC THOUGHT EXPERIMENT: Doubling London
# =============================================================================

print(f"\n{'='*100}")
print("REALISTIC SCENARIO: DOUBLING LONDON (9M -> 18M)")
print(f"{'='*100}")

for label, params in [("D&P US params", PARAMS_US), ("UK params", PARAMS_UK)]:
    r = compute_welfare_at_N(params, int(N_L_BASE * 2), N_L_BASE, N_TOTAL, Z_L, TAU, omega)
    sb = params['sigma'] + params['beta']
    gt = params['gamma'] + params['theta']
    
    print(f"\n--- {label} ---")
    print(f"  London population: {N_L_BASE/1e6:.1f}M -> {r['N_L']/1e6:.1f}M")
    print(f"  Rest of England:   {(N_TOTAL-N_L_BASE)/1e6:.1f}M -> {r['N_r']/1e6:.1f}M")
    print(f"")
    print(f"  London incumbent consumption:     {r['pct_c_L']:+.3f}%")
    print(f"  Rest-of-England consumption:      {r['pct_c_r']:+.3f}%")
    print(f"  London per-capita income:         {r['pct_y_L']:+.3f}%")
    print(f"")
    print(f"  Aggregate consumption (social):   {r['pct_C_social']:+.3f}%")
    print(f"  Aggregate consumption (D&P):      {r['pct_C_DP']:+.3f}%")
    print(f"  Aggregate income:                 {r['pct_Y']:+.3f}%")
    print(f"")
    print(f"  Permitting cost as % of c_L:      {r['p_L_share']:.1f}% (was 50.0%)")
    print(f"  Still socially beneficial to expand? {'YES' if r['marginal_gain'] > 0 else 'NO'}")
    
    # Eq (42) check: c_hat/c = (gt/(gt-sb)) * r^sb - (sb/(gt-sb)) * r^gt
    r_pop = 2.0
    c_ratio = (gt / (gt - sb)) * r_pop**sb - (sb / (gt - sb)) * r_pop**gt
    print(f"")
    print(f"  Eq (42) check: c_hat/c_baseline = {c_ratio:.6f} => {100*(c_ratio-1):+.3f}%")