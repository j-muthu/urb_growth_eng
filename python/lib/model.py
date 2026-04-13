"""
Core counterfactual computation — direct port of counterfactual_functions.R.
"""

import numpy as np
import pandas as pd

from .constants import RGDP_PER_CAPITA


def prepare_counterfactual_data(city_data, cities_in_cf, params, pop_totals):
    gamma = params["gamma"]
    theta = params["theta"]
    sigma = params["sigma"]
    beta = params["beta"]
    lam = params["lambda"]

    d = city_data.copy()
    gc = d["geographic_constraint"].values
    pop01 = d["bua_01_pop"].values.astype(float)
    pop21 = d["bua_21_pop"].values.astype(float)

    eq_coeff = (gamma + theta) / ((sigma + beta) * (gamma + 1)) * gc ** gamma
    eq_coeff_01 = eq_coeff * pop01 ** (gamma + theta - sigma - beta)
    eq_coeff_21 = eq_coeff * pop21 ** (gamma + theta - sigma - beta)

    income_div_tau_01 = eq_coeff_01 * pop01 ** (sigma + beta)
    income_div_tau_21 = eq_coeff_21 * pop21 ** (sigma + beta)

    cons_coeff = (gamma + theta - sigma - beta) / ((sigma + beta) * (gamma + 1)) * gc ** gamma
    cons_div_tau_01 = cons_coeff * pop01 ** (gamma + theta)
    cons_div_tau_21 = cons_coeff * pop21 ** (gamma + theta)

    # Tau normalisation
    rgdp_multiple = RGDP_PER_CAPITA["y2021"] / RGDP_PER_CAPITA["y2001"]
    sum_inc_pc_01 = np.sum(income_div_tau_01 * pop01) / pop_totals["urb_01"]
    sum_inc_pc_21 = np.sum(income_div_tau_21 * pop21) / pop_totals["urb_21"]

    tau_01 = 1.0
    tau_21 = rgdp_multiple / (sum_inc_pc_21 / sum_inc_pc_01)

    d["rho_A_h_01"] = eq_coeff_01 * tau_01
    d["rho_A_h_21"] = eq_coeff_21 * tau_21
    d["y_01"] = income_div_tau_01 * tau_01
    d["y_21"] = income_div_tau_21 * tau_21
    d["c_01"] = cons_div_tau_01 * tau_01
    d["c_21"] = cons_div_tau_21 * tau_21

    rur_01_income = d["c_01"].min()
    rur_21_income = d["c_21"].min()
    rur_01_prod = rur_01_income / (pop_totals["rur_01"] ** (-lam))
    rur_21_prod = rur_21_income / (pop_totals["rur_21"] ** (-lam))

    d["perm_01"] = d["c_01"] - rur_01_income
    d["perm_21"] = d["c_21"] - rur_21_income
    d["in_counterfact"] = d["BUA22NM"].isin(cities_in_cf).astype(int)
    d["pop_21_incumb"] = np.minimum(pop01, pop21)

    d = d.sort_values("bua_01_pop", ascending=False).reset_index(drop=True)

    rur_21_pop_incumb = pop_totals["tot_21"] - d["pop_21_incumb"].sum()

    return {
        "data": d,
        "tau_01": tau_01,
        "tau_21": tau_21,
        "rur_01_income": rur_01_income,
        "rur_21_income": rur_21_income,
        "rur_01_prod": rur_01_prod,
        "rur_21_prod": rur_21_prod,
        "rur_21_pop_incumb": rur_21_pop_incumb,
        "params": params,
        "pop_totals": pop_totals,
    }


def compute_cf_populations(cf_prep, perm_rate_cf):
    d = cf_prep["data"].copy()
    d["perm_rate_counterfact"] = perm_rate_cf
    d["bua_21_pop_counterfact"] = d["bua_21_pop"].values.astype(float).copy()
    mask = d["in_counterfact"].values == 1
    base_rate = d["bua_perm_rate_01_21"].values
    pop01 = d["bua_01_pop"].values
    pop21 = d["bua_21_pop"].values
    d.loc[mask, "bua_21_pop_counterfact"] = (
        (perm_rate_cf[mask] / base_rate[mask]) * (pop21[mask] - pop01[mask]) + pop01[mask]
    )
    return d


def find_marginal_city(cf_data, cf_prep):
    params = cf_prep["params"]
    pop_totals = cf_prep["pop_totals"]
    tau_21 = cf_prep["tau_21"]
    rur_21_prod = cf_prep["rur_21_prod"]
    gamma = params["gamma"]
    theta = params["theta"]
    sigma = params["sigma"]
    beta = params["beta"]
    lam = params["lambda"]

    d = cf_data.copy()
    pop_cf = d["bua_21_pop_counterfact"].values.astype(float)
    y21 = d["y_21"].values
    pop21 = d["bua_21_pop"].values.astype(float)
    gc = d["geographic_constraint"].values

    y_cf = (y21 / pop21 ** (sigma + beta)) * pop_cf ** (sigma + beta)
    c_cf = y_cf - (1.0 / (gamma + 1)) * tau_21 * gc ** gamma * pop_cf ** (gamma + theta)

    # Sort by counterfactual consumption descending
    order = np.argsort(-c_cf)
    d = d.iloc[order].reset_index(drop=True)
    y_cf = y_cf[order]
    c_cf = c_cf[order]
    pop_cf_sorted = pop_cf[order]

    cumpop = np.cumsum(pop_cf_sorted)
    n = len(d)

    marginal_city_index = n - 1
    while marginal_city_index >= 0:
        if cumpop[marginal_city_index] > pop_totals["tot_21"]:
            marginal_city_index -= 1
            continue
        urban_pop = cumpop[marginal_city_index]
        rural_pop = pop_totals["tot_21"] - urban_pop
        c_rural_threshold = rur_21_prod * rural_pop ** (-lam)
        if c_rural_threshold > c_cf[marginal_city_index]:
            marginal_city_index -= 1
        else:
            break

    pop_cum_final = cumpop[marginal_city_index] if marginal_city_index >= 0 else 0.0
    c_21_rural_threshold = rur_21_prod * (pop_totals["tot_21"] - pop_cum_final) ** (-lam)

    # Exclude cities below marginal
    d["pop_21_cf"] = pop_cf_sorted
    d["y_21_cf"] = y_cf
    d["c_21_cf"] = c_cf
    d["city_order"] = np.arange(n)

    excluded = d["city_order"].values > marginal_city_index
    d.loc[excluded, "pop_21_cf"] = np.nan
    d.loc[excluded, "y_21_cf"] = np.nan
    d.loc[excluded, "c_21_cf"] = np.nan

    d["c_21_rural_threshold"] = c_21_rural_threshold
    d["perm_21_counterfact"] = d["c_21_cf"] - c_21_rural_threshold
    d["pop_21_incumb_counterfact"] = np.where(
        np.isnan(d["pop_21_cf"].values),
        np.nan,
        np.minimum(d["pop_21_cf"].values, d["pop_21_incumb"].values),
    )

    return {
        "data": d,
        "marginal_city_index": marginal_city_index,
        "c_21_rur_counterfact": c_21_rural_threshold,
        "pop_21_rur_counterfact": pop_totals["tot_21"] - pop_cum_final,
    }


def compute_welfare_metrics(marg_result, cf_prep):
    d = marg_result["data"]
    pt = cf_prep["pop_totals"]
    rur_21_income = cf_prep["rur_21_income"]
    rur_21_pop_incumb = cf_prep["rur_21_pop_incumb"]
    c_rur_cf = marg_result["c_21_rur_counterfact"]
    pop_rur_cf = marg_result["pop_21_rur_counterfact"]
    tot = pt["tot_21"]

    # Income
    y_base = np.nansum(d["y_21"].values * d["bua_21_pop"].values / tot) + rur_21_income * (pt["rur_21"] / tot)
    y_cf = np.nansum(d["y_21_cf"].values * d["pop_21_cf"].values / tot) + c_rur_cf * (pop_rur_cf / tot)

    # D&P incumbent consumption
    c_inc_base = np.nansum(d["c_21"].values * d["pop_21_incumb"].values / tot) + rur_21_income * (rur_21_pop_incumb / tot)
    pop_rur_inc_cf = tot - np.nansum(d["pop_21_incumb_counterfact"].values)
    c_inc_cf = np.nansum(d["c_21_cf"].values * d["pop_21_incumb_counterfact"].values / tot) + c_rur_cf * (pop_rur_inc_cf / tot)

    # Social planner consumption
    c_soc_base = np.nansum(d["c_21"].values * d["bua_21_pop"].values / tot) + rur_21_income * (pt["rur_21"] / tot)
    c_soc_cf = np.nansum(d["c_21_cf"].values * d["pop_21_cf"].values / tot) + c_rur_cf * (pop_rur_cf / tot)

    return {
        "pct_chg_y_tot": 100.0 * (y_cf - y_base) / y_base,
        "pct_chg_c_incumb": 100.0 * (c_inc_cf - c_inc_base) / c_inc_base,
        "pct_chg_c_social": 100.0 * (c_soc_cf - c_soc_base) / c_soc_base,
        "pct_chg_c_rur": 100.0 * (c_rur_cf - rur_21_income) / rur_21_income,
        "pct_chg_pop_rur": 100.0 * (pop_rur_cf - pt["rur_21"]) / pt["rur_21"],
    }


def run_single_counterfactual(cf_prep, perm_rate_cf):
    cf_data = compute_cf_populations(cf_prep, perm_rate_cf)
    marg = find_marginal_city(cf_data, cf_prep)
    welfare = compute_welfare_metrics(marg, cf_prep)

    d = marg["data"]
    d["pct_chg_y"] = 100.0 * (d["y_21_cf"] - d["y_21"]) / d["y_21"]
    d["pct_chg_c"] = 100.0 * (d["c_21_cf"] - d["c_21"]) / d["c_21"]

    return {"data": d, "welfare": welfare}


def run_sweep(city_set_name, cities_in_cf, rate_sequence, pop_totals, params, city_data):
    cf_prep = prepare_counterfactual_data(city_data, cities_in_cf, params, pop_totals)

    in_cf = cf_prep["data"]["in_counterfact"].values == 1
    base_rate = cf_prep["data"]["bua_perm_rate_01_21"].values.copy()

    agg_rows = []
    city_income_rows = []
    city_cons_rows = []

    for target_rate in rate_sequence:
        perm_rate_cf = base_rate.copy()
        perm_rate_cf[in_cf] = np.maximum(base_rate[in_cf], target_rate)

        result = run_single_counterfactual(cf_prep, perm_rate_cf)
        rd = result["data"]
        w = result["welfare"]

        treated = rd[(rd["in_counterfact"] == 1) & rd["pct_chg_y"].notna()]
        if len(treated) > 0:
            pct_chg_y_cities = (
                (treated["pct_chg_y"] * treated["bua_21_pop"]).sum()
                / treated["bua_21_pop"].sum()
            )
        else:
            pct_chg_y_cities = float("nan")

        agg_rows.append({
            "target_rate": float(target_rate),
            "pct_chg_newcomer_cons": w["pct_chg_c_rur"],
            "pct_chg_cons_incumb": w["pct_chg_c_incumb"],
            "pct_chg_cons_social": w["pct_chg_c_social"],
            "pct_chg_national_income_pc": w["pct_chg_y_tot"],
            "pct_chg_city_income_pc": pct_chg_y_cities,
        })

        for _, row in treated.iterrows():
            city_income_rows.append({
                "BUA22NM": row["BUA22NM"],
                "bua_21_pop": row["bua_21_pop"],
                "pct_chg_y": row["pct_chg_y"],
                "target_rate": float(target_rate),
            })
            city_cons_rows.append({
                "BUA22NM": row["BUA22NM"],
                "bua_21_pop": row["bua_21_pop"],
                "pct_chg_c": row["pct_chg_c"],
                "target_rate": float(target_rate),
            })

    agg = pd.DataFrame(agg_rows)
    agg["city_set"] = city_set_name
    city_income = pd.DataFrame(city_income_rows)
    city_income["city_set"] = city_set_name
    city_cons = pd.DataFrame(city_cons_rows)
    city_cons["city_set"] = city_set_name

    return {"agg": agg, "city_income": city_income, "city_cons": city_cons}
