import os
import math
import numpy as np
import pandas as pd

from .constants import POP_ENGLAND, CITY_SET_LABELS

_DATA_DIR = os.path.join(os.path.dirname(__file__), "..", "data")


def _load_csv(name):
    return pd.read_csv(os.path.join(_DATA_DIR, name))


# ---------------------------------------------------------------------------
# City data (loaded once at module import / cold start)
# ---------------------------------------------------------------------------
city_data = _load_csv("city_data.csv")

# Population totals
urb_01 = city_data["bua_01_pop"].sum()
urb_21 = city_data["bua_21_pop"].sum()
rur_01 = POP_ENGLAND["y2001"] - urb_01
rur_21 = POP_ENGLAND["y2021"] - urb_21

pop_totals = {
    "tot_01": POP_ENGLAND["y2001"],
    "tot_21": POP_ENGLAND["y2021"],
    "urb_01": urb_01,
    "urb_21": urb_21,
    "rur_01": rur_01,
    "rur_21": rur_21,
}

# ---------------------------------------------------------------------------
# City sets (data is sorted by descending bua_21_pop)
# ---------------------------------------------------------------------------
city_names = city_data["BUA22NM"].tolist()

cities_sets = {
    "london_only": city_names[:1],
    "top_4": city_names[:4],
    "top_6": city_names[:6],
    "university_cities": ["Greater London", "Oxford", "Cambridge (Cambridge)"],
    "top_10": city_names[:10],
}

# ---------------------------------------------------------------------------
# Parameter ranges
# ---------------------------------------------------------------------------
param_ranges = _load_csv("parameter_estimates.csv")

PARAMS_CENTRAL = {}
for _, row in param_ranges.iterrows():
    PARAMS_CENTRAL[row["parameter"]] = row["central"]

# ---------------------------------------------------------------------------
# Reference rates
# ---------------------------------------------------------------------------
reference_rates = _load_csv("reference_rates.csv")

# ---------------------------------------------------------------------------
# Rate sequence
# ---------------------------------------------------------------------------
all_perm_rates = city_data["bua_perm_rate_01_21"].values
pct_75 = float(np.percentile(all_perm_rates, 75))
max_uk_rate = float(reference_rates["rate"].max())

rate_start = math.floor((pct_75 - 0.01) * 100) / 100
rate_end = math.ceil((max_uk_rate + 0.01) * 100) / 100
rate_sequence = np.arange(rate_start, rate_end + 0.0005, 0.001)
rate_sequence = np.round(rate_sequence, 3)

# ---------------------------------------------------------------------------
# Robustness check: census household growth as the housing-supply measure
# (households 2021 - households 2001, per 2001 household). Same counterfactual,
# with this column in place of the permitting rate. Targets are the 75th and
# 95th percentiles; the maximum (Ingleby Barwick, ~0.63) is a new-build outlier.
# ---------------------------------------------------------------------------
HH_COL = "bua_hh_growth_01_21"


def _percentile_ref(col, q):
    vals = city_data[col]
    rate = float(np.percentile(vals, q))
    city = city_data.loc[vals >= rate].sort_values(col).iloc[0]["BUA22NM"]
    return {"label": f"{q}th percentile ({city})", "rate": rate}


hh_reference_rates = pd.DataFrame([_percentile_ref(HH_COL, 75), _percentile_ref(HH_COL, 95)])
hh_rate_start = math.floor(float(np.median(city_data[HH_COL])) * 100) / 100
hh_rate_end = math.ceil((float(hh_reference_rates["rate"].max()) + 0.03) * 100) / 100
hh_rate_sequence = np.round(np.arange(hh_rate_start, hh_rate_end + 0.00125, 0.0025), 4)

MEASURES = {
    "permits": {
        "col": "bua_perm_rate_01_21",
        "rate_sequence": rate_sequence,
        "reference_rates": reference_rates,
        "x_label": "Counterfactual permitting rate (permits per 2001 resident, 2001\u20132021)",
        "dtick": 0.02,
    },
    "households": {
        "col": HH_COL,
        "rate_sequence": hh_rate_sequence,
        "reference_rates": hh_reference_rates,
        "x_label": "Counterfactual household growth (new households per 2001 household, 2001\u20132021)",
        "dtick": 0.05,
    },
}

# ---------------------------------------------------------------------------
# Pre-compute results for central parameters (runs once per cold start).
# Mirrors R's global.R: server.R short-circuits to these when params match.
# ---------------------------------------------------------------------------
from .model import run_sweep  # noqa: E402  (deferred to avoid hot-path cost on import order)

def _central_results(measure):
    m = MEASURES[measure]
    return {
        cs_name: run_sweep(cs_name, cs_cities, m["rate_sequence"], pop_totals, PARAMS_CENTRAL,
                           city_data, rate_col=m["col"])
        for cs_name, cs_cities in cities_sets.items()
    }


DEFAULT_RESULTS = _central_results("permits")
_DEFAULT_CACHE = {"permits": DEFAULT_RESULTS}


def default_results(measure):
    """Central-parameter results for a measure; the household ones are computed on first use."""
    if measure not in _DEFAULT_CACHE:
        _DEFAULT_CACHE[measure] = _central_results(measure)
    return _DEFAULT_CACHE[measure]
