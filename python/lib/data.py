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
austin_rate = reference_rates.loc[
    reference_rates["label"].str.contains("Austin"), "rate"
].iloc[0]

rate_start = math.floor((pct_75 - 0.01) * 100) / 100
rate_end = math.ceil((austin_rate + 0.005) * 100) / 100
rate_sequence = np.arange(rate_start, rate_end + 0.0005, 0.001)
rate_sequence = np.round(rate_sequence, 3)
