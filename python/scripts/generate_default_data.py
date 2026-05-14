"""Generate public/default_data.json from the pre-computed central-param results.

Run from python/ directory:
    python3 scripts/generate_default_data.py

Regenerate whenever city_data.csv or PARAMS_CENTRAL changes, then commit the JSON.
"""

import json
import os
import sys

sys.path.insert(0, os.path.join(os.path.dirname(__file__), ".."))

import pandas as pd
from lib.constants import CITY_SET_LABELS
from lib.data import (
    DEFAULT_RESULTS,
    PARAMS_CENTRAL,
    cities_sets,
    rate_sequence,
    reference_rates,
)

agg = pd.concat([DEFAULT_RESULTS[cs]["agg"] for cs in cities_sets], ignore_index=True)
city_income = pd.concat(
    [DEFAULT_RESULTS[cs]["city_income"] for cs in cities_sets], ignore_index=True
)
city_cons = pd.concat(
    [DEFAULT_RESULTS[cs]["city_cons"] for cs in cities_sets], ignore_index=True
)
city_pop = pd.concat(
    [DEFAULT_RESULTS[cs]["city_pop"] for cs in cities_sets], ignore_index=True
)

payload = {
    "agg": agg.to_dict(orient="records"),
    "city_income": city_income.to_dict(orient="records"),
    "city_cons": city_cons.to_dict(orient="records"),
    "city_pop": city_pop.to_dict(orient="records"),
    "reference_rates": reference_rates.to_dict(orient="records"),
    "params": PARAMS_CENTRAL,
    "city_set": "all",
    "city_set_labels": CITY_SET_LABELS,
    "rate_sequence": rate_sequence.tolist(),
}

out = os.path.abspath(
    os.path.join(os.path.dirname(__file__), "..", "public", "default_data.json")
)
with open(out, "w") as f:
    json.dump(payload, f)

print(f"Wrote {out} ({os.path.getsize(out) / 1024:.1f} KB)")
