"""Generate the static central-parameter results served to the front end:
public/default_data.json (permitting rate) and public/default_data_households.json
(census household growth robustness check).

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
from lib.data import MEASURES, PARAMS_CENTRAL, cities_sets, default_results

OUT_FILES = {"permits": "default_data.json", "households": "default_data_households.json"}

for measure, fname in OUT_FILES.items():
    results = default_results(measure)
    m = MEASURES[measure]

    def _cat(key):
        return pd.concat([results[cs][key] for cs in cities_sets], ignore_index=True)

    payload = {
        "agg": _cat("agg").to_dict(orient="records"),
        "city_income": _cat("city_income").to_dict(orient="records"),
        "city_cons": _cat("city_cons").to_dict(orient="records"),
        "city_pop": _cat("city_pop").to_dict(orient="records"),
        "reference_rates": m["reference_rates"].to_dict(orient="records"),
        "params": PARAMS_CENTRAL,
        "city_set": "all",
        "city_set_labels": CITY_SET_LABELS,
        "rate_sequence": m["rate_sequence"].tolist(),
        "measure": measure,
        "x_label": m["x_label"],
        "dtick": m["dtick"],
    }

    out = os.path.abspath(os.path.join(os.path.dirname(__file__), "..", "public", fname))
    with open(out, "w") as f:
        json.dump(payload, f)

    print(f"Wrote {out} ({os.path.getsize(out) / 1024:.1f} KB)")
