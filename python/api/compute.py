import json
import sys
import os
from http.server import BaseHTTPRequestHandler

# Add project root to path so lib/ is importable
sys.path.insert(0, os.path.join(os.path.dirname(__file__), ".."))

from lib.data import (
    city_data,
    cities_sets,
    rate_sequence,
    pop_totals,
    reference_rates,
    param_ranges,
    PARAMS_CENTRAL,
)
from lib.model import run_sweep
from lib.constants import CITY_SET_LABELS


def _run(params, city_set_key):
    sets_to_run = list(cities_sets.keys()) if city_set_key == "all" else [city_set_key]

    all_agg = []
    all_city_income = []
    all_city_cons = []

    for cs in sets_to_run:
        result = run_sweep(cs, cities_sets[cs], rate_sequence, pop_totals, params, city_data)
        all_agg.append(result["agg"])
        all_city_income.append(result["city_income"])
        all_city_cons.append(result["city_cons"])

    import pandas as pd

    agg = pd.concat(all_agg, ignore_index=True)
    city_income = pd.concat(all_city_income, ignore_index=True)
    city_cons = pd.concat(all_city_cons, ignore_index=True)

    return {
        "agg": agg.to_dict(orient="records"),
        "city_income": city_income.to_dict(orient="records"),
        "city_cons": city_cons.to_dict(orient="records"),
        "reference_rates": reference_rates.to_dict(orient="records"),
        "params": params,
        "city_set": city_set_key,
        "city_set_labels": CITY_SET_LABELS,
        "rate_sequence": rate_sequence.tolist(),
    }


class handler(BaseHTTPRequestHandler):
    def do_POST(self):
        try:
            content_length = int(self.headers.get("Content-Length", 0))
            body = json.loads(self.rfile.read(content_length)) if content_length else {}

            city_set_key = body.get("city_set", "all")
            if city_set_key != "all" and city_set_key not in cities_sets:
                self._error(400, f"Unknown city_set: {city_set_key}")
                return

            params = {}
            for _, row in param_ranges.iterrows():
                name = row["parameter"]
                val = body.get(name, PARAMS_CENTRAL[name])
                val = float(val)
                params[name] = val

            result = _run(params, city_set_key)

            self.send_response(200)
            self.send_header("Content-Type", "application/json")
            self.send_header("Access-Control-Allow-Origin", "*")
            self.end_headers()
            self.wfile.write(json.dumps(result).encode())

        except Exception as e:
            self._error(500, str(e))

    def do_OPTIONS(self):
        self.send_response(204)
        self.send_header("Access-Control-Allow-Origin", "*")
        self.send_header("Access-Control-Allow-Methods", "POST, OPTIONS")
        self.send_header("Access-Control-Allow-Headers", "Content-Type")
        self.end_headers()

    def _error(self, code, msg):
        self.send_response(code)
        self.send_header("Content-Type", "application/json")
        self.send_header("Access-Control-Allow-Origin", "*")
        self.end_headers()
        self.wfile.write(json.dumps({"error": msg}).encode())
