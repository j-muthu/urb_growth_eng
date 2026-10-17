"""Regression tests for the counterfactual model and API.

Run from python/:  python3 -m pytest tests
"""
import json
import os
import sys
import threading
import urllib.error
import urllib.request
from http.server import HTTPServer

import numpy as np

sys.path.insert(0, os.path.join(os.path.dirname(__file__), ".."))

from lib.data import PARAMS_CENTRAL, cities_sets, city_data, pop_totals  # noqa: E402
from lib.model import run_sweep  # noqa: E402


def test_no_treatment_gives_zero_change():
    """With no city treated beyond its observed rate, every national outcome is exactly zero."""
    for cs, cities in cities_sets.items():
        row = run_sweep(cs, cities, np.array([0.0]), pop_totals, PARAMS_CENTRAL, city_data)["agg"].iloc[0]
        for col in ["pct_chg_newcomer_cons", "pct_chg_cons_incumb",
                    "pct_chg_cons_social", "pct_chg_national_income_pc"]:
            assert abs(row[col]) < 1e-10, (cs, col, row[col])
        assert abs(row["pop_delta_rural"]) < 1e-6, (cs, row["pop_delta_rural"])


def _post(port, body):
    req = urllib.request.Request(f"http://127.0.0.1:{port}/", data=json.dumps(body).encode(),
                                 headers={"Content-Type": "application/json"})
    try:
        with urllib.request.urlopen(req) as resp:
            return resp.status, json.loads(resp.read())
    except urllib.error.HTTPError as e:
        return e.code, json.loads(e.read())


def test_api_rejects_second_order_condition_violation():
    from api.compute import handler

    server = HTTPServer(("127.0.0.1", 0), handler)
    port = server.server_address[1]
    thread = threading.Thread(target=server.serve_forever, daemon=True)
    thread.start()
    try:
        status, body = _post(port, {"gamma": 0.01, "theta": 0.01, "sigma": 0.03, "beta": 0.03,
                                    "lambda": 0.18, "city_set": "london_only"})
        assert status == 400 and "gamma + theta > sigma + beta" in body["error"]
        status, body = _post(port, {**PARAMS_CENTRAL, "gamma": 0.06, "city_set": "london_only"})
        assert status == 200 and len(body["agg"]) > 0
    finally:
        server.shutdown()
