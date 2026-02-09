import pandas as pd
from pathlib import Path

# Get the directory where the current script is located
script_dir = Path(__file__).parent

# Join the directory with the filename
file_path = script_dir / 'WHARTON LAND REGULATION DATA_01_15_2020.dta'
wrluri = pd.read_stata(file_path)

wrluri_filtered = wrluri[
	["state", "cbsatitle18", "countyname18", "communityname18", "WRLURI18"]
						 ].dropna(subset=['WRLURI18'])


# Group by CBSA and overwrite the WRLURI18 column with the group's mean
wrluri_filtered['WRLURI18'] = wrluri_filtered.groupby('cbsatitle18')['WRLURI18'].transform('mean')
wrluri_filtered.sort_values(by="WRLURI18") # lower is less restrictive

sf_str = "San Francisco-Oakland-Hayward, CA"
# sf = wrluri_filtered[wrluri_filtered["cbsatitle18"] == sf_str]
austin_str = "Austin-Round Rock, TX"
austin = wrluri_filtered[wrluri_filtered["cbsatitle18"] == austin_str]

austin_idx = austin["WRLURI18"].mean()
# sf_idx = sf["WRLURI18"].mean()
print(f"Austin: {austin_idx}")
# print(f"SF: {sf_idx} ({"matches" if round(sf_idx, 2) == 1.18 else "doesn't match"} "
# 	  "paper https://www.nber.org/system/files/working_papers/w26573/w26573.pdf)")

wrluri_filtered.to_csv("wrluri_filtered.csv")
# austin.to_csv("austin_wrluri.csv")


# 1. Create a DataFrame of unique CBSAs and their mean WRLURI18 scores
#    (If you already transformed the column, you can use .drop_duplicates())
cbsa_stats = wrluri_filtered.groupby("cbsatitle18")["WRLURI18"].mean().reset_index()

# 2. Calculate Rank (1 = Lowest Regulation/Least Restrictive)
cbsa_stats["Rank"] = cbsa_stats["WRLURI18"].rank(ascending=True)

# 3. Calculate Percentile (0 to 100)
cbsa_stats["Percentile"] = cbsa_stats["WRLURI18"].rank(pct=True) * 100

# 4. Output Austin's stats
austin_stats = cbsa_stats[cbsa_stats["cbsatitle18"] == austin_str]
sf_stats = cbsa_stats[cbsa_stats["cbsatitle18"] == sf_str]

stl_str = "St. Louis, MO-IL"
stl_stats = cbsa_stats[cbsa_stats["cbsatitle18"] == stl_str]

print("Total CBSAs:", len(cbsa_stats))
print(austin_stats.to_string(index=False),"\n")
print(sf_stats.to_string(index=False),"\n")
print(stl_stats.to_string(index=False),"\n")


# Austin: 0.5261561870574951
# Total CBSAs: 544
#           cbsatitle18  WRLURI18  Rank  Percentile
# Austin-Round Rock, TX  0.526156 447.0   82.169118 

#                       cbsatitle18  WRLURI18  Rank  Percentile
# San Francisco-Oakland-Hayward, CA  1.182753 517.0   95.036765 

#      cbsatitle18  WRLURI18  Rank  Percentile
# St. Louis, MO-IL -0.508881 177.0   32.536765 