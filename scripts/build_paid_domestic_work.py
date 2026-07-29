"""
Build "daily average duration of paid work and domestic work, by sex and
age group" and "participation rate in domestic work" -- replicating
genderpulse.md's charts, using Chile's public ENUT 2023 data.

Usage (from project root):
  pip install pyreadr pandas numpy
  python scripts/build_paid_domestic_work.py path/to/250403-ii-enut-bdd-r-v2.RDS
"""

import sys
import json

import numpy as np
import pandas as pd
import pyreadr

SRC = sys.argv[1] if len(sys.argv) > 1 else "250403-ii-enut-bdd-r-v2.RDS"
OUT_DIR = "data/enut"
MIN_N = 30

r = pyreadr.read_r(SRC)
df = list(r.values())[0]

SEX_LABEL = {1.0: "Hombres", 2.0: "Mujeres"}
df["sex"] = df["sexo"].map(SEX_LABEL)

bins = [11, 17, 24, 34, 44, 54, 64, 105]
bin_labels = ["12-17", "18-24", "25-34", "35-44", "45-54", "55-64", "65+"]
df["age_group"] = pd.cut(df["edad"], bins=bins, labels=bin_labels)


def weighted_mean(g, value_col, weight_col="fe_cut"):
    d = g.dropna(subset=[value_col, weight_col])
    if len(d) < MIN_N:
        return None, len(d)
    return float(np.average(d[value_col], weights=d[weight_col])), len(d)


# --- 1. Daily average duration of paid work (t_to_ds) and domestic work
# (t_tdnr_ds), by sex and age group, in hours/day ---
duration_rows = []
for label, col in [("Trabajo remunerado", "t_to_ds"), ("Trabajo doméstico", "t_tdnr_ds")]:
    for age_group in bin_labels:
        for sex_code, sex_label in SEX_LABEL.items():
            g = df[(df["age_group"] == age_group) & (df["sexo"] == sex_code)]
            mean, n = weighted_mean(g, col)
            if mean is not None:
                duration_rows.append({
                    "activity": label, "age_group": age_group, "sex": sex_label,
                    "hours": round(mean, 2), "n": n
                })

with open(f"{OUT_DIR}/paid_domestic_duration_by_age_sex.json", "w", encoding="utf-8") as f:
    json.dump(duration_rows, f, ensure_ascii=False, indent=2)

# --- 2. Participation rate in domestic work (p_tdnr_ds), by sex and age
# group -- p_ variables are already 0/1 "did at least 1 minute" indicators,
# so a weighted mean directly gives the participation proportion ---
participation_rows = []
for age_group in bin_labels:
    for sex_code, sex_label in SEX_LABEL.items():
        g = df[(df["age_group"] == age_group) & (df["sexo"] == sex_code)]
        mean, n = weighted_mean(g, "p_tdnr_ds")
        if mean is not None:
            participation_rows.append({
                "age_group": age_group, "sex": sex_label,
                "share": round(mean, 4), "n": n
            })

with open(f"{OUT_DIR}/domestic_participation_by_age_sex.json", "w", encoding="utf-8") as f:
    json.dump(participation_rows, f, ensure_ascii=False, indent=2)

print("duration:", len(duration_rows), "rows")
print("participation:", len(participation_rows), "rows")
