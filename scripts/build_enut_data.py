"""
Rebuild the aggregated JSON files used by uso-del-tiempo.qmd.

The raw dataset is NOT included in this repo (it's ~14MB and not ours to
redistribute in bulk) -- only the small derived aggregates in data/enut/
are committed. To regenerate them:

1. Download the II ENUT 2023 database in R format from INE. The direct
   file URL (found by hovering the download link in a browser -- see
   web-scraping.qmd for how) is:
   https://www.ine.gob.cl/docs/default-source/uso-del-tiempo-tiempo-libre/bbdd/ii-enut/250403-ii-enut-bdd-r-v2.zip
2. Unzip it -- you'll get a single .RDS file.
3. From the project root, run:
   pip install pyreadr pandas numpy
   python scripts/build_enut_data.py path/to/250403-ii-enut-bdd-r-v2.RDS
"""

import json
import sys

import numpy as np
import pandas as pd
import pyreadr

SRC = sys.argv[1] if len(sys.argv) > 1 else "250403-ii-enut-bdd-r-v2.RDS"
OUT_DIR = "data/enut"

r = pyreadr.read_r(SRC)
df = list(r.values())[0]

SEX_LABEL = {1.0: "Hombres", 2.0: "Mujeres"}
df["sexo_label"] = df["sexo"].map(SEX_LABEL)

MIN_N = 30  # suppress cells with too few respondents


def weighted_mean(g, value_col, weight_col="fe_cut"):
    d = g.dropna(subset=[value_col, weight_col])
    if len(d) < MIN_N:
        return None, len(d)
    return float(np.average(d[value_col], weights=d[weight_col])), len(d)


# --- 1. Daily time balance by sex, across life domains ---
domains = [
    ("to", "t_to_ds", "Trabajo remunerado"),
    ("tnr", "t_tnr_ds", "Trabajo no remunerado"),
    ("cpaf", "t_cpaf_ds", "Cuidado personal"),
    ("ed", "t_ed_ds", "Educación"),
    ("vsyo", "t_vsyo_ds", "Ocio y vida social"),
]

balance = []
for code, col, label in domains:
    for sex_code, sex_label in SEX_LABEL.items():
        g = df[df["sexo"] == sex_code]
        mean, n = weighted_mean(g, col)
        if mean is not None:
            balance.append({"domain": label, "domain_code": code, "sex": sex_label, "hours": round(mean, 2), "n": n})

with open(f"{OUT_DIR}/time_balance_by_sex.json", "w", encoding="utf-8") as f:
    json.dump(balance, f, ensure_ascii=False, indent=2)

# --- 2. Unpaid work breakdown by sex ---
subtypes = [
    ("tcnr", "t_tcnr_ds", "Trabajo de cuidados"),
    ("tdnr", "t_tdnr_ds", "Trabajo doméstico"),
    ("tvaoh", "t_tvaoh_ds", "Voluntariado / ayuda a otros hogares"),
]

breakdown = []
for code, col, label in subtypes:
    for sex_code, sex_label in SEX_LABEL.items():
        g = df[df["sexo"] == sex_code]
        mean, n = weighted_mean(g, col)
        if mean is not None:
            breakdown.append({"category": label, "category_code": code, "sex": sex_label, "hours": round(mean, 2), "n": n})

with open(f"{OUT_DIR}/unpaid_breakdown_by_sex.json", "w", encoding="utf-8") as f:
    json.dump(breakdown, f, ensure_ascii=False, indent=2)

# --- 3. Unpaid work gap by region ---
region_rows = []
for region, g_region in df.groupby("glosa_region"):
    row = {"region": region}
    means = {}
    ok = True
    for sex_code, sex_label in SEX_LABEL.items():
        g = g_region[g_region["sexo"] == sex_code]
        mean, n = weighted_mean(g, "t_tnr_ds")
        if mean is None:
            ok = False
            break
        means[sex_label] = round(mean, 2)
        row[f"n_{sex_label}"] = n
    if ok:
        row.update(means)
        row["gap"] = round(means["Mujeres"] - means["Hombres"], 2)
        region_rows.append(row)

region_rows.sort(key=lambda r: r["gap"], reverse=True)
with open(f"{OUT_DIR}/unpaid_gap_by_region.json", "w", encoding="utf-8") as f:
    json.dump(region_rows, f, ensure_ascii=False, indent=2)

# --- 4. Unpaid work by age group and sex (life-cycle pattern) ---
bins = [11, 17, 24, 34, 44, 54, 64, 105]
bin_labels = ["12-17", "18-24", "25-34", "35-44", "45-54", "55-64", "65+"]
df["age_group"] = pd.cut(df["edad"], bins=bins, labels=bin_labels)

age_rows = []
for age_group in bin_labels:
    for sex_code, sex_label in SEX_LABEL.items():
        g = df[(df["age_group"] == age_group) & (df["sexo"] == sex_code)]
        mean, n = weighted_mean(g, "t_tnr_ds")
        if mean is not None:
            age_rows.append({"age_group": age_group, "sex": sex_label, "hours": round(mean, 2), "n": n})

with open(f"{OUT_DIR}/unpaid_by_age_sex.json", "w", encoding="utf-8") as f:
    json.dump(age_rows, f, ensure_ascii=False, indent=2)

print("balance:", len(balance), "rows")
print("breakdown:", len(breakdown), "rows")
print("region:", len(region_rows), "rows")
print("age:", len(age_rows), "rows")
