"""
Build data for the GenderPulse.md-style clock/wheel visualization of
paid work vs domestic work, using Chile's public ENUT 2023 data.

Age groups match the reference (genderpulse.md) exactly: Total, 15-24,
25-64, 65+.

Usage (from project root):
  pip install pyreadr pandas numpy
  python scripts/build_paid_domestic_clock.py path/to/250403-ii-enut-bdd-r-v2.RDS
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
df = df[df["edad"] >= 15]  # match the reference's age floor

SEX_LABEL = {1.0: "Hombres", 2.0: "Mujeres"}
df["sex"] = df["sexo"].map(SEX_LABEL)

AGE_TABS = {
    "Total": lambda d: pd.Series(True, index=d.index),
    "15-24": lambda d: (d["edad"] >= 15) & (d["edad"] <= 24),
    "25-64": lambda d: (d["edad"] >= 25) & (d["edad"] <= 64),
    "65+": lambda d: d["edad"] >= 65,
}


def weighted_mean(g, value_col, weight_col="fe_cut"):
    d = g.dropna(subset=[value_col, weight_col])
    if len(d) < MIN_N:
        return None, len(d)
    return float(np.average(d[value_col], weights=d[weight_col])), len(d)


# --- 1. Daily average duration of paid work and domestic work, by sex
# and age tab, in hours/day ---
duration_rows = []
for label, col in [("Trabajo remunerado", "t_to_ds"), ("Trabajo doméstico", "t_tdnr_ds")]:
    for age_tab, mask_fn in AGE_TABS.items():
        sub = df[mask_fn(df)]
        for sex_code, sex_label in SEX_LABEL.items():
            g = sub[sub["sexo"] == sex_code]
            mean, n = weighted_mean(g, col)
            if mean is not None:
                duration_rows.append({
                    "activity": label, "age_tab": age_tab, "sex": sex_label,
                    "hours": round(mean, 2), "n": n
                })

with open(f"{OUT_DIR}/clock_duration.json", "w", encoding="utf-8") as f:
    json.dump(duration_rows, f, ensure_ascii=False, indent=2)

# --- 2. Participation rate in each domestic sub-activity, by sex and age
# tab -- Chile's own 7-category breakdown of "trabajo doméstico no
# remunerado" (not a 1:1 match to the reference's icon set, since that's
# Moldova's own classification, but this is the real Chilean equivalent) ---
SUBACT = [
    ("psc", "Preparar y servir comida"),
    ("lv", "Limpiar la vivienda"),
    ("lrc", "Lavar y planchar ropa"),
    ("mrm", "Mantenimiento del hogar"),
    ("admnhog", "Administrar el hogar"),
    ("comphog", "Hacer compras"),
    ("cmp", "Cuidar mascotas y plantas"),
]

participation_rows = []
for code, label in SUBACT:
    col = f"p_tdnr_{code}_ds"
    for age_tab, mask_fn in AGE_TABS.items():
        sub = df[mask_fn(df)]
        for sex_code, sex_label in SEX_LABEL.items():
            g = sub[sub["sexo"] == sex_code]
            mean, n = weighted_mean(g, col)
            if mean is not None:
                participation_rows.append({
                    "activity_code": code, "activity": label, "age_tab": age_tab,
                    "sex": sex_label, "share": round(mean, 4), "n": n
                })

with open(f"{OUT_DIR}/clock_participation.json", "w", encoding="utf-8") as f:
    json.dump(participation_rows, f, ensure_ascii=False, indent=2)

print("duration:", len(duration_rows), "rows")
print("participation:", len(participation_rows), "rows")
