"""
Build the "a day unfolding" dataset for oneday.qmd, from Argentina's
public ENUT 2021 (INDEC) time-use diary -- a genuine 10-minute-interval
diary, unlike ENUT Chile which only has daily totals per activity.

Fully public data, no confidentiality restrictions -- downloaded via
INDEC's open-data CKAN API (see web-scraping.qmd for the general pattern):
  https://www.indec.gob.ar/ftp/cuadros/menusuperior/enut/enut2021_diario.zip
  https://www.indec.gob.ar/ftp/cuadros/menusuperior/enut/enut2021_base.zip

Usage (from project root):
  pip install pandas numpy
  python scripts/build_argentina_oneday.py path/to/enut_ar_dir
"""

import sys
import json

import numpy as np
import pandas as pd

DATA_DIR = sys.argv[1] if len(sys.argv) > 1 else "."
OUT_DIR = "data/enut_ar"

SEX_LABEL = {1: "Mujeres", 2: "Varones"}

# Grupo (7 top-level categories) per ENUT 2021's CAUTAL code table
# (enut2021_cautal.xlsx) -- codes not listed fall back to "Sin clasificar".
GRUPO_MAP = {
    11: "Trabajo", 12: "Trabajo", 13: "Trabajo", 14: "Trabajo", 2: "Trabajo",
    411: "Cuidado", 412: "Cuidado", 413: "Cuidado", 414: "Cuidado", 419: "Cuidado",
    421: "Cuidado", 422: "Cuidado", 423: "Cuidado", 429: "Cuidado",
    431: "Cuidado", 432: "Cuidado", 433: "Cuidado", 439: "Cuidado",
    441: "Cuidado", 442: "Cuidado", 443: "Cuidado", 449: "Cuidado",
    31: "Domésticas", 32: "Domésticas", 33: "Domésticas", 34: "Domésticas",
    35: "Domésticas", 36: "Domésticas", 37: "Domésticas",
    911: "Personal", 912: "Personal", 914: "Personal",
    921: "Personal", 922: "Personal", 923: "Personal", 61: "Personal", 62: "Personal",
    711: "Ocio", 712: "Ocio", 72: "Ocio", 73: "Ocio", 74: "Ocio",
    81: "Ocio", 82: "Ocio", 83: "Ocio", 84: "Ocio", 85: "Ocio",
    52: "Voluntarias", 53: "Voluntarias", 54: "Voluntarias", 55: "Voluntarias",
    999: "Sin clasificar",
}

diario = pd.read_csv(f"{DATA_DIR}/enut2021_diario.txt", sep="|", encoding="latin1")
base = pd.read_csv(f"{DATA_DIR}/enut2021_base.txt", sep="|", encoding="latin1")

d = diario.merge(base[["ID", "N_MIEMBRO", "SEXO_SEL", "WPER"]], on=["ID", "N_MIEMBRO"], how="left")
d["sex"] = d["SEXO_SEL"].map(SEX_LABEL)
d["grupo"] = d["ACTIVIDAD_1"].map(GRUPO_MAP).fillna("Sin clasificar")
d["time"] = d["ACTIVIDAD_HORA"].apply(lambda h: f"{h:02d}") + ":" + d["ACTIVIDAD_MINUTO"].apply(lambda m: f"{m:02d}")
d = d.dropna(subset=["sex", "WPER"])

grouped = d.groupby(["time", "sex", "grupo"])["WPER"].sum().reset_index()
totals = grouped.groupby(["time", "sex"])["WPER"].transform("sum")
grouped["pct"] = (grouped["WPER"] / totals).round(4)
result = grouped[["time", "sex", "grupo", "pct"]].sort_values(["time", "sex", "grupo"])

rows = result.to_dict(orient="records")

import os
os.makedirs(OUT_DIR, exist_ok=True)
out_path = f"{OUT_DIR}/oneday_by_sex.json"
with open(out_path, "w", encoding="utf-8") as f:
    json.dump(rows, f, ensure_ascii=False, indent=2)

print(f"Wrote {len(rows)} rows to {out_path}")

# --- overall (all sexes combined), with a stable "n per 1000" allocation
# per time slot -- largest-remainder rounding so counts always sum to 1000 ---
overall = d.groupby(["time", "grupo"])["WPER"].sum().reset_index()
overall_totals = overall.groupby("time")["WPER"].transform("sum")
overall["pct"] = overall["WPER"] / overall_totals

def allocate_1000(group):
    raw = group["pct"] * 1000
    floor_n = np.floor(raw).astype(int)
    remainder = 1000 - floor_n.sum()
    order = (raw - floor_n).sort_values(ascending=False).index
    floor_n.loc[order[:remainder]] += 1
    group = group.copy()
    group["n"] = floor_n
    return group

overall = overall.groupby("time", group_keys=False).apply(allocate_1000)
overall["pct"] = overall["pct"].round(4)
overall = overall[["time", "grupo", "pct", "n"]].sort_values(["time", "grupo"])

overall_rows = overall.to_dict(orient="records")
out_path_overall = f"{OUT_DIR}/oneday_overall.json"
with open(out_path_overall, "w", encoding="utf-8") as f:
    json.dump(overall_rows, f, ensure_ascii=False, indent=2)

print(f"Wrote {len(overall_rows)} rows to {out_path_overall}")
