# Rebuild the aggregated JSON files used by uso-del-tiempo.qmd -- R version
# of build_enut_data.py. Produces the same output files.
#
# Not run against the real dataset in this repo (no R available in the
# environment that built this site) -- treat as a direct translation to
# review/adapt rather than a verified script.
#
# 1. Download & unzip the II ENUT 2023 R database (see build_enut_data.py
#    or web-scraping.qmd for the direct URL and how it was found).
# 2. From the project root:
#    install.packages(c("dplyr", "tidyr", "jsonlite"))
#    Rscript scripts/build_enut_data.R path/to/250403-ii-enut-bdd-r-v2.RDS

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(jsonlite)
})

args <- commandArgs(trailingOnly = TRUE)
SRC <- if (length(args) >= 1) args[1] else "250403-ii-enut-bdd-r-v2.RDS"
OUT_DIR <- "data/enut"
MIN_N <- 30

df <- readRDS(SRC)  # base R -- .RDS is R's native format, no package needed

sex_label <- c(`1` = "Hombres", `2` = "Mujeres")
df$sexo_label <- sex_label[as.character(df$sexo)]

# --- 1. Daily time balance by sex, across life domains ---
domains <- tribble(
  ~col,          ~label,
  "t_to_ds",     "Trabajo remunerado",
  "t_tnr_ds",    "Trabajo no remunerado",
  "t_cpaf_ds",   "Cuidado personal",
  "t_ed_ds",     "Educación",
  "t_vsyo_ds",   "Ocio y vida social"
)

balance <- df |>
  select(sexo_label, fe_cut, all_of(domains$col)) |>
  pivot_longer(all_of(domains$col), names_to = "col", values_to = "value") |>
  left_join(domains, by = "col") |>
  filter(!is.na(value), !is.na(fe_cut)) |>
  group_by(domain = label, sex = sexo_label) |>
  summarise(hours = round(weighted.mean(value, fe_cut), 2), n = n(), .groups = "drop") |>
  filter(n >= MIN_N)

write_json(balance, file.path(OUT_DIR, "time_balance_by_sex.json"), auto_unbox = TRUE, pretty = TRUE)

# --- 2. Unpaid work breakdown by sex ---
subtypes <- tribble(
  ~col,          ~label,
  "t_tcnr_ds",   "Trabajo de cuidados",
  "t_tdnr_ds",   "Trabajo doméstico",
  "t_tvaoh_ds",  "Voluntariado / ayuda a otros hogares"
)

breakdown <- df |>
  select(sexo_label, fe_cut, all_of(subtypes$col)) |>
  pivot_longer(all_of(subtypes$col), names_to = "col", values_to = "value") |>
  left_join(subtypes, by = "col") |>
  filter(!is.na(value), !is.na(fe_cut)) |>
  group_by(category = label, sex = sexo_label) |>
  summarise(hours = round(weighted.mean(value, fe_cut), 2), n = n(), .groups = "drop") |>
  filter(n >= MIN_N)

write_json(breakdown, file.path(OUT_DIR, "unpaid_breakdown_by_sex.json"), auto_unbox = TRUE, pretty = TRUE)

# --- 3. Unpaid work gap by region ---
region_gap <- df |>
  filter(!is.na(t_tnr_ds), !is.na(fe_cut)) |>
  group_by(region = glosa_region, sex = sexo_label) |>
  summarise(hours = round(weighted.mean(t_tnr_ds, fe_cut), 2), n = n(), .groups = "drop") |>
  filter(n >= MIN_N) |>
  pivot_wider(names_from = sex, values_from = c(hours, n)) |>
  filter(!is.na(hours_Hombres), !is.na(hours_Mujeres)) |>
  mutate(gap = round(hours_Mujeres - hours_Hombres, 2)) |>
  arrange(desc(gap))

write_json(region_gap, file.path(OUT_DIR, "unpaid_gap_by_region.json"), auto_unbox = TRUE, pretty = TRUE)

# --- 4. Unpaid work by age group and sex (life-cycle pattern) ---
df <- df |>
  mutate(age_group = cut(
    edad,
    breaks = c(11, 17, 24, 34, 44, 54, 64, 105),
    labels = c("12-17", "18-24", "25-34", "35-44", "45-54", "55-64", "65+")
  ))

age_df <- df |>
  filter(!is.na(t_tnr_ds), !is.na(fe_cut), !is.na(age_group)) |>
  group_by(age_group, sex = sexo_label) |>
  summarise(hours = round(weighted.mean(t_tnr_ds, fe_cut), 2), n = n(), .groups = "drop") |>
  filter(n >= MIN_N)

write_json(age_df, file.path(OUT_DIR, "unpaid_by_age_sex.json"), auto_unbox = TRUE, pretty = TRUE)

cat("balance:", nrow(balance), "rows\n")
cat("breakdown:", nrow(breakdown), "rows\n")
cat("region:", nrow(region_gap), "rows\n")
cat("age:", nrow(age_df), "rows\n")
