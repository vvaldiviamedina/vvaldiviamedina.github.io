# Build "daily average duration of paid work and domestic work, by sex and
# age group" and "participation rate in domestic work" -- replicating
# genderpulse.md's charts, using Chile's public ENUT 2023 data.
#
# Usage (from project root):
#   install.packages(c("dplyr", "jsonlite"))
#   Rscript scripts/build_paid_domestic_work.R path/to/250403-ii-enut-bdd-r-v2.RDS

suppressPackageStartupMessages({
  library(dplyr)
  library(jsonlite)
})

args <- commandArgs(trailingOnly = TRUE)
SRC <- if (length(args) >= 1) args[1] else "250403-ii-enut-bdd-r-v2.RDS"
OUT_DIR <- "data/enut"
MIN_N <- 30

df <- readRDS(SRC)

sex_label <- c(`1` = "Hombres", `2` = "Mujeres")
df$sex <- sex_label[as.character(df$sexo)]

df <- df |>
  mutate(age_group = cut(
    edad,
    breaks = c(11, 17, 24, 34, 44, 54, 64, 105),
    labels = c("12-17", "18-24", "25-34", "35-44", "45-54", "55-64", "65+")
  ))

# --- 1. Daily average duration of paid work (t_to_ds) and domestic work
# (t_tdnr_ds), by sex and age group, in hours/day ---
duration <- bind_rows(
  df |> transmute(activity = "Trabajo remunerado", age_group, sex, value = t_to_ds, w = fe_cut),
  df |> transmute(activity = "Trabajo doméstico", age_group, sex, value = t_tdnr_ds, w = fe_cut)
) |>
  filter(!is.na(value), !is.na(w), !is.na(age_group)) |>
  group_by(activity, age_group, sex) |>
  summarise(hours = round(weighted.mean(value, w), 2), n = n(), .groups = "drop") |>
  filter(n >= MIN_N)

write_json(duration, file.path(OUT_DIR, "paid_domestic_duration_by_age_sex.json"), auto_unbox = TRUE, pretty = TRUE)

# --- 2. Participation rate in domestic work (p_tdnr_ds) -- p_ variables are
# already 0/1 "did at least 1 minute" indicators, so a weighted mean directly
# gives the participation proportion ---
participation <- df |>
  filter(!is.na(p_tdnr_ds), !is.na(fe_cut), !is.na(age_group)) |>
  group_by(age_group, sex) |>
  summarise(share = round(weighted.mean(p_tdnr_ds, fe_cut), 4), n = n(), .groups = "drop") |>
  filter(n >= MIN_N)

write_json(participation, file.path(OUT_DIR, "domestic_participation_by_age_sex.json"), auto_unbox = TRUE, pretty = TRUE)

cat("duration:", nrow(duration), "rows\n")
cat("participation:", nrow(participation), "rows\n")
