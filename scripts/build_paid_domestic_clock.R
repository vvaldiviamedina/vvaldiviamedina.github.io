# Build data for the GenderPulse.md-style clock/wheel visualization of
# paid work vs domestic work, using Chile's public ENUT 2023 data.
#
# Age groups match the reference (genderpulse.md) exactly: Total, 15-24,
# 25-64, 65+.
#
# Usage (from project root):
#   install.packages(c("dplyr", "jsonlite"))
#   Rscript scripts/build_paid_domestic_clock.R path/to/250403-ii-enut-bdd-r-v2.RDS

suppressPackageStartupMessages({
  library(dplyr)
  library(jsonlite)
})

args <- commandArgs(trailingOnly = TRUE)
SRC <- if (length(args) >= 1) args[1] else "250403-ii-enut-bdd-r-v2.RDS"
OUT_DIR <- "data/enut"
MIN_N <- 30

df <- readRDS(SRC)
df <- df[df$edad >= 15, ]  # match the reference's age floor

sex_label <- c(`1` = "Hombres", `2` = "Mujeres")
df$sex <- sex_label[as.character(df$sexo)]

age_tab_of <- function(edad) {
  case_when(
    edad >= 15 & edad <= 24 ~ "15-24",
    edad >= 25 & edad <= 64 ~ "25-64",
    edad >= 65 ~ "65+"
  )
}
df$age_tab_specific <- age_tab_of(df$edad)

wmean_n <- function(value, w) {
  ok <- !is.na(value) & !is.na(w)
  if (sum(ok) < MIN_N) return(c(mean = NA, n = sum(ok)))
  c(mean = weighted.mean(value[ok], w[ok]), n = sum(ok))
}

age_tabs <- c("Total", "15-24", "25-64", "65+")

# --- 1. Daily average duration of paid work and domestic work ---
duration_rows <- list()
activities <- list(c("Trabajo remunerado", "t_to_ds"), c("Trabajo doméstico", "t_tdnr_ds"))
for (act in activities) {
  label <- act[1]; col <- act[2]
  for (age_tab in age_tabs) {
    sub <- if (age_tab == "Total") df else df[df$age_tab_specific == age_tab, ]
    for (sc in names(sex_label)) {
      g <- sub[sub$sexo == as.numeric(sc), ]
      r <- wmean_n(g[[col]], g$fe_cut)
      if (!is.na(r["mean"])) {
        duration_rows[[length(duration_rows) + 1]] <- list(
          activity = label, age_tab = age_tab, sex = sex_label[[sc]],
          hours = round(unname(r["mean"]), 2), n = unname(r["n"])
        )
      }
    }
  }
}
write_json(duration_rows, file.path(OUT_DIR, "clock_duration.json"), auto_unbox = TRUE, pretty = TRUE)

# --- 2. Participation rate in each domestic sub-activity ---
subact <- list(
  c("psc", "Preparar y servir comida"),
  c("lv", "Limpiar la vivienda"),
  c("lrc", "Lavar y planchar ropa"),
  c("mrm", "Mantenimiento del hogar"),
  c("admnhog", "Administrar el hogar"),
  c("comphog", "Hacer compras"),
  c("cmp", "Cuidar mascotas y plantas")
)
participation_rows <- list()
for (sa in subact) {
  code <- sa[1]; label <- sa[2]
  col <- paste0("p_tdnr_", code, "_ds")
  for (age_tab in age_tabs) {
    sub <- if (age_tab == "Total") df else df[df$age_tab_specific == age_tab, ]
    for (sc in names(sex_label)) {
      g <- sub[sub$sexo == as.numeric(sc), ]
      r <- wmean_n(g[[col]], g$fe_cut)
      if (!is.na(r["mean"])) {
        participation_rows[[length(participation_rows) + 1]] <- list(
          activity_code = code, activity = label, age_tab = age_tab,
          sex = sex_label[[sc]], share = round(unname(r["mean"]), 4), n = unname(r["n"])
        )
      }
    }
  }
}
write_json(participation_rows, file.path(OUT_DIR, "clock_participation.json"), auto_unbox = TRUE, pretty = TRUE)

cat("duration:", length(duration_rows), "rows\n")
cat("participation:", length(participation_rows), "rows\n")
