# Build the "a day unfolding" dataset for oneday.qmd, from Argentina's
# public ENUT 2021 (INDEC) time-use diary -- a genuine 10-minute-interval
# diary, unlike ENUT Chile which only has daily totals per activity.
#
# Fully public data (no confidentiality restrictions): downloaded via
# INDEC's open-data CKAN API, see web-scraping.qmd for the general pattern.
#
# 1. Download & unzip:
#    https://www.indec.gob.ar/ftp/cuadros/menusuperior/enut/enut2021_diario.zip
#    https://www.indec.gob.ar/ftp/cuadros/menusuperior/enut/enut2021_base.zip
# 2. From the project root:
#    install.packages(c("dplyr", "tidyr", "jsonlite", "readr"))
#    Rscript scripts/build_argentina_oneday.R path/to/enut_ar_dir

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(readr)
  library(jsonlite)
})

args <- commandArgs(trailingOnly = TRUE)
DATA_DIR <- if (length(args) >= 1) args[1] else "."
OUT_DIR <- "data/enut_ar"

diario <- read_delim(file.path(DATA_DIR, "enut2021_diario.txt"), delim = "|", show_col_types = FALSE)
base <- read_delim(file.path(DATA_DIR, "enut2021_base.txt"), delim = "|", show_col_types = FALSE)

sex_label <- c(`1` = "Mujeres", `2` = "Varones")

# Grupo -- ENUT 2021's own CAUTAL table has only 7 top-level groups, which
# lumps sleep/hygiene/eating/study/travel-for-those all into one "Personal"
# bucket. Split that out into finer categories (closer to the reference
# viz's own breakdown) using the underlying activity codes directly.
# "Traslados" (Traveling) unifies travel-purpose codes from every group
# (commute, care-related trips, personal/study trips) into one category,
# same as the reference visualization -- otherwise it's a near-empty sliver.
grupo_map <- c(
  `11` = "Trabajo", `12` = "Trabajo", `13` = "Trabajo", `2` = "Trabajo",
  `14` = "Traslados",
  `411` = "Cuidado", `412` = "Cuidado", `413` = "Cuidado", `419` = "Cuidado",
  `421` = "Cuidado", `422` = "Cuidado", `429` = "Cuidado",
  `431` = "Cuidado", `432` = "Cuidado", `439` = "Cuidado",
  `441` = "Cuidado", `442` = "Cuidado", `449` = "Cuidado",
  `414` = "Traslados", `423` = "Traslados", `433` = "Traslados", `443` = "Traslados",
  `31` = "Domésticas", `32` = "Domésticas", `33` = "Domésticas", `34` = "Domésticas",
  `35` = "Domésticas", `36` = "Domésticas", `37` = "Domésticas",
  `922` = "Dormir",
  `911` = "Cuidado personal", `912` = "Cuidado personal", `923` = "Cuidado personal",
  `921` = "Comer y beber",
  `914` = "Traslados", `62` = "Traslados",
  `61` = "Educación",
  `711` = "Ocio", `712` = "Ocio", `72` = "Ocio", `73` = "Ocio", `74` = "Ocio",
  `81` = "Ocio", `82` = "Ocio", `83` = "Ocio", `84` = "Ocio", `85` = "Ocio",
  `52` = "Voluntarias", `53` = "Voluntarias", `54` = "Voluntarias", `55` = "Voluntarias",
  `999` = "Sin clasificar"
)

d <- diario |>
  left_join(base |> select(ID, N_MIEMBRO, SEXO_SEL, WPER), by = c("ID", "N_MIEMBRO")) |>
  mutate(
    sex = unname(sex_label[as.character(SEXO_SEL)]),
    grupo = unname(grupo_map[as.character(ACTIVIDAD_1)]),
    grupo = ifelse(is.na(grupo), "Sin clasificar", grupo),
    time = sprintf("%02d:%02d", as.integer(ACTIVIDAD_HORA), as.integer(ACTIVIDAD_MINUTO))
  ) |>
  filter(!is.na(sex), !is.na(WPER))

result <- d |>
  group_by(time, sex, grupo) |>
  summarise(w = sum(WPER), .groups = "drop") |>
  group_by(time, sex) |>
  mutate(pct = round(w / sum(w), 4)) |>
  ungroup() |>
  select(time, sex, grupo, pct) |>
  arrange(time, sex, grupo)

dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)
out_path <- file.path(OUT_DIR, "oneday_by_sex.json")
write_json(result, out_path, auto_unbox = TRUE, pretty = TRUE)

cat("Wrote", nrow(result), "rows to", normalizePath(out_path), "\n")

# --- overall (all sexes combined), with a stable "n per 1000" allocation
# per time slot -- largest-remainder rounding so counts always sum to 1000 ---
allocate_1000 <- function(pct) {
  raw <- pct * 1000
  n <- floor(raw)
  remainder <- 1000 - sum(n)
  if (remainder > 0) {
    order <- order(raw - n, decreasing = TRUE)
    n[order[seq_len(remainder)]] <- n[order[seq_len(remainder)]] + 1
  }
  n
}

overall <- d |>
  group_by(time, grupo) |>
  summarise(w = sum(WPER), .groups = "drop") |>
  group_by(time) |>
  mutate(pct = round(w / sum(w), 4), n = allocate_1000(w / sum(w))) |>
  ungroup() |>
  select(time, grupo, pct, n) |>
  arrange(time, grupo)

out_path_overall <- file.path(OUT_DIR, "oneday_overall.json")
write_json(overall, out_path_overall, auto_unbox = TRUE, pretty = TRUE)

cat("Wrote", nrow(overall), "rows to", normalizePath(out_path_overall), "\n")
