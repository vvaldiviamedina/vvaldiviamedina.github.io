# Run this LOCALLY, wherever the internal ENUT processing file is reachable.
# It never needs to leave your machine -- this script only ever writes out a
# small aggregate JSON (a handful of proportions + confidence intervals),
# never any row-level data. Copy the resulting file into data/enut/ in the
# site repo (or hand it back) once it's built.
#
# Usage:
#   Rscript scripts/build_paid_care_workforce.R
# (edit SRC below if the network path differs)

suppressPackageStartupMessages({
  library(dplyr)
  library(survey)
  library(jsonlite)
  library(haven)
})

SRC <- "//Buvmfswinp01/SEET_ENUT/ii_enut/5_procesamiento/5.6_ponderar/data/enut_5.6_ponderada.RDS"
OUT_DIR <- "data/enut"
sex_label <- c(`1` = "Hombres", `2` = "Mujeres")

enut <- readRDS(SRC)

# --- classification of paid care work (occupation x sector), as given ---
enut <- enut |>
  mutate(
    cw_d = case_when(
      !cae %in% 2 ~ NA,
      ciuo_2d %in% c(22, 23, 32, 36, 53) & caenes_1d %in% c(16, 17, 20) ~ 11,
      ciuo_2d %in% c(22, 23, 32, 36, 53) & !caenes_1d %in% c(16, 17, 20) ~ 12,
      ciuo_2d %in% c(51, 91, 94, 96) & caenes_1d %in% c(16, 17, 20) ~ 21,
      !ciuo_2d %in% c(22, 23, 32, 36, 53, 51, 91, 94, 96) & caenes_1d %in% c(16, 17, 20) ~ 31,
      TRUE ~ 91
    )
  ) |>
  mutate(
    broad_unk_con = case_when(
      !cae %in% 2 ~ NA,
      cw_d %in% c(11, 12, 21, 31) ~ 1,
      TRUE ~ 0
    ),
    slightly_unk_con = case_when(
      !cae %in% 2 ~ NA,
      cw_d %in% c(11, 12, 21) ~ 1,
      cw_d %in% 31 ~ 0,
      TRUE ~ 0
    ),
    cwe = case_when(
      !cae %in% 2 ~ NA,
      cw_d %in% c(11, 12) ~ 1,
      TRUE ~ 0
    )
  ) |>
  mutate(
    tc_sc = case_when(!cae %in% 2 ~ NA, cw_d == 11 ~ 1, TRUE ~ 0),
    tc_nsc = case_when(!cae %in% 2 ~ NA, cw_d == 12 ~ 1, TRUE ~ 0),
    habilitadores = case_when(!cae %in% 2 ~ NA, cw_d == 21 ~ 1, TRUE ~ 0),
    facilitadores = case_when(!cae %in% 2 ~ NA, cw_d == 31 ~ 1, TRUE ~ 0)
  )

enut <- subset(enut, tiempo == 1)

diseno <- svydesign(data = enut, strata = ~varstrat, ids = ~varunit, weights = ~fe_cut)
options(survey.lonely.psu = "certainty")
dise_ocup <- subset(diseno, cae %in% 2)

# --- extract a svyby(FUN = svymean) result into a tidy list of rows ---
extract_rows <- function(svyby_result, indicator_col, definition_label) {
  d <- as.data.frame(svyby_result)
  lapply(seq_len(nrow(d)), function(i) {
    row <- d[i, ]
    list(
      definition = definition_label,
      sex = unname(sex_label[as.character(row$sexo)]),
      share = round(row[[indicator_col]], 4),
      se = round(row[["se"]], 4),
      ci_low = round(row[["ci_l"]], 4),
      ci_high = round(row[["ci_u"]], 4)
    )
  })
}

res_cwe <- svyby(~cwe, by = ~sexo, design = dise_ocup, FUN = svymean, vartype = c("se", "ci"), na.rm = TRUE)
res_slight <- svyby(~slightly_unk_con, by = ~sexo, design = dise_ocup, FUN = svymean, vartype = c("se", "ci"), na.rm = TRUE)
res_broad <- svyby(~broad_unk_con, by = ~sexo, design = dise_ocup, FUN = svymean, vartype = c("se", "ci"), na.rm = TRUE)

rows <- c(
  extract_rows(res_cwe, "cwe", "Trabajo de cuidados remunerados"),
  extract_rows(res_slight, "slightly_unk_con", "TCR + Habilitadores"),
  extract_rows(res_broad, "broad_unk_con", "TCR + Habilitadores + Facilitadores")
)

dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)
out_path <- file.path(OUT_DIR, "paid_care_workforce_by_sex.json")
write_json(rows, out_path, auto_unbox = TRUE, pretty = TRUE)

cat("Wrote", length(rows), "rows to", file.path(OUT_DIR, "paid_care_workforce_by_sex.json"), "\n")
# --- individual cw_d categories (mutually exclusive), by sex ---
res_11 <- svyby(~tc_sc, by = ~sexo, design = dise_ocup, FUN = svymean, vartype = c("se", "ci"), na.rm = TRUE)
res_12 <- svyby(~tc_nsc, by = ~sexo, design = dise_ocup, FUN = svymean, vartype = c("se", "ci"), na.rm = TRUE)
res_21 <- svyby(~habilitadores, by = ~sexo, design = dise_ocup, FUN = svymean, vartype = c("se", "ci"), na.rm = TRUE)
res_31 <- svyby(~facilitadores, by = ~sexo, design = dise_ocup, FUN = svymean, vartype = c("se", "ci"), na.rm = TRUE)

rows_cwd <- c(
  extract_rows(res_11, "tc_sc", "Trabajo de cuidados en la ocupación dentro del sector de cuidados"),
  extract_rows(res_12, "tc_nsc", "Trabajo de cuidados en la ocupación fuera del sector de cuidados"),
  extract_rows(res_21, "habilitadores", "Habilitadores"),
  extract_rows(res_31, "facilitadores", "Facilitadores")
)

out_path_cwd <- file.path(OUT_DIR, "paid_care_workforce_cwd_by_sex.json")
write_json(rows_cwd, out_path_cwd, auto_unbox = TRUE, pretty = TRUE)

cat("Wrote", length(rows_cwd), "rows to", normalizePath(out_path_cwd), "\n")
cat("Both files contain only aggregate proportions + CIs -- safe to hand off.\n")
