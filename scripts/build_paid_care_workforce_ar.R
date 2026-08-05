# Run this LOCALLY, wherever Argentina's ENUT microdata is reachable. It
# never needs to leave your machine -- this script only ever writes out a
# small aggregate JSON (a handful of proportions + confidence intervals),
# never any row-level data. Copy the resulting file into data/enut_ar/ in
# the site repo (or hand it back) once it's built.
#
# This mirrors scripts/build_paid_care_workforce.R (the Chile version)
# exactly in structure and output shape, so it can slot into the existing
# country selector on the "Cuidado remunerado" pages without touching any
# chart code. What's DIFFERENT here is the classification step -- Chile's
# script keys off ciuo_2d (CIUO-08 occupation, 2-digit) and caenes_1d
# (CAENES industry, 1-digit), both Chile-specific coding schemes. Before
# this script will run, you need to fill in:
#
#   1. SRC below -- path to Argentina's ENUT microdata.
#   2. The variable names for occupation and industry/branch in that file
#      (see "TODO" comments below) -- check the codebook for something
#      like CODE_OCUP / CALIFICACION (occupation) and CLANAE / RAMA
#      (industry). Argentina's household surveys (EPH-based, which ENUT
#      usually follows) often carry occupation coded to CAES or a national
#      CNO scheme rather than ISCO-08 directly -- confirm which one.
#   3. The actual code lists for each cw_d bucket below. Chile's codes
#      (22, 23, 26, 32, 53 for occupation; 16, 17, 20 for industry) are
#      ISCO-08 sub-major groups (health/teaching/legal-social-cultural
#      professionals & associates, personal care workers) and CAENES
#      branches (education, health, social assistance) respectively --
#      note Chile's own script also includes occupation code 36, but
#      that's a Chile-specific adaptation on top of ISCO-08, not a
#      standard sub-major group, so it has no equivalent to carry over to
#      any other country and is deliberately excluded from the list above.
#      The DEFINITION is "professionals/associates in health, education,
#      social work, or personal care occupations, working within or
#      outside the care-sector industries." You need Argentina's own code
#      values for that same definition, not Chile's numbers. If Argentina's
#      occupation variable is already ISCO-08-coded, the SAME numeric
#      sub-major-group codes should carry over unchanged; if it's CNO or
#      another national scheme, you'll need a crosswalk.
#
# Usage:
#   Rscript scripts/build_paid_care_workforce_ar.R

suppressPackageStartupMessages({
  library(dplyr)
  library(survey)
  library(jsonlite)
  library(haven)
})

# TODO: path to Argentina's ENUT microdata (RDS, dta, sav, whatever format
# it's distributed in -- haven::read_dta()/read_sav() if not RDS).
SRC <- "PATH/TO/ARGENTINA/ENUT/MICRODATA"

OUT_DIR <- "data/enut_ar"
# Argentina's ENUT codes sex as SEXO_SEL, 1=Mujeres/2=Varones -- opposite
# numeric direction from Chile's ENUT (1=Hombres/2=Mujeres), confirmed
# when oneday.qmd was built from this same source. That page displays
# "Varones" as its own standalone label, which is fine there -- but the
# paid-care-workforce charts this script feeds into share one `allLong`
# dataset across countries with a hardcoded color domain of
# ["Hombres", "Mujeres"] (see cuidado-remunerado-formas.qmd /
# cuidado-remunerado-comparado.qmd). So here "Varones" is relabeled to
# "Hombres" to match that shared domain -- without this, Argentina's rows
# would fall outside the color scale and render unstyled.
sex_label <- c(`1` = "Mujeres", `2` = "Hombres")

enut <- readRDS(SRC) # TODO: swap for haven::read_dta()/read_sav() if needed

# --- TODO: fill in Argentina's variable names and code lists before running.
# Left as undefined names on purpose: sourcing this file will fail loudly
# with "object not found" until you fill these in, instead of silently
# running against the wrong columns.
occ_var <- NULL   # occupation code variable name, e.g. "codigo_ocupacion"
ind_var <- NULL   # industry/branch code variable name, e.g. "clanae"
emp_var <- NULL   # employment-status variable -- equivalent of Chile's `cae`
emp_code <- NULL  # value(s) of emp_var meaning "employed"

# Occupation code lists for each bucket -- Chile's are ISCO-08 sub-major
# groups (22/23/26/32/53 = health, teaching, legal-social-cultural
# professionals & associates, personal care workers). If Argentina's
# occupation variable is ISCO-08-coded, these same values likely carry
# over; if it's CNO or another national scheme, translate them first.
occ_codes_tcr <- NULL       # professional-level care occupations
occ_codes_habilitadores <- NULL  # associate/support-level care occupations
# Industry/branch codes -- Chile's are CAENES 1-digit (16/17/20 = education,
# health, social assistance). Use Argentina's CLANAE (or equivalent)
# codes for the same three industries.
ind_codes_care_sector <- NULL

# Pre-compute a plain, literally-named boolean column for the employment
# filter, then use THAT everywhere below instead of `.data[[emp_var]]`
# directly. .data[[...]] is tidy-eval syntax mutate()/case_when() understand,
# but subset() (and survey::subset.survey.design, used further down) use
# base R's NSE, which does NOT know what .data is -- this bug bit both the
# Colombia and Mexico versions of this script (subset(diseno,
# .data[[emp_var]] %in% emp_code) fails silently, leaving dise_ocup never
# created, instead of throwing where the actual problem is) before being
# caught and fixed there. Fixed here too, preemptively.
enut <- enut |> mutate(is_employed = .data[[emp_var]] %in% emp_code)

# --- classification of paid care work (occupation x sector) ---
# Mirrors Chile's logic 1:1 -- only the variable/code assignments above
# need to change. Do NOT change the structure (cw_d values 11/12/21/31/91
# and what they feed into) unless the Chile definition itself changes,
# since the two countries need to stay comparable.
enut <- enut |>
  mutate(
    cw_d = case_when(
      !is_employed ~ NA,
      .data[[occ_var]] %in% occ_codes_tcr & .data[[ind_var]] %in% ind_codes_care_sector ~ 11,
      .data[[occ_var]] %in% occ_codes_tcr & !.data[[ind_var]] %in% ind_codes_care_sector ~ 12,
      .data[[occ_var]] %in% occ_codes_habilitadores & .data[[ind_var]] %in% ind_codes_care_sector ~ 21,
      !.data[[occ_var]] %in% c(occ_codes_tcr, occ_codes_habilitadores) & .data[[ind_var]] %in% ind_codes_care_sector ~ 31,
      TRUE ~ 91
    )
  ) |>
  mutate(
    broad_unk_con = case_when(
      !is_employed ~ NA,
      cw_d %in% c(11, 12, 21, 31) ~ 1,
      TRUE ~ 0
    ),
    slightly_unk_con = case_when(
      !is_employed ~ NA,
      cw_d %in% c(11, 12, 21) ~ 1,
      cw_d %in% 31 ~ 0,
      TRUE ~ 0
    ),
    cwe = case_when(
      !is_employed ~ NA,
      cw_d %in% c(11, 12) ~ 1,
      TRUE ~ 0
    )
  ) |>
  mutate(
    tc_sc = case_when(!is_employed ~ NA, cw_d == 11 ~ 1, TRUE ~ 0),
    tc_nsc = case_when(!is_employed ~ NA, cw_d == 12 ~ 1, TRUE ~ 0),
    habilitadores = case_when(!is_employed ~ NA, cw_d == 21 ~ 1, TRUE ~ 0),
    facilitadores = case_when(!is_employed ~ NA, cw_d == 31 ~ 1, TRUE ~ 0)
  )

# The person-level weight is WPER -- confirmed already, since
# build_argentina_oneday.R/.py use it. Strata and PSU (cluster) variables
# are NOT confirmed: the oneday.qmd build only needed weighted point
# estimates (simple sum-of-weights), not standard errors, so it never
# needed strata/PSU and neither script references them. This script does
# need them for svyby()'s CIs -- check the codebook for ENUT's design
# variables (may not use the same names as Chile's varstrat/varunit, and
# it's possible ENUT's public microdata doesn't expose replicate weights
# or PSU/strata at all, in which case svydesign(ids=~1) with just weights
# -- no clustering -- is the fallback, though CIs from that will
# understate true sampling variance).
strata_var <- NULL  # TODO: strata variable name, or NULL if not clustered
psu_var <- NULL     # TODO: primary sampling unit variable name

diseno <- if (is.null(strata_var) && is.null(psu_var)) {
  svydesign(data = enut, ids = ~1, weights = ~WPER)
} else {
  svydesign(data = enut, strata = as.formula(paste0("~", strata_var)), ids = as.formula(paste0("~", psu_var)), weights = ~WPER)
}
options(survey.lonely.psu = "certainty")
dise_ocup <- subset(diseno, is_employed)

# --- extract a svyby(FUN = svymean) result into a tidy list of rows ---
extract_rows <- function(svyby_result, indicator_col, definition_label) {
  d <- as.data.frame(svyby_result)
  lapply(seq_len(nrow(d)), function(i) {
    row <- d[i, ]
    list(
      definition = definition_label,
      sex = unname(sex_label[as.character(row$SEXO_SEL)]),
      share = round(row[[indicator_col]], 4),
      se = round(row[["se"]], 4),
      ci_low = round(row[["ci_l"]], 4),
      ci_high = round(row[["ci_u"]], 4)
    )
  })
}

res_cwe <- svyby(~cwe, by = ~SEXO_SEL, design = dise_ocup, FUN = svymean, vartype = c("se", "ci"), na.rm = TRUE)
res_slight <- svyby(~slightly_unk_con, by = ~SEXO_SEL, design = dise_ocup, FUN = svymean, vartype = c("se", "ci"), na.rm = TRUE)
res_broad <- svyby(~broad_unk_con, by = ~SEXO_SEL, design = dise_ocup, FUN = svymean, vartype = c("se", "ci"), na.rm = TRUE)

rows <- c(
  extract_rows(res_cwe, "cwe", "Trabajo de cuidados remunerados"),
  extract_rows(res_slight, "slightly_unk_con", "TCR + Habilitadores"),
  extract_rows(res_broad, "broad_unk_con", "TCR + Habilitadores + Facilitadores")
)

dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)
out_path <- file.path(OUT_DIR, "paid_care_workforce_by_sex_ar.json")
write_json(rows, out_path, auto_unbox = TRUE, pretty = TRUE)

cat("Wrote", length(rows), "rows to", out_path, "\n")

# --- individual cw_d categories (mutually exclusive), by sex ---
res_11 <- svyby(~tc_sc, by = ~SEXO_SEL, design = dise_ocup, FUN = svymean, vartype = c("se", "ci"), na.rm = TRUE)
res_12 <- svyby(~tc_nsc, by = ~SEXO_SEL, design = dise_ocup, FUN = svymean, vartype = c("se", "ci"), na.rm = TRUE)
res_21 <- svyby(~habilitadores, by = ~SEXO_SEL, design = dise_ocup, FUN = svymean, vartype = c("se", "ci"), na.rm = TRUE)
res_31 <- svyby(~facilitadores, by = ~SEXO_SEL, design = dise_ocup, FUN = svymean, vartype = c("se", "ci"), na.rm = TRUE)

rows_cwd <- c(
  extract_rows(res_11, "tc_sc", "TCR dentro del sector"),
  extract_rows(res_12, "tc_nsc", "TCR fuera del sector"),
  extract_rows(res_21, "habilitadores", "Habilitadores"),
  extract_rows(res_31, "facilitadores", "Facilitadores")
)

out_path_cwd <- file.path(OUT_DIR, "paid_care_workforce_cwd_by_sex_ar.json")
write_json(rows_cwd, out_path_cwd, auto_unbox = TRUE, pretty = TRUE)

cat("Wrote", length(rows_cwd), "rows to", normalizePath(out_path_cwd), "\n")
cat("Both files contain only aggregate proportions + CIs -- safe to hand off.\n")
