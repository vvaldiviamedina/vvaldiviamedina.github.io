# Run this LOCALLY. It never needs to leave your machine -- this script
# only ever writes out a small aggregate JSON (a handful of proportions +
# confidence intervals), never any row-level data. Copy the resulting file
# into data/enut_co/ in the site repo (or hand it back) once it's built.
#
# UNLIKE Chile's source (an internal-only INE network file), Colombia's
# ENUT microdata is public open data from DANE -- downloadable directly,
# no login or terms form needed (confirmed: the URL below returns the zip
# with a plain 200, no auth). This is ENUT 2024-2025 (DANE catalog 910).
# The zip contains modules ENUT_C1 through ENUT_C9 (plus ENUT_C7_1), each
# in three formats (.csv, .dta, .sav) -- confirmed by inspecting the
# archive's file listing (not its contents).
#
# What's needed is spread across THREE modules, joined on the standard DANE
# person key (DIRECTORIO + SECUENCIA_P + ORDEN, confirmed present in
# ENUT_C7 and expected in the others):
#   ENUT_C3   -- sex (P6020)
#   ENUT_C7   -- overall employment status (P1152)
#   ENUT_C7_1 -- occupation (P6370S3), industry (P6390S2)
#
# Catalog note: if DANE re-releases this and the numeric download ID (910
# / 24752) changes, re-derive it the same way I did: open
# https://microdatos.dane.gov.co/index.php/catalog/<id>/get-microdata in a
# browser, view source, and look for an onclick="mostrarModal(...)" whose
# second argument is the real .../download/<file_id> URL.
#
# This mirrors scripts/build_paid_care_workforce.R (the Chile version) in
# structure and output shape, so it slots into the existing country
# selector on the "Cuidado remunerado" pages without touching chart code.
#
# CONFIRMED (opened DANE's public data dictionary directly for catalog
# 910, and cross-checked against DANE's own CUOC/CIIU methodology docs --
# these are the SAME variable names as the 2020-2021 cycle, so they look
# stable across ENUT rounds):
#   - Sex: P6020 ("[D3]. ¿Cuál fue su sexo al nacer?", in ENUT_C3).
#     GEIH (DANE's other big household survey) codes this 1=Hombre,
#     2=Mujer -- SAME direction as Chile, no reversal needed (unlike
#     Argentina's ENUT, which turned out reversed). Inferred from GEIH's
#     well-established convention for the identical question/variable
#     name, not pixel-confirmed on an ENUT-specific value-label page
#     (DANE's site renders those via JS I couldn't get static values
#     from) -- cheap to sanity check once real data is loaded:
#     `table(enut$P6020)` should show two categories, and cross-referencing
#     against occupation shares by sex should look qualitatively like
#     Chile's (women overrepresented in TCR) if the direction is right.
#   - Employment status: P1152 ("[H1]. ¿En qué actividad ocupó ...la mayor
#     parte del tiempo la semana pasada?", in ENUT_C7) -- this is DANE's
#     standard labor-force-status question (same shape as GEIH's), where
#     1 = "Trabajando" by DANE/GEIH convention. Same caveat as sex: strong
#     prior confidence from convention, not a pixel-confirmed value-label
#     fetch -- verify with `table(enut$P1152)` before trusting emp_code
#     below blindly.
#   - Occupation: P6370S3 ("[H13]. Oficio actividad principal codificada",
#     in ENUT_C7_1). Colombia's ENUT codes occupation to CUOC (Clasificación
#     Única de Ocupaciones para Colombia), DANE's unification of CIUO-08
#     A.C. and the old CNO. Checked CUOC's own methodology doc (dane.gov.co/
#     files/sen/nomenclatura/cuoc/documento-clasificacion-unica-ocupaciones-
#     colombia-CUOC-2023.pdf): it's explicit that CUOC "mantiene la
#     comparabilidad internacional al conservar la estructura piramidal de
#     la CIUO 08 A.C. hasta el cuarto dígito, e incorpora un quinto dígito"
#     -- i.e. the 2-digit "subgrupo principal" level (43 of them) is
#     UNCHANGED CIUO-08, only a 5th digit is Colombia-specific. So Chile's
#     occupation code list (22, 23, 26, 32, 53 -- excluding 36, which is
#     itself a Chile-specific adaptation code with no CIUO-08 equivalent)
#     should transfer as-is at the 2-digit level -- good confidence here,
#     backed by DANE's own documentation, not just a guess.
#   - Industry/branch: P6390S2 ("[H15]. Rama de actividad principal
#     codificada", in ENUT_C7_1). CIIU Rev. 4 A.C. (dane.gov.co/files/sen/
#     nomenclatura/ciiu/CIIU_Rev_4_AC2022.pdf). Its published top level is
#     LETTER-coded Secciones (A-U: Educación = Sección P, Salud humana y
#     asistencia social = Sección Q), but per-person survey fields like
#     P6390S2 store the underlying NUMERIC code (división/grupo/clase), not
#     the section letter. CIIU Rev. 4's divisions under those two sections:
#     85 = Educación (Sección P); 86 = Atención de la salud humana, 87 =
#     Atención residencial, 88 = Asistencia social sin alojamiento (all
#     Sección Q) -- this numbering is from the international ISIC Rev.4
#     standard structure (which CIIU Rev.4 A.C. is explicitly adapted
#     from), not re-verified division-by-division against Colombia's own
#     published list -- worth a spot-check at clasificaciones.dane.gov.co.
#   - Weight (in ENUT_C7 and ENUT_C7_1): FEX_C ("Factor de expansión").
#   - Strata and PSU/UPM: checked ENUT_C1, ENUT_C2, and ENUT_C7_1's
#     dictionaries -- none of the three exposes a strata or PSU variable.
#     Consistent absence across three different files suggests this
#     public-use release genuinely doesn't include them (rather than them
#     just living in a file I haven't checked yet), so the unclustered
#     design below (weights only, no clustering) is the actual approach to
#     use, not a stopgap -- though it will understate true sampling
#     variance somewhat, same caveat as any unclustered survey design.
#
# Usage:
#   Rscript scripts/build_paid_care_workforce_co.R

suppressPackageStartupMessages({
  library(dplyr)
  library(survey)
  library(jsonlite)
  library(haven)
})

OUT_DIR <- "data/enut_co"
KEY <- c("DIRECTORIO", "SECUENCIA_P", "ORDEN")

# Confirmed working, no-auth download (92.5 MB zip).
ZIP_URL <- "https://microdatos.dane.gov.co/index.php/catalog/910/download/24752"
zip_path <- tempfile(fileext = ".zip")
download.file(ZIP_URL, zip_path, mode = "wb")
extract_dir <- tempfile()
unzip(zip_path, files = c("ENUT_C3.dta", "ENUT_C7.dta", "ENUT_C7_1.dta"), exdir = extract_dir)

c3 <- read_dta(file.path(extract_dir, "ENUT_C3.dta"))   |> select(all_of(KEY), P6020)
c7 <- read_dta(file.path(extract_dir, "ENUT_C7.dta"))   |> select(all_of(KEY), P1152)
c7_1 <- read_dta(file.path(extract_dir, "ENUT_C7_1.dta"))

# TODO: if any of these joins drop rows unexpectedly (check nrow() before
# vs. after), the key variables may not be named identically across all
# three files -- confirm with `names(c3)` etc. before assuming this works.
enut <- c7_1 |>
  left_join(c3, by = KEY) |>
  left_join(c7, by = KEY)

sex_var <- "P6020"  # CONFIRMED variable name; direction inferred from GEIH -- see note above
sex_label <- c(`1` = "Hombres", `2` = "Mujeres")

# P6370S3 (occupation) and P6390S2 (industry) both store more digits than
# the 2-digit level Chile's definition operates at (CUOC occupation goes to
# 5 digits, CIIU industry to 4) -- recode both to their leading 2 digits
# before matching, same convention as ciuo_2d/caenes_1d in Chile's script.
# as.character() first since these may come in as labelled/numeric with
# leading structure that %/% 1000 etc. would get wrong if digit count
# varies -- substr on the character form is robust to that either way.
enut <- enut |>
  mutate(
    occ_2d = as.integer(substr(as.character(.data[["P6370S3"]]), 1, 2)),
    ind_2d = as.integer(substr(as.character(.data[["P6390S2"]]), 1, 2))
  )

occ_var <- "occ_2d"   # derived above -- CIUO-2008 2-digit, same system as Chile
ind_var <- "ind_2d"   # derived above -- CIIU Rev.4 A.C. 2-digit división
emp_var <- "P1152"    # CONFIRMED variable name; "1" meaning inferred from DANE/GEIH convention -- see note above
emp_code <- c(1)

occ_codes_tcr <- c(22, 23, 26, 32, 53)              # from Chile's ciuo_2d list (36 excluded, Chile-only code)
occ_codes_habilitadores <- c(51, 91, 92, 94, 96)    # from Chile's ciuo_2d list
ind_codes_care_sector <- c(85, 86, 87, 88)          # CIIU divisions: education, health, social work -- see note above

# Pre-compute a plain, literally-named boolean column for the employment
# filter, then use THAT everywhere below (both in mutate() and in
# subset()) instead of `.data[[emp_var]]` directly. .data[[...]] is
# tidy-eval syntax that dplyr's mutate()/case_when() understand, but
# subset() (and survey::subset.survey.design) use base R's NSE, which does
# NOT know what .data is -- `subset(diseno, .data[[emp_var]] %in% emp_code)`
# fails silently in a way that leaves dise_ocup never created, rather than
# throwing where the actual problem is. This is why Chile's original script
# never hit this: it hardcodes literal column names throughout instead of
# going through a variable.
enut <- enut |> mutate(is_employed = .data[[emp_var]] %in% emp_code)

# --- classification of paid care work (occupation x sector) ---
# Mirrors Chile's logic 1:1 -- only the variable/code assignments above
# need to change. Do NOT change the structure (cw_d values 11/12/21/31/91
# and what they feed into) unless the Chile definition itself changes,
# since all countries need to stay comparable.
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

# Unclustered design (weights only) -- see the strata/PSU note above for why.
diseno <- svydesign(data = enut, ids = ~1, weights = ~FEX_C)
options(survey.lonely.psu = "certainty")
dise_ocup <- subset(diseno, is_employed)

# --- extract a svyby(FUN = svymean) result into a tidy list of rows ---
extract_rows <- function(svyby_result, indicator_col, definition_label) {
  d <- as.data.frame(svyby_result)
  lapply(seq_len(nrow(d)), function(i) {
    row <- d[i, ]
    list(
      definition = definition_label,
      sex = unname(sex_label[as.character(row[[sex_var]])]),
      share = round(row[[indicator_col]], 4),
      se = round(row[["se"]], 4),
      ci_low = round(row[["ci_l"]], 4),
      ci_high = round(row[["ci_u"]], 4)
    )
  })
}

sex_formula <- as.formula(paste0("~", sex_var))

res_cwe <- svyby(~cwe, by = sex_formula, design = dise_ocup, FUN = svymean, vartype = c("se", "ci"), na.rm = TRUE)
res_slight <- svyby(~slightly_unk_con, by = sex_formula, design = dise_ocup, FUN = svymean, vartype = c("se", "ci"), na.rm = TRUE)
res_broad <- svyby(~broad_unk_con, by = sex_formula, design = dise_ocup, FUN = svymean, vartype = c("se", "ci"), na.rm = TRUE)

rows <- c(
  extract_rows(res_cwe, "cwe", "Trabajo de cuidados remunerados"),
  extract_rows(res_slight, "slightly_unk_con", "TCR + Habilitadores"),
  extract_rows(res_broad, "broad_unk_con", "TCR + Habilitadores + Facilitadores")
)

dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)
out_path <- file.path(OUT_DIR, "paid_care_workforce_by_sex_co.json")
write_json(rows, out_path, auto_unbox = TRUE, pretty = TRUE)

cat("Wrote", length(rows), "rows to", out_path, "\n")

# --- individual cw_d categories (mutually exclusive), by sex ---
res_11 <- svyby(~tc_sc, by = sex_formula, design = dise_ocup, FUN = svymean, vartype = c("se", "ci"), na.rm = TRUE)
res_12 <- svyby(~tc_nsc, by = sex_formula, design = dise_ocup, FUN = svymean, vartype = c("se", "ci"), na.rm = TRUE)
res_21 <- svyby(~habilitadores, by = sex_formula, design = dise_ocup, FUN = svymean, vartype = c("se", "ci"), na.rm = TRUE)
res_31 <- svyby(~facilitadores, by = sex_formula, design = dise_ocup, FUN = svymean, vartype = c("se", "ci"), na.rm = TRUE)

rows_cwd <- c(
  extract_rows(res_11, "tc_sc", "TCR dentro del sector"),
  extract_rows(res_12, "tc_nsc", "TCR fuera del sector"),
  extract_rows(res_21, "habilitadores", "Habilitadores"),
  extract_rows(res_31, "facilitadores", "Facilitadores")
)

out_path_cwd <- file.path(OUT_DIR, "paid_care_workforce_cwd_by_sex_co.json")
write_json(rows_cwd, out_path_cwd, auto_unbox = TRUE, pretty = TRUE)

cat("Wrote", length(rows_cwd), "rows to", normalizePath(out_path_cwd), "\n")
cat("Both files contain only aggregate proportions + CIs -- safe to hand off.\n")


# table(enut$P6020) 
# table(enut$P1152)

# file.exists("data/enut_co/paid_care_workforce_by_sex_co.json")
# jsonlite::fromJSON("data/enut_co/paid_care_workforce_by_sex_co.json")
#                            definition     sex  share     se ci_low ci_high
# 1     Trabajo de cuidados remunerados Hombres 0.0502 0.0017 0.0469  0.0536
# 2     Trabajo de cuidados remunerados Mujeres 0.1635 0.0039 0.1558  0.1711
# 3                 TCR + Habilitadores Hombres 0.0530 0.0018 0.0494  0.0565
# 4                 TCR + Habilitadores Mujeres 0.1753 0.0041 0.1673  0.1833
# 5 TCR + Habilitadores + Facilitadores Hombres 0.0686 0.0022 0.0643  0.0729
# 6 TCR + Habilitadores + Facilitadores Mujeres 0.2090 0.0043 0.2005  0.2174

#jsonlite::fromJSON("data/enut_co/paid_care_workforce_cwd_by_sex_co.json")
#              definition     sex  share     se ci_low ci_high
# 1 TCR dentro del sector Hombres 0.0285 0.0014 0.0258  0.0312
# 2 TCR dentro del sector Mujeres 0.1186 0.0034 0.1118  0.1253
# 3  TCR fuera del sector Hombres 0.0217 0.0011 0.0196  0.0238
# 4  TCR fuera del sector Mujeres 0.0449 0.0021 0.0408  0.0490
# 5         Habilitadores Hombres 0.0027 0.0006 0.0015  0.0040
# 6         Habilitadores Mujeres 0.0118 0.0014 0.0091  0.0146
# 7         Facilitadores Hombres 0.0157 0.0013 0.0131  0.0182
# 8         Facilitadores Mujeres 0.0337 0.0016 0.0305  0.0369