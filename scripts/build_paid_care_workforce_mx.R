# Run this LOCALLY. It never needs to leave your machine -- this script
# only ever writes out a small aggregate JSON (a handful of proportions +
# confidence intervals), never any row-level data. Copy the resulting file
# into data/enut_mx/ in the site repo (or hand it back) once it's built.
#
# UNLIKE Chile's source (an internal-only INE network file), Mexico's ENUT
# 2024 microdata is public open data, downloadable directly with no login:
#   Data (CSV, 17.9 MB zip): https://www.inegi.org.mx/contenidos/programas/
#     enut/2024/microdatos/enut_2024_bd_csv.zip
#   Data dictionary: https://www.inegi.org.mx/contenidos/programas/enut/
#     2024/microdatos/enut_2024_fd.xlsx
# Neither link is advertised on the RNM catalog page (inegi.org.mx/rnm/
# index.php/catalog/1127) -- that page only links back to the survey
# homepage (inegi.org.mx/programas/enut/2024/), which is a JS-rendered
# page whose real download links only show up in the browser's network
# requests, not in the static HTML. Found by loading the page with a
# headless browser and inspecting outgoing requests -- if INEGI
# reorganizes the site later, that's the technique to redo this with.
#
# IMPORTANT STRUCTURAL LIMITATION, confirmed directly from the data:
# ENUT 2024's labor module (TMODULO) has NO industry/economic-branch
# variable at all. The questionnaire's section 5 jumps straight from
# question 5.3 (occupation, P5_3C) to question 5.5 -- there's no 5.4 asking
# about the employer's line of business. The bundled catalog file
# (enut_2024_cat.xlsx, inside the data zip) has exactly 4 lookup sheets --
# PARENTESCO, LENGUA INDÍGENA, OCUPACIÓN, USO TIEMPO -- no industry catalog
# either. Chile's cw_d classification is fundamentally occupation x
# industry; Mexico only gives you the occupation half. Concretely, this
# means:
#   - "Trabajo de cuidados remunerados" (cwe) IS computable -- it only
#     needs occupation.
#   - An occupation-only approximation of "TCR + Habilitadores" is
#     computable (see slightly_unk_con below), but it's not quite the same
#     definition as Chile's -- Chile also requires the associate-level
#     occupations to be *within* a care-sector industry, which can't be
#     checked here, so this version is looser (any associate/support-level
#     care occupation counts, regardless of employer industry).
#   - "TCR + Habilitadores + Facilitadores" (broad_unk_con) and the "TCR
#     dentro/fuera del sector" split (tc_sc/tc_nsc) are NOT computable at
#     all -- "Facilitadores" is specifically defined as non-care
#     occupations working *within* a care-sector industry (e.g. a cook or
#     security guard employed at a hospital), which is unrecoverable
#     without an industry variable. This script does not produce those two
#     outputs for Mexico -- don't approximate them, that would misrepresent
#     what's actually being measured.
#
# This mirrors scripts/build_paid_care_workforce.R (the Chile version)
# where the definitions overlap, so what it DOES produce slots into the
# existing country selector on the "Cuidado remunerado" pages without
# touching chart code -- those charts just won't have Mexico data for the
# two indicators listed above until/unless a source with industry data
# is found.
#
# CONFIRMED (from the data dictionary + the bundled occupation catalog,
# opened directly -- not just web research this time):
#   - Sex: SEXO (in TSDEM)
#   - Occupation: P5_3C, 4-digit SINCO (Sistema Nacional de Clasificación
#     de Ocupaciones) -- Mexico's OWN scheme, confirmed NOT ISCO-08/CIUO
#     (spot-checked real codes: SINCO major group "1" is directors/
#     managers, "2" is professionals, etc. -- structurally different
#     from Chile's ciuo_2d groupings even where the leading digit looks
#     similar). Built occ_codes_tcr / occ_codes_habilitadores below by
#     searching the real catalog (enut_2024_cat.xlsx, sheet
#     "OCUPACIÓN_P5_3C 2024") for health/teaching/social-work/personal-care
#     descriptions -- see the comments by each code for what it is. This
#     is MY reading of the Spanish descriptions, not an official INEGI
#     crosswalk -- worth a second pair of eyes before treating it as final,
#     the same way Chile's own list presumably went through methodological
#     review.
#   - Employment status: P5_1 ("¿trabajó al menos una hora?", 1 = Sí is the
#     first response category) -- CONFIRMED this is the right restriction
#     variable (unlike the last version of this script, which only had
#     P5_1 as a guess).
#   - Weight: FAC_PER -- TSDEM's dictionary only surfaced FAC_HOG on the
#     first pass, but general ENUT documentation lists three weights
#     (FAC_VIV/FAC_HOG/FAC_PER); use FAC_PER for a person-level indicator.
#     Still worth confirming it's actually a column in TSDEM.csv once
#     downloaded (`names(tsdem)`).
#   - Strata: EST_DIS: PSU: UPM_DIS (both in TSDEM)
#
# Usage:
#   Rscript scripts/build_paid_care_workforce_mx.R

suppressPackageStartupMessages({
  library(dplyr)
  library(survey)
  library(jsonlite)
  library(readr)
})

OUT_DIR <- "data/enut_mx"

# Confirmed working, no-auth download.
ZIP_URL <- "https://www.inegi.org.mx/contenidos/programas/enut/2024/microdatos/enut_2024_bd_csv.zip"
zip_path <- tempfile(fileext = ".zip")
download.file(ZIP_URL, zip_path, mode = "wb")
extract_dir <- tempfile()
unzip(zip_path, files = c("tmodulo.csv", "tsdem.csv"), exdir = extract_dir)

tmodulo <- read_csv(file.path(extract_dir, "tmodulo.csv"), col_types = cols(.default = "c"), show_col_types = FALSE)
tsdem <- read_csv(file.path(extract_dir, "tsdem.csv"), col_types = cols(.default = "c"), show_col_types = FALSE)

# Join on the person key. TODO: confirm these three columns are the right
# join key and exist with these exact names in both files -- the data
# dictionary calls it "LLAVEMOD"/"LLAVEHOG"/"LLAVEVIV" for TMODULO; TSDEM
# may use a subset of these or a differently-named equivalent.
enut <- tmodulo |>
  left_join(tsdem, by = c("LLAVEHOG", "LLAVEVIV"))

sex_var <- "SEXO"  # CONFIRMED
# TODO: confirm coding direction -- do not assume this matches Chile's
# 1=Hombres/2=Mujeres without checking (Argentina's ENUT turned out to be
# reversed from Chile's; verify Mexico's rather than assuming either way).
sex_label <- c(`1` = "Hombres", `2` = "Mujeres")  # UNVERIFIED direction

occ_var <- "P5_3C"
emp_var <- "P5_1"     # CONFIRMED -- "¿trabajó al menos una hora la semana pasada?"
emp_code <- c("1")    # "1" = Sí

# Occupation codes pulled from the real SINCO catalog (enut_2024_cat.xlsx)
# by searching descriptions for health/teaching/social-work/personal-care
# terms, then hand-filtering out false positives and out-of-scope matches
# (e.g. "cuidadores de autos" -- car-parking attendants, not caregiving;
# "cuidado de mascotas"/animal husbandry; engineering and administrative
# roles that happen to sit in a health/social ministry). Grouped the same
# way as Chile's TCR (professional) vs Habilitadores (associate/support):
occ_codes_tcr <- c(
  "2132", # Investigadores y profesionistas en sociología y desarrollo social
  "2142", # Psicólogos
  "2143", # Profesionistas en trabajo social
  "2321", "2322", "2331", "2332", "2334", "2335", "2339", # Profesores (todos los niveles)
  "2341", "2342", "2343", "2399",                          # Profesores de educación especial y otros
  "2411", "2424", "2429",                                  # Médicos generales y especialistas
  "2434", "2435",                                           # Salud pública, medicina alternativa
  "2436"                                                    # Enfermeras y paramédicos profesionales
)
occ_codes_habilitadores <- c(
  "2531", # Auxiliares en ciencias sociales y humanistas
  "2711", # Auxiliares y técnicos en pedagogía y en educación
  "2811", "2812", "2813", "2815", "2817",                   # Enfermeras técnicas, técnicos médicos
  "2821", "2823",                                            # Auxiliares en enfermería y hospitalarios
  "5201", "5221", "5222"                                    # Cuidadores de niños/personas con discapacidad/ancianos
)

# Pre-compute a plain, literally-named boolean column for the employment
# filter, then use THAT everywhere below instead of `.data[[emp_var]]`
# directly. .data[[...]] is tidy-eval syntax mutate()/case_when() understand,
# but subset() (and survey::subset.survey.design) use base R's NSE, which
# does NOT know what .data is -- `subset(diseno, .data[[emp_var]] %in%
# emp_code)` fails silently in a way that leaves dise_ocup never created,
# rather than throwing where the actual problem is.
enut <- enut |> mutate(is_employed = .data[[emp_var]] %in% emp_code)

# --- classification of paid care work (occupation only -- see the
# structural-limitation note above for why there's no industry/sector
# split for Mexico) ---
enut <- enut |>
  mutate(
    cwe = case_when(
      !is_employed ~ NA,
      .data[[occ_var]] %in% occ_codes_tcr ~ 1,
      TRUE ~ 0
    ),
    slightly_unk_con = case_when(
      !is_employed ~ NA,
      .data[[occ_var]] %in% c(occ_codes_tcr, occ_codes_habilitadores) ~ 1,
      TRUE ~ 0
    )
  )

diseno <- svydesign(
  data = enut,
  strata = ~EST_DIS,  # CONFIRMED
  ids = ~UPM_DIS,     # CONFIRMED
  weights = ~FAC_PER  # see UNVERIFIED note above
)
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

# Only two rows, not three like Chile/Colombia -- no "TCR + Habilitadores +
# Facilitadores" for Mexico, see note at the top of this file.
rows <- c(
  extract_rows(res_cwe, "cwe", "Trabajo de cuidados remunerados"),
  extract_rows(res_slight, "slightly_unk_con", "TCR + Habilitadores")
)

dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)
out_path <- file.path(OUT_DIR, "paid_care_workforce_by_sex_mx.json")
write_json(rows, out_path, auto_unbox = TRUE, pretty = TRUE)

cat("Wrote", length(rows), "rows to", out_path, "\n")
cat("Note: no paid_care_workforce_cwd_by_sex_mx.json is produced -- the\n")
cat("dentro/fuera del sector split needs an industry variable ENUT 2024\n")
cat("doesn't have. File contains only aggregate proportions + CIs -- safe to hand off.\n")
