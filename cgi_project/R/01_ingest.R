# Script: 01_ingest.R — Purpose: Read all raw data, apply consistent keys, save to processed/
# Reads from data/raw/ (symlinked to DATAN/) and from DATAN directly via config paths.
# Outputs six clean .rds files to data/processed/.
# No modelling here — only parsing, key normalisation, and light validation.

# Resolve script directory robustly (works via Rscript and source())
.script_dir <- tryCatch(
  dirname(normalizePath(sys.frames()[[1]]$ofile, winslash = "/")),
  error = function(e) {
    args <- commandArgs(trailingOnly = FALSE)
    f    <- sub("--file=", "", args[grep("--file=", args)])
    if (length(f) == 1) dirname(normalizePath(f, winslash = "/"))
    else "C:/Users/Samsung/OneDrive/Desktop/ProjectMay/ResearchBacked_SS_proj/cgi_project/R"
  }
)
source(file.path(.script_dir, "00_config.R"))

# ── Helper: normalise gemeente_code ───────────────────────────────────────────
# DAAN files use "GM0358" format; RDS cache uses "0358".  Strip the "GM" prefix.
norm_gem <- function(x) sub("^GM", "", as.character(x))

# ── 1. Geographic crosswalks ───────────────────────────────────────────────────
message("Loading geographic crosswalks...")

# 1a. Gemeente → COROP (from cached RDS; 342 rows)
gem_corop <- readRDS(PATH_GEM_COROP) %>%                        # gemeente_code, corop_code, corop_naam
  mutate(gemeente_code = as.character(gemeente_code),
         corop_code    = sprintf("%02d", as.integer(corop_code)))

# 1b. COROP → province (static CBS mapping; 40 COROP regions, 12 provinces)
corop_province <- tribble(
  ~corop_code, ~province,
  "01", "Groningen",    "02", "Groningen",    "03", "Groningen",
  "04", "Fryslân",      "05", "Fryslân",      "06", "Fryslân",
  "07", "Drenthe",      "08", "Drenthe",      "09", "Drenthe",
  "10", "Overijssel",   "11", "Overijssel",   "12", "Overijssel",
  "13", "Gelderland",   "14", "Gelderland",   "15", "Gelderland",   "16", "Gelderland",
  "17", "Utrecht",
  "18", "Noord-Holland","19", "Noord-Holland","20", "Noord-Holland",
  "21", "Noord-Holland","22", "Noord-Holland","23", "Noord-Holland","24", "Noord-Holland",
  "25", "Zuid-Holland", "26", "Zuid-Holland", "27", "Zuid-Holland",
  "28", "Zuid-Holland", "29", "Zuid-Holland", "30", "Zuid-Holland",
  "31", "Zeeland",      "32", "Zeeland",
  "33", "Noord-Brabant","34", "Noord-Brabant","35", "Noord-Brabant","36", "Noord-Brabant",
  "37", "Limburg",      "38", "Limburg",      "39", "Limburg",
  "40", "Flevoland"
)

# Full gemeente → province lookup (342 rows)
gem_province <- gem_corop %>%
  left_join(corop_province, by = "corop_code") %>%
  select(gemeente_code, province)

stopifnot("All 342 gemeenten mapped to a province" =
            sum(is.na(gem_province$province)) == 0)

# 1c. PC4 → gemeente_code  (from cbs_postcodes; used to geolocate wachttijden providers)
message("Loading PC4 → gemeente crosswalk...")
pc4_gem <- read_excel(PATH_CBS_POSTCODES, sheet = "data",
                      col_types = "text") %>%
  select(pc4 = code, gemeente_code) %>%
  mutate(gemeente_code = norm_gem(gemeente_code),
         pc4           = substr(pc4, 1, 4)) %>%   # keep numeric 4-digit prefix
  distinct(pc4, .keep_all = TRUE)

# Join pc4 → province
pc4_province <- pc4_gem %>%
  left_join(gem_province, by = "gemeente_code") %>%
  select(pc4, province) %>%
  filter(!is.na(province))

saveRDS(gem_province,   file.path(PROCESSED_DIR, "gem_province.rds"))
saveRDS(pc4_province,   file.path(PROCESSED_DIR, "pc4_province.rds"))
message("  gem_province: ", nrow(gem_province), " rows | pc4_province: ", nrow(pc4_province), " rows")

# ── 2. NZa OpenDIS DBC data ────────────────────────────────────────────────────
message("Loading NZa OpenDIS DBC data...")

dbc_raw <- read_excel(PATH_NZA_OPENDIS, sheet = "data",
                      col_types = c("numeric","numeric","text","text","text",
                                    "numeric","numeric","numeric","numeric",
                                    "numeric","numeric","numeric")) %>%
  rename(
    year          = jaar,
    spec_code     = behandelend_specialisme_code,
    n_patients    = aantal_patienten_per_specialisme,
    n_subtraject  = aantal_subtrajecten_per_specialisme
  ) %>%
  select(year, spec_code, n_patients, n_subtraject)

# IMPORTANT: aantal_subtrajecten_per_specialisme is a CONSTANT repeated across
# every zorgproduct row for that specialism+year.  Summing across rows would
# multiply the true total by the row count (~1,000+).
# Correct approach: deduplicate to one row per specialism+year first,
# then aggregate across specialism codes into categories.
dbc_raw <- dbc_raw %>%
  distinct(year, spec_code, .keep_all = TRUE)    # one row per spec_code × year

# Join specialism categories
dbc_raw <- dbc_raw %>%
  left_join(specialism_map, by = "spec_code")

# Flag unmapped codes
unmapped_dbc <- dbc_raw %>% filter(is.na(category)) %>% distinct(spec_code)
if (nrow(unmapped_dbc) > 0) {
  warning("Unmapped DBC specialism codes: ", paste(unmapped_dbc$spec_code, collapse = ", "))
}

# Determine base year: use 2025 if available with >=20 categories, else fall back to 2024
year_counts <- dbc_raw %>%
  filter(!is.na(category)) %>%
  group_by(year) %>%
  summarise(n_cat = n_distinct(category), .groups = "drop")

dbc_base_year <- if ((year_counts %>% filter(year == 2025) %>% pull(n_cat)) >= 20) 2025L else 2024L
message("  DBC base year for μ_s calculation: ", dbc_base_year)

# National DBC totals by category and year
# Sum across spec_codes within a category (e.g., 0308+0330 → Neurology & Neurosurgery)
dbc_national <- dbc_raw %>%
  filter(!is.na(category)) %>%
  group_by(year, category) %>%
  summarise(
    n_subtraject = sum(n_subtraject, na.rm = TRUE),  # sum across merged spec codes
    n_patients   = sum(n_patients,   na.rm = TRUE),
    .groups      = "drop"
  )

# National specialism shares μ_s at base year
dbc_shares <- dbc_national %>%
  filter(year == dbc_base_year) %>%
  mutate(mu_s = n_subtraject / sum(n_subtraject)) %>%  # share of national total
  select(category, n_subtraject_base = n_subtraject, mu_s)

stopifnot("μ_s shares sum to 1" = abs(sum(dbc_shares$mu_s) - 1) < 1e-6)

saveRDS(dbc_national, file.path(PROCESSED_DIR, "dbc_national.rds"))
saveRDS(dbc_shares,   file.path(PROCESSED_DIR, "dbc_shares.rds"))
message("  dbc_national: ", nrow(dbc_national), " rows | ",
        n_distinct(dbc_national$category), " categories | years ",
        min(dbc_national$year), "-", max(dbc_national$year))
message("  dbc_shares (μ_s): ", nrow(dbc_shares), " categories at base year ", dbc_base_year)

# ── 3. DAAN workforce data ─────────────────────────────────────────────────────
message("Loading DAAN workforce data (data_2)...")

workforce_raw <- read_excel(PATH_DAAN_WORKFORCE, sheet = "data_2",
                            col_types = "text") %>%
  mutate(
    gemeente_code              = norm_gem(gemeente_code),
    year                       = as.integer(jaar),
    werkende                   = as.numeric(prognose_aantal_werkende),
    tekort                     = as.numeric(prognose_arbeids_vraag_tekort),
    perc_0_24                  = as.numeric(prognose_perc_werkende_0),
    perc_25_34                 = as.numeric(prognose_perc_werkende_25),
    perc_35_44                 = as.numeric(prognose_perc_werkende_35),
    perc_45_54                 = as.numeric(prognose_perc_werkende_45),
    perc_55_64                 = as.numeric(prognose_perc_werkende_55),
    perc_65_plus               = as.numeric(prognose_perc_werkende_65_plus)
  ) %>%
  select(gemeente_code, gemeente, year, sector,
         werkende, tekort,
         perc_0_24, perc_25_34, perc_35_44, perc_45_54, perc_55_64, perc_65_plus)

# Attach province
workforce_raw <- workforce_raw %>%
  left_join(gem_province, by = "gemeente_code")

n_missing_prov <- sum(is.na(workforce_raw$province))
if (n_missing_prov > 0) warning("Workforce rows without province: ", n_missing_prov)

# Aggregate to province × sector × year
# sd_tekort needed later for Monte Carlo (computed BEFORE aggregation)
workforce_sd <- workforce_raw %>%
  filter(!is.na(province)) %>%
  group_by(province, sector, year) %>%
  summarise(
    sd_tekort  = sd(tekort, na.rm = TRUE),   # cross-gemeente SD within (province, sector)
    .groups    = "drop"
  )

# Safe weighted mean: handles NAs in both x and w without relying on weighted.mean(na.rm)
.wm <- function(x, w) {
  ok <- !is.na(x) & !is.na(w) & w > 0
  if (!any(ok)) return(NA_real_)
  sum(x[ok] * w[ok]) / sum(w[ok])
}

# Two separate summarise passes to avoid dplyr 1.1+ sequential-evaluation masking
# (within a single summarise(), newly computed `werkende` scalar would mask the column vector)
wf_filtered <- workforce_raw %>% filter(!is.na(province))

wf_sums <- wf_filtered %>%
  group_by(province, sector, year) %>%
  summarise(
    werkende = sum(werkende, na.rm = TRUE),  # total FTE
    tekort   = sum(tekort,   na.rm = TRUE),  # total shortage (can be negative)
    .groups  = "drop"
  )

wf_pcts <- wf_filtered %>%
  group_by(province, sector, year) %>%
  summarise(
    perc_0_24    = .wm(perc_0_24,    werkende),   # FTE-weighted age distribution
    perc_25_34   = .wm(perc_25_34,   werkende),
    perc_35_44   = .wm(perc_35_44,   werkende),
    perc_45_54   = .wm(perc_45_54,   werkende),
    perc_55_64   = .wm(perc_55_64,   werkende),
    perc_65_plus = .wm(perc_65_plus, werkende),
    .groups      = "drop"
  )

workforce <- wf_sums %>%
  left_join(wf_pcts,        by = c("province", "sector", "year")) %>%
  left_join(workforce_sd,   by = c("province", "sector", "year")) %>%
  mutate(share_55plus = (perc_55_64 + perc_65_plus) / 100)  # retirement-age share [0,1]

saveRDS(workforce, file.path(PROCESSED_DIR, "workforce.rds"))
message("  workforce: ", nrow(workforce), " rows | ",
        n_distinct(workforce$province), " provinces | ",
        n_distinct(workforce$sector), " sectors | years ",
        min(workforce$year), "-", max(workforce$year))

# ── 4. Population projections (DAAN data_3 — single-year ages by gemeente) ────
message("Loading population projections (DAAN data_3)...")

# Read only the columns we need (full file is wide with 100+ cols)
pop_cols_needed <- c("gemeente_code", "geslacht", "leeftijd",
                     "aantal_inwoners_2025",
                     "prognose_aantal_inwoners_2030",
                     "prognose_aantal_inwoners_2035")

pop_raw <- read_excel(PATH_DAAN_WORKFORCE, sheet = "data_3",
                      col_types = "text") %>%
  select(all_of(pop_cols_needed)) %>%
  mutate(
    gemeente_code = norm_gem(gemeente_code),
    leeftijd      = as.integer(leeftijd),
    pop_2025      = as.numeric(aantal_inwoners_2025),
    pop_2030      = as.numeric(prognose_aantal_inwoners_2030),
    pop_2035      = as.numeric(prognose_aantal_inwoners_2035)
  ) %>%
  select(gemeente_code, geslacht, leeftijd, pop_2025, pop_2030, pop_2035)

# Attach province
pop_raw <- pop_raw %>%
  left_join(gem_province, by = "gemeente_code")

# Map single-year ages to CGI age bands (matching contacts_per_age table)
pop_raw <- pop_raw %>%
  mutate(age_band = case_when(
    leeftijd <  15 ~ "0_15",
    leeftijd <  25 ~ "15_25",
    leeftijd <  45 ~ "25_45",
    leeftijd <  65 ~ "45_65",
    TRUE           ~ "65_plus"
  ))

# Aggregate: sum both genders, all ages within band, to province × age_band × year
population <- pop_raw %>%
  filter(!is.na(province)) %>%
  group_by(province, age_band) %>%
  summarise(
    pop_2025 = sum(pop_2025, na.rm = TRUE),
    pop_2030 = sum(pop_2030, na.rm = TRUE),
    pop_2035 = sum(pop_2035, na.rm = TRUE),
    .groups  = "drop"
  ) %>%
  # Pivot to long format: province × age_band × year × population
  pivot_longer(cols = starts_with("pop_"),
               names_to  = "year",
               names_prefix = "pop_",
               values_to = "population") %>%
  mutate(year = as.integer(year))

saveRDS(population, file.path(PROCESSED_DIR, "population.rds"))
message("  population: ", nrow(population), " rows | ",
        n_distinct(population$province), " provinces | ",
        n_distinct(population$age_band), " age bands | years ",
        paste(sort(unique(population$year)), collapse = "/"))

# Quick sanity: 2025 total NL population should be ~18M
nl_2025 <- population %>% filter(year == 2025) %>% summarise(total = sum(population)) %>% pull()
message("  NL total population 2025: ", format(round(nl_2025 / 1e6, 2), nsmall = 2), "M")

# ── 5. NZa waiting times ───────────────────────────────────────────────────────
message("Loading NZa wachttijden MSZ (", format(1130720, big.mark = ","), " rows)...")

wacht_raw <- fread(PATH_NZA_WACHTTIJD,
                   select      = c("wachttijd_type", "wachttijd", "specialisme",
                                   "postcode", "naam_instelling",
                                   "insufficient_observations", "peildatum"),
                   colClasses  = list(character = c("wachttijd_type", "specialisme",
                                                    "postcode", "naam_instelling",
                                                    "insufficient_observations", "peildatum"),
                                      integer   = "wachttijd"))

# Drop rows with insufficient data or missing wait time
wacht_raw <- wacht_raw[insufficient_observations != "true" & !is.na(wachttijd)]

# Extract primary specialism code from 'specialisme' field
# Format: "Name (NNN)" or "Name (NNN) / Name2 (NNN2) / ..."
# Take the FIRST 3-digit code as primary specialism
# str_extract returns NA (not character(0)) for non-matching rows — safe for data.table :=
wacht_raw[, spec_code_3 := str_extract(specialisme, "[0-9]{3}")]
wacht_raw[, spec_code   := fifelse(!is.na(spec_code_3),
                                    sprintf("0%s", spec_code_3),
                                    NA_character_)]  # restore leading zero → 4-digit

# Map to CGI category
spec_map_dt <- as.data.table(specialism_map)
wacht_raw <- merge(wacht_raw, spec_map_dt,
                   by = "spec_code", all.x = TRUE)

# Attach province via PC4 (first 4 chars of postcode)
wacht_raw[, pc4 := substr(postcode, 1, 4)]
pc4_prov_dt <- as.data.table(pc4_province)
wacht_raw <- merge(wacht_raw, pc4_prov_dt, by = "pc4", all.x = TRUE)

# Recode wachttijd_type to standardised labels
wacht_raw[, wait_type := fcase(
  wachttijd_type == "Polikliniekbezoek", "outpatient",
  wachttijd_type == "Behandeling",       "treatment",
  wachttijd_type == "Diagnostiek",       "diagnosis",
  default = NA_character_
)]

# Apply Treeknorm threshold per type
wacht_raw[, treeknorm := fcase(
  wait_type == "treatment", TREEKNORM_BEHANDELING,
  default   = TREEKNORM_POLIKLINIEK   # outpatient + diagnosis both = 28 days
)]
wacht_raw[, exceeds_treeknorm := wachttijd > treeknorm]

# Keep only rows with resolved category and province
wacht_clean <- wacht_raw[!is.na(category) & !is.na(province),
                          .(province, category, wait_type, wachttijd,
                            treeknorm, exceeds_treeknorm, naam_instelling)]

saveRDS(wacht_clean, file.path(PROCESSED_DIR, "wachttijden.rds"))
message("  wachttijden (clean): ", format(nrow(wacht_clean), big.mark = ","), " rows | ",
        n_distinct(wacht_clean$province),  " provinces | ",
        n_distinct(wacht_clean$category),  " categories | ",
        n_distinct(wacht_clean$wait_type), " wait types")

categories_in_wacht <- sort(unique(wacht_clean$category))
categories_missing  <- setdiff(ALL_CATEGORIES, categories_in_wacht)
message("  Categories WITHOUT wachttijden data: ",
        paste(categories_missing, collapse = "; "))

# ── 6. Summary validation ──────────────────────────────────────────────────────
message("\n=== Ingest summary ===")
message("gem_province   : ", nrow(gem_province),  " gemeenten → 12 provinces")
message("dbc_national   : ", nrow(dbc_national),  " rows | ", n_distinct(dbc_national$year), " years")
message("dbc_shares     : ", nrow(dbc_shares),    " categories (μ_s computed)")
message("workforce      : ", nrow(workforce),     " province×sector×year rows")
message("population     : ", nrow(population),    " province×age_band×year rows")
message("wachttijden    : ", format(nrow(wacht_clean), big.mark=","), " clean rows")
message("=== 01_ingest.R complete ===")
