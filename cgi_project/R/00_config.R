# Script: 00_config.R — Purpose: Paths, constants, package loading, specialism mapping
# All hardcoded paths and constants live here. Every other script sources this file first.

# ── Packages ──────────────────────────────────────────────────────────────────
packages <- c(
  "tidyverse",   # dplyr, ggplot2, tidyr, purrr, readr, stringr
  "data.table",  # fast aggregation on large DAAN data
  "readxl",      # read .xlsx files
  "sf",          # spatial data for choropleth maps
  "leaflet",     # interactive maps
  "gt",          # publication-quality tables
  "patchwork",   # compose multi-panel ggplots
  "scales",      # axis formatting
  "viridis",     # colour palettes
  "ggrepel",     # non-overlapping labels
  "mc2d"         # Monte Carlo / Dirichlet draws
  # "COINr"      # optional OECD/JRC toolkit; install separately if available
)

installed <- rownames(installed.packages())
to_install <- setdiff(packages, installed)
if (length(to_install) > 0) {
  message("Installing missing packages: ", paste(to_install, collapse = ", "))
  install.packages(to_install, repos = "https://cloud.r-project.org")
}
suppressPackageStartupMessages(lapply(packages, library, character.only = TRUE))

# ── Paths ─────────────────────────────────────────────────────────────────────
# Base directories — adjust DATAN_DIR if data moves
DATAN_DIR  <- "C:/Users/Samsung/OneDrive/Desktop/ProjectMay/DATAN"

# Resolve project root robustly (works both in RStudio and via Rscript)
.this_script <- tryCatch(
  normalizePath(sys.frames()[[1]]$ofile, winslash = "/"),  # when source()d
  error = function(e) {
    args <- commandArgs(trailingOnly = FALSE)
    f    <- sub("--file=", "", args[grep("--file=", args)])
    if (length(f) == 1) normalizePath(f, winslash = "/") else NULL
  }
)
if (!is.null(.this_script)) {
  PROJ_DIR <- normalizePath(file.path(dirname(.this_script), ".."), winslash = "/")
} else {
  # Interactive fallback
  PROJ_DIR <- "C:/Users/Samsung/OneDrive/Desktop/ProjectMay/ResearchBacked_SS_proj/cgi_project"
}

RAW_DIR       <- file.path(PROJ_DIR, "data", "raw")
PROCESSED_DIR <- file.path(PROJ_DIR, "data", "processed")
TABLES_DIR    <- file.path(PROJ_DIR, "output", "tables")
FIGURES_DIR   <- file.path(PROJ_DIR, "output", "figures")
REPORT_DIR    <- file.path(PROJ_DIR, "output", "report")

# Raw data file paths
PATH_NZA_OPENDIS    <- file.path(DATAN_DIR, "nza",               "nza_dbc_open_dis.xlsx")
PATH_NZA_WACHTTIJD  <- file.path(DATAN_DIR, "nza",               "nza_msz_waiting_times.csv")
PATH_DAAN_WORKFORCE <- file.path(DATAN_DIR, "healthcare_supply",  "daan_healthcare_activity.xlsx")
PATH_CBS_POP        <- file.path(DATAN_DIR, "demographics",       "cbs_population_forecast_2023_2050.xlsx")
PATH_CBS_GEMEENTE   <- file.path(DATAN_DIR, "geography",          "cbs_municipalities_2020_onwards.xlsx")
PATH_VEKTIS_SPEC    <- file.path(DATAN_DIR, "vektis",             "vektis_specialisms.xlsx")
PATH_GEM_COROP      <- file.path(DATAN_DIR, "geography",          "cbs_gemeente_corop_cache.rds")
PATH_CBS_POSTCODES  <- file.path(DATAN_DIR, "geography",          "cbs_postcodes.xlsx")

# ── Analysis years ────────────────────────────────────────────────────────────
YEARS_ANALYSIS <- c(2025L, 2030L, 2035L)  # years to report in CGI panel
YEAR_BASE      <- 2025L                    # normalization reference year

# ── Treeknorm thresholds (NZa, days) ─────────────────────────────────────────
TREEKNORM_POLIKLINIEK <- 28L  # Polikliniekbezoek / Diagnose
TREEKNORM_BEHANDELING <- 42L  # Behandeling (start of treatment)

# ── Monte Carlo parameters ────────────────────────────────────────────────────
N_MC_DRAWS      <- 1000L  # number of Monte Carlo draws
MC_PHI_CV       <- 0.15   # log-normal CV for productivity φ (±15%)
MC_CONTACT_CV   <- 0.10   # normal CV for age contact rates ψ_a (±10%)
MC_SEED         <- 42L    # reproducibility seed

# ── Dataset E — pre-computed specialist contacts per person per age band ──────
# Source: NZa/CBS computation from prior project phase; treat as given.
contacts_per_age <- tribble(
  ~age_band,  ~contacts_per_year,
  "0_15",     2.60,
  "15_25",    3.15,
  "25_45",    4.45,
  "45_65",    4.85,
  "65_plus",  5.74
)

# ── Specialism mapping ─────────────────────────────────────────────────────────
# 28 NZa specialism codes derived from NZa OpenDIS + Vektis COD016 reference.
# Codes are the behandelend_specialisme_code field in nza_dbc_open_dis.xlsx.
# Categories (25) are clinically meaningful groupings; 3 pairs of codes merged.
#
# Code | Vektis abbreviation | Vektis omschrijving          | CGI category
# ──────────────────────────────────────────────────────────────────────────────
# 0301   OOG   Oogheelkunde                        → Ophthalmology
# 0302   KNO   Keel-, neus- en oorheelkunde        → ENT
# 0303   CHI   Chirurgie                           → General & Trauma Surgery
# 0304   PLA   Plastische chirurgie                → Plastic & Reconstructive Surgery
# 0305   ORT   Orthopedie                          → Orthopedics
# 0306   URO   Urologie                            → Urology
# 0307   GYN   Obstetrie en gynaecologie           → Gynecology & Obstetrics
# 0308   NCH   Neurochirurgie                      → Neurology & Neurosurgery (merged)
# 0310   DER   Dermatologie en Venerologie         → Dermatology & Allergology (merged)
# 0313   INT   Interne geneeskunde                 → Internal Medicine
# 0316   KIN   Kindergeneeskunde                   → Pediatrics
# 0318   GAS   Gastro-enterologie (MDL)            → Gastroenterology & Hepatology
# 0320   CAR   Cardiologie                         → Cardiology
# 0322   LON   Longziekten                         → Pulmonology
# 0324   REU   Reumatologie                        → Rheumatology
# 0326   ALL   Allergologie                        → Dermatology & Allergology (merged)
# 0327   REV   Revalidatie                         → Rehabilitation Medicine
# 0328   CTC   Cardio thoracale chirurgie          → Cardiothoracic Surgery
# 0329   PSY   Psychiatrie                         → Psychiatry & Mental Health
# 0330   NEU   Neurologie                          → Neurology & Neurosurgery (merged)
# 0335   GER   Geriatrie                           → Geriatrics & Elderly Care (merged)
# 0361   RADT  Radiotherapie                       → Oncology & Radiotherapy
# 0362   RAD   Radiologie                          → Radiology & Interventional Radiology
# 0389   ANE   Anesthesiologie                     → Pain Management & Anaesthesiology
# 0390   GEN   Klinische genetica                  → Clinical Genetics
# 1900   —     Audiologische centra                → Audiology
# 8416   —     Overige artsen, sportgeneeskunde    → Sports Medicine & Other
# 8418   —     Overige artsen, specialist ouderengeneeskunde → Geriatrics & Elderly Care (merged)

specialism_map <- tribble(
  ~spec_code, ~category,
  "0301",  "Ophthalmology",
  "0302",  "ENT",
  "0303",  "General & Trauma Surgery",
  "0304",  "Plastic & Reconstructive Surgery",
  "0305",  "Orthopedics",
  "0306",  "Urology",
  "0307",  "Gynecology & Obstetrics",
  "0308",  "Neurology & Neurosurgery",   # Neurochirurgie
  "0310",  "Dermatology & Allergology",  # Dermatologie
  "0313",  "Internal Medicine",
  "0316",  "Pediatrics",
  "0318",  "Gastroenterology & Hepatology",
  "0320",  "Cardiology",
  "0322",  "Pulmonology",
  "0324",  "Rheumatology",
  "0326",  "Dermatology & Allergology",  # Allergologie (merged with Dermatology)
  "0327",  "Rehabilitation Medicine",
  "0328",  "Cardiothoracic Surgery",
  "0329",  "Psychiatry & Mental Health",
  "0330",  "Neurology & Neurosurgery",   # Neurologie (merged with Neurochirurgie)
  "0335",  "Geriatrics & Elderly Care",  # Geriatrie
  "0361",  "Oncology & Radiotherapy",    # Radiotherapie = cancer treatment
  "0362",  "Radiology & Interventional Radiology",
  "0389",  "Pain Management & Anaesthesiology",
  "0390",  "Clinical Genetics",
  "1900",  "Audiology",
  "8416",  "Sports Medicine & Other",
  "8418",  "Geriatrics & Elderly Care"   # Specialist ouderengeneeskunde (merged)
)

# Categories WITHOUT NZa waiting time data → AS pillar = NA → CGI = (DP + SP) / 2
NO_WACHTTIJD_CATEGORIES <- c(
  "Oncology & Radiotherapy",       # 0361 radiotherapie not in MSZ wachttijden
  "Psychiatry & Mental Health"     # 0329 PSY managed via GGZ, not MSZ
)

# All 25 unique category names (ordered alphabetically for consistent output)
ALL_CATEGORIES <- sort(unique(specialism_map$category))

# ── DAAN sector codes (as they appear in daan_healthcare_activity.xlsx data_2) ─
# DBC-producing sectors only:
DBC_SECTORS <- c("zkh", "umc", "ggz")  # ggz only for Psychiatry & Mental Health

# All sectors in the DAAN file:
# zkh   = Ziekenhuizen (hospitals)
# umc   = Universitair Medisch Centrum (academic medical centres)
# ggz   = Geestelijke gezondheidszorg (mental health)
# vv    = Verpleging & verzorging (nursing & care)
# thzrg = Thuiszorg (home care)
# gehzrg= Gehandicaptenzorg (disability care)
# jzrg  = Jeugdzorg (youth care)
# socw  = Sociaal werk (social work)
# kdv   = Kinderopvang (childcare)
# ovzw  = Overige zorg (other care)
# hgc   = Huisartsgeneeskunde (general practice)

# Workforce age-band columns (as they appear in the DAAN data_2 sheet)
# The percentages are: 0–24, 25–34, 35–44, 45–54, 55–64, 65+
DAAN_AGE_COLS <- c(
  "prognose_perc_werkende_0",
  "prognose_perc_werkende_25",
  "prognose_perc_werkende_35",
  "prognose_perc_werkende_45",
  "prognose_perc_werkende_55",
  "prognose_perc_werkende_65_plus"
)
# Retirement pressure = share aged 55+ = perc_55 + perc_65_plus
RETIREMENT_COLS <- c("prognose_perc_werkende_55", "prognose_perc_werkende_65_plus")

# ── Validation: face-validity provinces from earlier analysis ─────────────────
# These 4 provinces appeared in an earlier (methodologically limited) hospital-only
# analysis. Used only for face-validity check in 12_validate.R — NOT ground truth.
FACE_VALIDITY_PROVINCES <- c("Flevoland", "Utrecht", "Noord-Holland", "Zuid-Holland")

message("00_config.R loaded — ", length(ALL_CATEGORIES),
        " specialism categories, years: ", paste(YEARS_ANALYSIS, collapse = "/"))
