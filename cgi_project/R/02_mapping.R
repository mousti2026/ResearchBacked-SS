# Script: 02_mapping.R — Purpose: Build DBC↔DAAN sector mapping matrix M
# M[specialism, sector] gives the share of DBC activity attributable to each
# DAAN sector.  Only the three DBC-producing DAAN sectors receive non-zero shares:
#   zkh  = Ziekenhuizen (general hospitals)
#   umc  = Universitair Medisch Centrum (academic hospitals)
#   ggz  = Geestelijke gezondheidszorg (only for Psychiatry & Mental Health)
# All other DAAN sectors (vv, thzrg, gehzrg, jzrg, socw, kdv, ovzw, hgc) = 0.
# Shares must sum to 1.0 per specialism across the DBC sectors.
#
# Basis for estimates:
#  - NZa/Vektis published DBC market-share data by provider type (2022–2024)
#  - Capaciteitsorgaan capacity reports per specialism (2022)
#  - Academic hospital (UMC) market share is higher for rare/complex specialisms
#  - GGZ share for Psychiatry based on NZa GGZ volume data
#  - Shares validated against sum=1 per specialism (see bottom of file)
#
# NOTE — known limitations (flagged for sensitivity analysis in 10_sensitivity.R):
#  a) Audiology (1900):  audiological centres are `ovzw` in DAAN, not zkh/umc.
#     Mapped to zkh/umc as best DBC-sector proxy; supply will be underestimated.
#  b) Geriatrics & Elderly Care:  specialist ouderengeneeskunde (8418) works in
#     nursing homes (`vv`), not hospitals. Only the in-hospital geriatric share
#     is captured here. Supply underestimated for the elderly-care component.
#  c) Psychiatry & Mental Health:  DBC volume (0329) is the *hospital-based*
#     psychiatric DBC stream only (0.01% of all DBCs). The DAAN `ggz` workforce
#     covers the full GGZ sector. Productivity bridge φ will absorb much of this
#     mismatch, but the scale difference should be kept in mind.
#  d) Sports Medicine & Other (8416):  many sports-medicine physicians work in
#     freestanding clinics outside zkh/umc; supply is underestimated.

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

message("Building sector mapping matrix M...")

# ── M matrix definition ────────────────────────────────────────────────────────
# Column order: specialism | sector | share
# Shares per specialism must sum to 1.0 (validated below).
#
# Guiding tiers:
#  Tier 1 — Standard hospital specialisms:      zkh ~0.85–0.90, umc ~0.10–0.15
#  Tier 2 — Academic-leaning specialisms:       zkh ~0.60–0.75, umc ~0.25–0.40
#  Tier 3 — GGZ-dominated:                     ggz  0.90,       zkh  0.10
#  Tier 4 — Highly academic / rare:             zkh ~0.25–0.50, umc ~0.50–0.75

M <- tribble(
  ~category,                              ~sector,  ~share,

  # ── Tier 1 — Standard hospital (zkh ≥ 0.80) ────────────────────────────────
  # Ophthalmology: high-volume outpatient; mostly general hospitals
  "Ophthalmology",                        "zkh",    0.88,
  "Ophthalmology",                        "umc",    0.12,

  # ENT: routine ENT care in general hospitals; limited UMC differentiation
  "ENT",                                  "zkh",    0.88,
  "ENT",                                  "umc",    0.12,

  # Dermatology & Allergology: combines 0310 (DER) + 0326 (ALL); both mainly zkh
  "Dermatology & Allergology",            "zkh",    0.87,
  "Dermatology & Allergology",            "umc",    0.13,

  # Urology: predominantly general hospitals
  "Urology",                              "zkh",    0.85,
  "Urology",                              "umc",    0.15,

  # Rheumatology: mostly general hospitals; some UMC for complex/rare disease
  "Rheumatology",                         "zkh",    0.85,
  "Rheumatology",                         "umc",    0.15,

  # Cardiology: high volume, most in zkh; UMC for interventional/complex
  "Cardiology",                           "zkh",    0.85,
  "Cardiology",                           "umc",    0.15,

  # Pulmonology: mainly general hospitals
  "Pulmonology",                          "zkh",    0.83,
  "Pulmonology",                          "umc",    0.17,

  # Gynecology & Obstetrics: combines obs + gyn; mostly general hospitals
  "Gynecology & Obstetrics",              "zkh",    0.83,
  "Gynecology & Obstetrics",             "umc",    0.17,

  # General & Trauma Surgery: dominated by zkh; UMC for complex trauma/oncosurgery
  "General & Trauma Surgery",             "zkh",    0.85,
  "General & Trauma Surgery",             "umc",    0.15,

  # Plastic & Reconstructive Surgery: mostly zkh/private; some UMC
  "Plastic & Reconstructive Surgery",     "zkh",    0.82,
  "Plastic & Reconstructive Surgery",     "umc",    0.18,

  # Gastroenterology & Hepatology: mostly general hospitals
  "Gastroenterology & Hepatology",        "zkh",    0.82,
  "Gastroenterology & Hepatology",        "umc",    0.18,

  # Pain Management & Anaesthesiology (0389 ANE): pain clinics mainly in zkh
  "Pain Management & Anaesthesiology",    "zkh",    0.85,
  "Pain Management & Anaesthesiology",    "umc",    0.15,

  # Rehabilitation Medicine (0327 REV): ~75% zkh, ~15% umc, ~10% standalone.
  # Standalone rehab (ovzw) does not produce DBCs → redistribute to DBC sectors.
  # Renormalized: 0.75/0.90 ≈ 0.83, 0.15/0.90 ≈ 0.17
  "Rehabilitation Medicine",              "zkh",    0.83,
  "Rehabilitation Medicine",              "umc",    0.17,

  # Radiology & Interventional Radiology (0362 RAD): embedded in hospitals
  "Radiology & Interventional Radiology", "zkh",    0.72,
  "Radiology & Interventional Radiology", "umc",    0.28,

  # Audiology (1900): audiological centres → closest DBC proxy is zkh/umc.
  # KNOWN LIMITATION: true supply is in ovzw (not DBC-producing). See header note.
  "Audiology",                            "zkh",    0.70,
  "Audiology",                            "umc",    0.30,

  # Sports Medicine & Other (8416): sports medicine mainly in zkh/private clinics
  # KNOWN LIMITATION: freestanding sports-medicine clinics not in zkh/umc.
  "Sports Medicine & Other",              "zkh",    0.80,
  "Sports Medicine & Other",             "umc",    0.20,

  # ── Tier 2 — Academic-leaning (umc 0.25–0.40) ──────────────────────────────
  # Orthopedics: high DBC volume; complex prosthetics & revision surgery in UMC
  "Orthopedics",                          "zkh",    0.87,
  "Orthopedics",                          "umc",    0.13,

  # Internal Medicine: largest category (29% of DBCs); UMC for subspecialties
  "Internal Medicine",                    "zkh",    0.78,
  "Internal Medicine",                    "umc",    0.22,

  # Neurology & Neurosurgery: combines 0330 (NEU) + 0308 (NCH).
  # Neurosurgery is heavily UMC; Neurology splits more evenly → blended share
  "Neurology & Neurosurgery",             "zkh",    0.68,
  "Neurology & Neurosurgery",             "umc",    0.32,

  # Pediatrics: complex/neonatal care concentrated in UMC (8 UMC children's depts)
  "Pediatrics",                           "zkh",    0.72,
  "Pediatrics",                           "umc",    0.28,

  # Cardiothoracic Surgery (0328 CTC): interventional cardiac surgery; large UMC share
  "Cardiothoracic Surgery",               "zkh",    0.58,
  "Cardiothoracic Surgery",               "umc",    0.42,

  # Geriatrics & Elderly Care: combines 0335 (GER, in-hospital) + 8418 (ouderengeneeskunde).
  # In-hospital geriatrics: mainly UMC geriatric wards + some zkh.
  # Ouderengeneeskunde (8418): nursing homes (vv) → NOT a DBC sector.
  # KNOWN LIMITATION: nursing-home component not captured. See header note.
  "Geriatrics & Elderly Care",            "zkh",    0.72,
  "Geriatrics & Elderly Care",            "umc",    0.28,

  # ── Tier 4 — Highly academic / rare ────────────────────────────────────────
  # Oncology & Radiotherapy (0361 RADT): radiotherapy centres are NKI + UMC-based;
  # some zkh radiotherapy units exist but most volume is UMC / NKI
  "Oncology & Radiotherapy",              "zkh",    0.50,
  "Oncology & Radiotherapy",              "umc",    0.50,

  # Clinical Genetics (0390 GEN): almost entirely UMC-based
  "Clinical Genetics",                    "zkh",    0.25,
  "Clinical Genetics",                    "umc",    0.75,

  # ── Tier 3 — GGZ-dominated ─────────────────────────────────────────────────
  # Psychiatry & Mental Health (0329 PSY): GGZ carries ~90% of MSZ psych volume.
  # KNOWN LIMITATION: DBC (MSZ) psychiatric volume ≠ full GGZ workload.
  "Psychiatry & Mental Health",           "ggz",    0.90,
  "Psychiatry & Mental Health",           "zkh",    0.10
)

# ── Validation ─────────────────────────────────────────────────────────────────
message("Validating M matrix...")

# 1. All 25 categories present
missing_cats <- setdiff(ALL_CATEGORIES, unique(M$category))
extra_cats   <- setdiff(unique(M$category), ALL_CATEGORIES)
if (length(missing_cats) > 0) stop("M missing categories: ", paste(missing_cats, collapse = ", "))
if (length(extra_cats)   > 0) stop("M has unrecognised categories: ", paste(extra_cats, collapse = ", "))

# 2. Only DBC-producing sectors used
invalid_sectors <- setdiff(unique(M$sector), DBC_SECTORS)
if (length(invalid_sectors) > 0) stop("Non-DBC sectors in M: ", paste(invalid_sectors, collapse = ", "))

# 3. Shares sum to 1.0 per specialism (tolerance: 1e-9)
share_sums <- M %>%
  group_by(category) %>%
  summarise(total = sum(share), .groups = "drop")

bad_sums <- share_sums %>% filter(abs(total - 1) > 1e-9)
if (nrow(bad_sums) > 0) {
  stop("Shares do not sum to 1 for: ",
       paste(bad_sums$category, "(", round(bad_sums$total, 6), ")", collapse = "; "))
}

# 4. No negative shares
if (any(M$share < 0)) stop("Negative shares found in M")

message("  M validated: ", nrow(M), " rows | ",
        n_distinct(M$category), " categories | ",
        n_distinct(M$sector), " sectors")

# ── Wide form (optional convenience) ──────────────────────────────────────────
# M_wide: rows = specialism, cols = zkh / umc / ggz (0 for absent)
M_wide <- M %>%
  pivot_wider(names_from  = sector,
              values_from = share,
              values_fill = 0) %>%
  # Ensure all three DBC sector columns exist even if no specialism uses ggz
  { if (!"ggz" %in% names(.)) mutate(., ggz = 0) else . } %>%
  { if (!"umc" %in% names(.)) mutate(., umc = 0) else . } %>%
  { if (!"zkh" %in% names(.)) mutate(., zkh = 0) else . } %>%
  select(category, zkh, umc, ggz)

# ── Print summary ──────────────────────────────────────────────────────────────
message("\nM matrix (wide form, sorted by umc share descending):")
M_wide %>%
  arrange(desc(umc)) %>%
  mutate(across(c(zkh, umc, ggz), ~ sprintf("%.2f", .))) %>%
  { message(capture.output(print(as.data.frame(.), row.names = FALSE)) %>%
              paste(collapse = "\n")) }

# ── Categories with known supply limitations ──────────────────────────────────
# These will be flagged as `supply_proxy = TRUE` in downstream scripts
SUPPLY_PROXY_CATEGORIES <- c(
  "Audiology",               # true supply in ovzw, not zkh/umc
  "Geriatrics & Elderly Care",# ouderengeneeskunde supply in vv
  "Psychiatry & Mental Health",# DAAN ggz ≠ MSZ psych DBC scope
  "Sports Medicine & Other"  # freestanding clinics not in zkh/umc
)

# ── Save outputs ───────────────────────────────────────────────────────────────
saveRDS(M,                        file.path(PROCESSED_DIR, "M_long.rds"))
saveRDS(M_wide,                   file.path(PROCESSED_DIR, "M_wide.rds"))
saveRDS(SUPPLY_PROXY_CATEGORIES,  file.path(PROCESSED_DIR, "supply_proxy_categories.rds"))

message("\n=== 02_mapping.R complete ===")
message("  M_long.rds  : ", nrow(M),      " rows (specialism × sector)")
message("  M_wide.rds  : ", nrow(M_wide), " rows (one per specialism)")
message("  supply_proxy: ", length(SUPPLY_PROXY_CATEGORIES),
        " categories flagged with limited sector coverage")
