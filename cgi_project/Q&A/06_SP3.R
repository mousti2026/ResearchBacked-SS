# Indicator: Productivity-adjusted FTE gap
# Pillar: Supply Pressure
# Output: one numeric value per province × specialism combination

library(tidyverse)

# ── Path setup ─────────────────────────────────────────────────────────────────
.here <- tryCatch(
  dirname(normalizePath(sys.frames()[[1]]$ofile, winslash = "/")),
  error = function(e) {
    f <- sub("--file=", "", commandArgs(FALSE)[grep("--file=", commandArgs(FALSE))])
    if (length(f) == 1) dirname(normalizePath(f, winslash = "/"))
    else "C:/Users/Samsung/OneDrive/Desktop/ProjectMay/ResearchBacked_SS_proj/cgi_project/Q&A"
  }
)
PROCESSED <- normalizePath(file.path(.here, "..", "data", "processed"), winslash = "/")
OUT_DIR   <- file.path(.here, "output")
dir.create(OUT_DIR, showWarnings = FALSE, recursive = TRUE)

# ── M matrix: specialism → DAAN sector allocation shares ──────────────────────
ALL_SPECS <- c(
  "Audiology", "Cardiology", "Cardiothoracic Surgery",
  "Clinical Genetics", "Dermatology & Allergology", "ENT",
  "Gastroenterology & Hepatology", "General & Trauma Surgery",
  "Geriatrics & Elderly Care", "Gynecology & Obstetrics",
  "Internal Medicine", "Neurology & Neurosurgery",
  "Oncology & Radiotherapy", "Ophthalmology", "Orthopedics",
  "Pain Management & Anaesthesiology", "Pediatrics",
  "Plastic & Reconstructive Surgery", "Psychiatry & Mental Health",
  "Pulmonology", "Radiology & Interventional Radiology",
  "Rehabilitation Medicine", "Rheumatology",
  "Sports Medicine & Other", "Urology"
)

m_matrix <- tibble(specialism = ALL_SPECS) %>%
  cross_join(tibble(sector = c("zkh", "umc", "ggz"))) %>%
  mutate(share = case_when(
    specialism == "Cardiology"             & sector == "zkh" ~ 0.88,
    specialism == "Cardiology"             & sector == "umc" ~ 0.12,
    specialism == "Cardiology"             & sector == "ggz" ~ 0.00,
    specialism == "Psychiatry & Mental Health" & sector == "zkh" ~ 0.10,
    specialism == "Psychiatry & Mental Health" & sector == "umc" ~ 0.00,
    specialism == "Psychiatry & Mental Health" & sector == "ggz" ~ 0.90,
    sector == "zkh" ~ 0.85,
    sector == "umc" ~ 0.15,
    TRUE            ~ 0.00
  )) %>%
  filter(share > 0)

# ── Load data ──────────────────────────────────────────────────────────────────
workforce   <- readRDS(file.path(PROCESSED, "workforce.rds"))    # province × sector × year
pop_summary <- readRDS(file.path(PROCESSED, "pop_summary.rds"))  # province × year
dbc_shares  <- readRDS(file.path(PROCESSED, "dbc_shares.rds"))   # category, mu_s

# ── Step 1: 2025 province-level expected DBC (Eq. 1) ──────────────────────────
# ExpectedDBC_{p,s,2025} = contact_potential_{p,2025} × μ_s
expected_dbc_2025 <- pop_summary %>%
  filter(year == 2025) %>%
  select(province, contact_potential) %>%
  cross_join(dbc_shares %>% select(specialism = category, mu_s)) %>%
  mutate(expected_dbc = contact_potential * mu_s) %>%    # Eq. 1
  select(province, specialism, expected_dbc)

# ── Step 2: Province-calibrated M matrix (mirrors 04_supply.R) ────────────────
wf_2025 <- workforce %>%
  filter(year == 2025, sector %in% c("zkh", "umc", "ggz")) %>%
  select(province, sector, werkende, tekort)

# Province sector FTE shares at 2025 base year (werkende proportion within province)
prov_sector_shares <- wf_2025 %>%
  group_by(province) %>%
  mutate(actual_share = werkende / sum(werkende, na.rm = TRUE)) %>%
  ungroup() %>%
  select(province, sector, actual_share)

# Blend national M × province sector share, renormalise per (province, specialism)
m_prov <- m_matrix %>%
  left_join(prov_sector_shares, by = "sector",
            relationship = "many-to-many") %>%
  mutate(blend = share * coalesce(actual_share, 0)) %>%
  group_by(province, specialism) %>%
  mutate(share_prov = ifelse(sum(blend) > 0, blend / sum(blend), 0)) %>%
  ungroup() %>%
  select(province, specialism, sector, share_prov)

# Apply province-calibrated M → FTE_relevant and tekort_relevant
supply_agg <- m_prov %>%
  left_join(wf_2025, by = c("province", "sector")) %>%
  group_by(province, specialism) %>%
  summarise(
    fte_relevant    = sum(share_prov * werkende, na.rm = TRUE),
    tekort_relevant = sum(share_prov * tekort,   na.rm = TRUE),
    .groups = "drop"
  )

# ── Step 3: φ_{p,s} = ExpectedDBC_{p,s,2025} / FTE_relevant_{p,s,2025} ────────
# Province-level φ mirrors 04_supply.R; national median per specialism used as
# fallback for zero-FTE cells (not national sum, which would conflate SP3 with SP1).
phi_prov <- expected_dbc_2025 %>%
  left_join(supply_agg, by = c("province", "specialism")) %>%
  mutate(phi = if_else(fte_relevant > 0, expected_dbc / fte_relevant, NA_real_))

phi_national_median <- phi_prov %>%
  group_by(specialism) %>%
  summarise(phi_median = median(phi, na.rm = TRUE), .groups = "drop")

phi_prov <- phi_prov %>%
  left_join(phi_national_median, by = "specialism") %>%
  mutate(phi = if_else(is.na(phi) | !is.finite(phi), phi_median, phi)) %>%
  select(province, specialism, phi)

write_csv(phi_national_median %>% rename(phi = phi_median),
          file.path(OUT_DIR, "phi_lookup.csv"))

# ── Step 4: SP3_raw = φ_{p,s} × tekort_relevant / ExpectedDBC ─────────────────
# Interpretation: fraction of expected care volume that cannot be delivered
sp3_raw <- expected_dbc_2025 %>%
  left_join(supply_agg, by = c("province", "specialism")) %>%
  left_join(phi_prov,   by = c("province", "specialism")) %>%
  mutate(SP3_raw = if_else(is.na(phi) | expected_dbc == 0, NA_real_,
                            phi * tekort_relevant / expected_dbc)) %>%
  select(province, specialism, SP3_raw)

# ── Step 5: Min-max normalisation anchored to 2025 cross-sectional distribution
min_2025 <- min(sp3_raw$SP3_raw, na.rm = TRUE)   # 2025 reference floor
max_2025 <- max(sp3_raw$SP3_raw, na.rm = TRUE)   # 2025 reference ceiling

sp3_norm <- sp3_raw %>%
  mutate(SP3_norm = if (max_2025 == min_2025) 0.5
                    else (SP3_raw - min_2025) / (max_2025 - min_2025)) %>%
  select(province, specialism, SP3_norm)

# ── Step 6: Save and verify ────────────────────────────────────────────────────
write_csv(sp3_norm, file.path(OUT_DIR, "SP3_norm.csv"))

cat("min_2025 =", round(min_2025, 6), "| max_2025 =", round(max_2025, 6), "\n")
cat("SP3_norm range:", paste(round(range(sp3_norm$SP3_norm, na.rm = TRUE), 4), collapse = " – "), "\n")
cat("NA rows (phi = Inf/NaN):", sum(is.na(sp3_norm$SP3_norm)), "\n")
cat("phi_lookup.csv also saved for reference.\n")
print(head(sp3_norm, 5))
message("06_SP3.R complete — ", nrow(sp3_norm), " rows → SP3_norm.csv")
