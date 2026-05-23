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

# ── Step 2: M-weighted FTE and shortage at 2025 (all provinces) ───────────────
wf_2025 <- workforce %>%
  filter(year == 2025, sector %in% c("zkh", "umc", "ggz")) %>%
  select(province, sector, werkende, tekort)

supply_agg <- m_matrix %>%
  left_join(wf_2025, by = "sector",
            relationship = "many-to-many") %>%    # one sector × many specialisms
  group_by(province, specialism) %>%
  summarise(
    fte_relevant    = sum(share * werkende, na.rm = TRUE),  # FTE_relevant_{p,s}
    tekort_relevant = sum(share * tekort,   na.rm = TRUE),  # shortage_{p,s}
    .groups = "drop"
  )

# ── Step 3: φ_s = national ExpectedDBC_s / national FTE_s  (Eq. 2 — national) ─
# Using a national-level φ keeps SP3 distinct from SP1.
# SP1 = tekort/werkende  (shortage as % of workforce — dimensionless ratio)
# SP3 = φ_s × tekort / ExpectedDBC  (unmet DBCs as % of required volume)
# With province-level φ, SP3 collapses to SP1; national φ breaks the identity.
phi_national <- expected_dbc_2025 %>%
  left_join(supply_agg, by = c("province", "specialism")) %>%
  group_by(specialism) %>%
  summarise(
    nat_dbc = sum(expected_dbc,   na.rm = TRUE),   # national expected volume
    nat_fte = sum(fte_relevant,   na.rm = TRUE),   # national relevant FTE
    .groups = "drop"
  ) %>%
  mutate(phi = if_else(nat_fte == 0, NA_real_,     # guard zero-FTE specialisms
                       nat_dbc / nat_fte))          # DBCs per FTE — national rate

write_csv(phi_national %>% select(specialism, phi),  # save φ_s lookup for reference
          file.path(OUT_DIR, "phi_lookup.csv"))

# ── Step 4: SP3_raw = φ_s × tekort_relevant / ExpectedDBC ─────────────────────
# Interpretation: fraction of expected care volume that cannot be delivered
sp3_raw <- expected_dbc_2025 %>%
  left_join(supply_agg,   by = c("province", "specialism")) %>%
  left_join(phi_national  %>% select(specialism, phi), by = "specialism") %>%
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
