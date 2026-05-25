# Indicator: Retirement pressure
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
# Cardiology: zkh 88% / umc 12%.  Psychiatry: GGZ 90% / zkh 10%.
# All other DBC specialisms: zkh 85% / umc 15%.  Non-DBC sectors excluded.
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
workforce <- readRDS(file.path(PROCESSED, "workforce.rds"))  # province × sector × year

# ── Step 1: 2025 workforce with retirement-age share ──────────────────────────
# share_55plus = (perc_55_64 + perc_65_plus) / 100  — already in processed file
wf_2025 <- workforce %>%
  filter(year == 2025, sector %in% c("zkh", "umc", "ggz")) %>%
  select(province, sector, werkende, share_55plus)

# ── Step 1b: Province-calibrated M matrix (mirrors 04_supply.R) ───────────────
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
  mutate(share_prov = if_else(sum(blend) > 0, blend / sum(blend), 0)) %>%
  ungroup() %>%
  select(province, specialism, sector, share_prov)

# ── Step 2: Province-calibrated M-weighted retirement share ───────────────────
# SP2_raw = Σ_k M_prov[p,s,k] × werkende_{p,k} × share_55plus_{p,k}
#         / Σ_k M_prov[p,s,k] × werkende_{p,k}
sp2_agg <- m_prov %>%
  left_join(wf_2025, by = c("province", "sector")) %>%
  mutate(effective_weight = share_prov * werkende) %>%     # province-calibrated weight × FTE
  group_by(province, specialism) %>%
  summarise(
    SP2_raw = if_else(sum(effective_weight, na.rm = TRUE) == 0, NA_real_,
                      sum(effective_weight * share_55plus, na.rm = TRUE) /
                        sum(effective_weight, na.rm = TRUE)),
    .groups = "drop"
  )

# ── Step 3: Min-max normalisation anchored to 2025 cross-sectional distribution
min_2025 <- min(sp2_agg$SP2_raw, na.rm = TRUE)   # 2025 reference floor
max_2025 <- max(sp2_agg$SP2_raw, na.rm = TRUE)   # 2025 reference ceiling

sp2_norm <- sp2_agg %>%
  mutate(SP2_norm = if (max_2025 == min_2025) 0.5
                    else (SP2_raw - min_2025) / (max_2025 - min_2025)) %>%
  select(province, specialism, SP2_norm)

# ── Step 4: Save and verify ────────────────────────────────────────────────────
write_csv(sp2_norm, file.path(OUT_DIR, "SP2_norm.csv"))

cat("min_2025 =", round(min_2025, 6), "| max_2025 =", round(max_2025, 6), "\n")
cat("SP2_norm range:", paste(round(range(sp2_norm$SP2_norm, na.rm = TRUE), 4), collapse = " – "), "\n")
cat("NA rows:", sum(is.na(sp2_norm$SP2_norm)), "\n")
print(head(sp2_norm, 5))
message("05_SP2.R complete — ", nrow(sp2_norm), " rows → SP2_norm.csv")
