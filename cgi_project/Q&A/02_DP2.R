# Indicator: Ageing momentum
# Pillar: Demand Pressure
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

# ── Specialism age-elasticity ε_s (clinically-informed; Capaciteitsorgaan 2022)
# ε > 1: elderly-heavy demand; ε < 1: younger-skewed demand; ε = 1: neutral
age_elasticity <- tribble(
  ~specialism,                               ~epsilon,
  "Geriatrics & Elderly Care",               2.00,   # by definition elderly-focused
  "Cardiology",                              1.50,   # CVRM burden accelerates with age
  "Orthopedics",                             1.45,   # prosthetics, osteoarthritis
  "Ophthalmology",                           1.40,   # cataract, macular degeneration
  "Neurology & Neurosurgery",                1.35,   # stroke, Parkinson, dementia
  "Pulmonology",                             1.30,   # COPD, respiratory failure
  "Urology",                                 1.30,   # prostate, incontinence
  "Cardiothoracic Surgery",                  1.30,   # cardiac surgery older patients
  "Rheumatology",                            1.25,   # osteoarthritis rises with age
  "Internal Medicine",                       1.20,   # diabetes, multimorbidity
  "Rehabilitation Medicine",                 1.20,   # post-stroke, post-fracture rehab
  "Audiology",                               1.20,   # hearing loss increases with age
  "Oncology & Radiotherapy",                 1.20,   # cancer incidence rises with age
  "Gastroenterology & Hepatology",           1.15,   # colorectal, hepatic disease
  "Pain Management & Anaesthesiology",       1.15,   # chronic pain increases with age
  "Radiology & Interventional Radiology",    1.10,   # mild age effect via referral mix
  "General & Trauma Surgery",                1.10,   # hernia, bowel resection
  "Dermatology & Allergology",               1.05,   # skin cancer increases
  "Clinical Genetics",                       1.00,   # age-neutral (mostly congenital)
  "Plastic & Reconstructive Surgery",        1.00,   # mixed ages
  "ENT",                                     1.00,   # flat age distribution
  "Psychiatry & Mental Health",              0.90,   # peaks working age / young adults
  "Sports Medicine & Other",                 0.80,   # younger athletes, sports injuries
  "Gynecology & Obstetrics",                 0.60,   # reproductive-age concentrated
  "Pediatrics",                              0.20    # child-focused; declines with 65+ share
)

# ── Load data ──────────────────────────────────────────────────────────────────
pop_summary <- readRDS(file.path(PROCESSED, "pop_summary.rds"))  # province × year

# ── Step 1: Extract share_65plus at 2025 and 2035 ─────────────────────────────
share_wide <- pop_summary %>%
  filter(year %in% c(2025, 2035)) %>%
  select(province, year, share_65plus) %>%
  pivot_wider(names_from = year, values_from = share_65plus,
              names_prefix = "s")  # → s2025, s2035

# ── Step 2: Compute Δ share_65plus (2025→2035) per province ───────────────────
delta_share <- share_wide %>%
  mutate(delta_65 = s2035 - s2025) %>%  # positive = ageing province
  select(province, delta_65)

# ── Step 3: Cross-join with specialisms, weight by ε_s ────────────────────────
# DP2_raw = Δ share_65plus × ε_s
dp2_raw <- delta_share %>%
  cross_join(age_elasticity) %>%             # all province × specialism pairs
  mutate(DP2_raw = delta_65 * epsilon) %>%   # ageing momentum weighted by specialism
  select(province, specialism, DP2_raw)

# ── Step 4: Min-max normalisation — pipeline reference (07_normalize.R) ───────
# DP2 is zero for ALL cells at 2025 by construction (Δshare_65plus from 2025→2025 = 0).
# min_ref = 0  (no change at base year, by definition)
# max_ref = max value across provinces × specialisms at 2035  (worst deterioration)
min_ref <- 0
max_ref <- max(dp2_raw$DP2_raw, na.rm = TRUE)

dp2_norm <- dp2_raw %>%
  mutate(DP2_norm = if (max_ref == min_ref) 0.5
                    else (DP2_raw - min_ref) / (max_ref - min_ref)) %>%
  select(province, specialism, DP2_norm)

# ── Step 5: Save and verify ────────────────────────────────────────────────────
write_csv(dp2_norm, file.path(OUT_DIR, "DP2_norm.csv"))

cat("min_ref =", round(min_ref, 6), "| max_ref =", round(max_ref, 6), "\n")
cat("DP2_norm range:", paste(round(range(dp2_norm$DP2_norm, na.rm = TRUE), 4), collapse = " – "), "\n")
print(head(dp2_norm, 5))
message("02_DP2.R complete — ", nrow(dp2_norm), " rows → DP2_norm.csv")
