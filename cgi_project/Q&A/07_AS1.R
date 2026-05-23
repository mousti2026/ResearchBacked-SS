# Indicator: Treeknorm breach rate
# Pillar: Access Stress
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

# ── Treeknorm thresholds (NZa, days) ──────────────────────────────────────────
TREEKNORM_POLIKLINIEK <- 28L  # outpatient visit / diagnosis
TREEKNORM_BEHANDELING <- 42L  # start of treatment

# ── Load data ──────────────────────────────────────────────────────────────────
# wachttijden.rds already has exceeds_treeknorm flagged per record
wacht <- readRDS(file.path(PROCESSED, "wachttijden.rds"))  # province × category × record

# ── Step 1: Breach rate = share of records exceeding Treeknorm ────────────────
# Pooled across wait_type (outpatient + treatment) per province × specialism
as1_raw <- wacht %>%
  group_by(province, specialism = category) %>%
  summarise(
    n_records       = n(),
    n_breach        = sum(exceeds_treeknorm, na.rm = TRUE),  # count breaches
    AS1_raw         = n_breach / n_records,                  # breach rate [0, 1]
    .groups = "drop"
  ) %>%
  select(province, specialism, AS1_raw)

# ── Step 2: Min-max normalisation anchored to 2025 cross-sectional distribution
# (wachttijden is a 2025 snapshot so the distribution IS the 2025 distribution)
min_2025 <- min(as1_raw$AS1_raw, na.rm = TRUE)   # 2025 reference floor
max_2025 <- max(as1_raw$AS1_raw, na.rm = TRUE)   # 2025 reference ceiling

as1_norm <- as1_raw %>%
  mutate(AS1_norm = if (max_2025 == min_2025) 0.5
                    else (AS1_raw - min_2025) / (max_2025 - min_2025)) %>%
  select(province, specialism, AS1_norm)

# ── Step 3: Save and verify ────────────────────────────────────────────────────
write_csv(as1_norm, file.path(OUT_DIR, "AS1_norm.csv"))

cat("Treeknorm thresholds: outpatient/diagnosis =", TREEKNORM_POLIKLINIEK,
    "days | treatment =", TREEKNORM_BEHANDELING, "days\n")
cat("min_2025 =", round(min_2025, 6), "| max_2025 =", round(max_2025, 6), "\n")
cat("AS1_norm range:", paste(round(range(as1_norm$AS1_norm, na.rm = TRUE), 4), collapse = " – "), "\n")
cat("Province × specialism cells with data:", nrow(as1_norm), "\n")
print(head(as1_norm, 5))
message("07_AS1.R complete — ", nrow(as1_norm), " rows → AS1_norm.csv")
