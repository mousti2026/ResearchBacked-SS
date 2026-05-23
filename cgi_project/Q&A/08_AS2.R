# Indicator: Mean waiting days z-score
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

# ── Load data ──────────────────────────────────────────────────────────────────
wacht <- readRDS(file.path(PROCESSED, "wachttijden.rds"))  # province × category × record

# ── Step 1: Mean waiting days per province × specialism ───────────────────────
mean_wait <- wacht %>%
  group_by(province, specialism = category) %>%
  summarise(mean_wait = mean(wachttijd, na.rm = TRUE),  # average days per cell
            .groups = "drop")

# ── Step 2: Z-score within each specialism (across provinces) ─────────────────
# z = (mean_wait_{p,s} - national_mean_s) / national_sd_s
# Captures how a province deviates from the national average for that specialism
as2_raw <- mean_wait %>%
  group_by(specialism) %>%
  mutate(
    nat_mean = mean(mean_wait, na.rm = TRUE),  # specialism-national mean
    nat_sd   = sd(mean_wait,   na.rm = TRUE),  # specialism-national SD
    AS2_raw  = if_else(nat_sd == 0 | is.na(nat_sd), 0,  # no variation → z = 0
                       (mean_wait - nat_mean) / nat_sd)  # z-score
  ) %>%
  ungroup() %>%
  select(province, specialism, AS2_raw)

# ── Step 3: Min-max normalisation anchored to 2025 cross-sectional distribution
min_2025 <- min(as2_raw$AS2_raw, na.rm = TRUE)   # 2025 reference floor
max_2025 <- max(as2_raw$AS2_raw, na.rm = TRUE)   # 2025 reference ceiling

as2_norm <- as2_raw %>%
  mutate(AS2_norm = if (max_2025 == min_2025) 0.5
                    else (AS2_raw - min_2025) / (max_2025 - min_2025)) %>%
  select(province, specialism, AS2_norm)

# ── Step 4: Save and verify ────────────────────────────────────────────────────
write_csv(as2_norm, file.path(OUT_DIR, "AS2_norm.csv"))

cat("min_2025 =", round(min_2025, 6), "| max_2025 =", round(max_2025, 6), "\n")
cat("AS2_norm range:", paste(round(range(as2_norm$AS2_norm, na.rm = TRUE), 4), collapse = " – "), "\n")
cat("Province × specialism cells with data:", nrow(as2_norm), "\n")
print(head(as2_norm, 5))
message("08_AS2.R complete — ", nrow(as2_norm), " rows → AS2_norm.csv")
