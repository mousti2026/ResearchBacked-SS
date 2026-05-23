# Indicator: Provider density (inverted)
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
wacht       <- readRDS(file.path(PROCESSED, "wachttijden.rds"))   # province × category × record
pop_summary <- readRDS(file.path(PROCESSED, "pop_summary.rds"))   # province × year

# ── Step 1: Count unique provider locations per province × specialism ──────────
providers <- wacht %>%
  group_by(province, specialism = category) %>%
  summarise(
    n_providers = n_distinct(naam_instelling, na.rm = TRUE),  # unique institutions
    .groups = "drop"
  )

# ── Step 2: Province population at 2025 baseline ──────────────────────────────
pop_2025 <- pop_summary %>%
  filter(year == 2025) %>%
  select(province, pop_total)

# ── Step 3: Provider density per 100k population ──────────────────────────────
density <- providers %>%
  left_join(pop_2025, by = "province") %>%
  mutate(density_per_100k = n_providers / (pop_total / 100000))  # providers per 100k

# ── Step 4: Invert density — sparse coverage = higher stress score ─────────────
# raw = -density so that low density → less-negative → normalises to higher value
as3_raw <- density %>%
  mutate(AS3_raw = -density_per_100k) %>%   # inversion: low density = high stress
  select(province, specialism, AS3_raw)

# ── Step 5: Min-max normalisation anchored to 2025 cross-sectional distribution
# After inversion: min = most dense (low stress), max = least dense (high stress)
min_2025 <- min(as3_raw$AS3_raw, na.rm = TRUE)   # 2025 reference floor
max_2025 <- max(as3_raw$AS3_raw, na.rm = TRUE)   # 2025 reference ceiling

as3_norm <- as3_raw %>%
  mutate(AS3_norm = if (max_2025 == min_2025) 0.5
                    else (AS3_raw - min_2025) / (max_2025 - min_2025)) %>%
  select(province, specialism, AS3_norm)

# ── Step 6: Save and verify ────────────────────────────────────────────────────
write_csv(as3_norm, file.path(OUT_DIR, "AS3_norm.csv"))

cat("min_2025 =", round(min_2025, 6), "| max_2025 =", round(max_2025, 6), "\n")
cat("AS3_norm range:", paste(round(range(as3_norm$AS3_norm, na.rm = TRUE), 4), collapse = " – "), "\n")
cat("Province × specialism cells with data:", nrow(as3_norm), "\n")
print(head(as3_norm, 5))
message("09_AS3.R complete — ", nrow(as3_norm), " rows → AS3_norm.csv")
