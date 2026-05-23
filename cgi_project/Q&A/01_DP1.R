# Indicator: Per-capita DBC need intensity
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

# ── Load data ──────────────────────────────────────────────────────────────────
pop_summary <- readRDS(file.path(PROCESSED, "pop_summary.rds"))   # province × year
dbc_shares  <- readRDS(file.path(PROCESSED, "dbc_shares.rds"))    # category, mu_s

# ── Step 1: Isolate 2025 population summaries ──────────────────────────────────
pop_2025 <- pop_summary %>%
  filter(year == 2025) %>%           # baseline year only
  select(province, contact_potential, pop_total)

# ── Step 2: Cross-join provinces with specialisms, compute raw DP1 ─────────────
# ExpectedDBC_{p,s} = contact_potential_p × μ_s   (Eq. 1)
# DP1_raw = ExpectedDBC / pop_total                (per-capita need)
dp1_raw <- pop_2025 %>%
  cross_join(dbc_shares %>% select(specialism = category, mu_s)) %>%  # all p × s
  mutate(DP1_raw = (contact_potential * mu_s) / pop_total) %>%        # per-capita
  select(province, specialism, DP1_raw)

# ── Step 3: Min-max normalisation anchored to 2025 cross-sectional distribution
min_2025 <- min(dp1_raw$DP1_raw, na.rm = TRUE)   # 2025 reference floor
max_2025 <- max(dp1_raw$DP1_raw, na.rm = TRUE)   # 2025 reference ceiling

dp1_norm <- dp1_raw %>%
  mutate(DP1_norm = if (max_2025 == min_2025) 0.5
                    else (DP1_raw - min_2025) / (max_2025 - min_2025)) %>%
  select(province, specialism, DP1_norm)

# ── Step 4: Save and verify ────────────────────────────────────────────────────
write_csv(dp1_norm, file.path(OUT_DIR, "DP1_norm.csv"))

cat("min_2025 =", round(min_2025, 6), "| max_2025 =", round(max_2025, 6), "\n")
cat("DP1_norm range:", paste(round(range(dp1_norm$DP1_norm, na.rm = TRUE), 4), collapse = " – "), "\n")
print(head(dp1_norm, 5))
message("01_DP1.R complete — ", nrow(dp1_norm), " rows → DP1_norm.csv")
