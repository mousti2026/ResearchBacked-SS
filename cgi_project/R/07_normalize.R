# Script: 07_normalize.R — Purpose: Min-max normalization against 2025 baseline
#
# Method: I_i*_{p,s,t} = (I_i_{p,s,t} - min_2025) / (max_2025 - min_2025)
#
# min_2025 and max_2025 are computed across ALL (p, s) cells at t = 2025 ONLY.
# These reference values are frozen and reused for 2030/2035 projections.
# DP and AS indicators are clipped to [0, 1]:
#   - DP2/DP3 are change-from-baseline anchored to 2035 max by design → naturally bounded
#   - DP1 population growth does not meaningfully exceed 2025 worst-case
#   - AS is a 2025 snapshot held constant → always within its own range
# SP indicators are LEFT UNBOUNDED:
#   - SP1/SP3 measure workforce shortages that worsen structurally beyond 2025 reference
#   - Values > 1 carry genuine information: shortage pressure exceeds the 2025 worst-case
#   - Clipping SP would mask the primary deterioration signal the index is designed to capture
#   - Hotspot classification uses the empirical 67th-percentile threshold (not a fixed value)
#     so the composite CGI remains interpretable even when SP > 1
#
# For AS3_raw (negated provider density), the inversion is already embedded in the raw
# value (multiplied by -1 in 05_access.R), so standard min-max applies.
#
# Degenerate case: if min_2025 == max_2025 for any indicator (all cells identical),
# set all normalized values to 0.5.
#
# Output: indicators_norm.rds — 900 rows with both _raw and _norm columns,
#         plus norm_reference.rds — the 9 min/max reference values (for MC reuse)

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

message("Loading processed inputs...")
indicators <- readRDS(file.path(PROCESSED_DIR, "indicators.rds"))

# Indicators to normalize (in order)
RAW_COLS  <- c("DP1_raw", "DP2_raw", "DP3_raw",
               "SP1_raw", "SP2_raw", "SP3_raw",
               "AS1_raw", "AS2_raw", "AS3_raw")
NORM_COLS <- c("DP1", "DP2", "DP3",
               "SP1", "SP2", "SP3",
               "AS1", "AS2", "AS3")

# ── Step 1: Compute reference distribution ────────────────────────────────────
# Standard approach: min/max from 2025 cross-section.
# EXCEPTION — DP2 and DP3 are zero for ALL cells at 2025 by construction
# (they measure CHANGE from 2025 → 2025 = 0).  For these, we use:
#   min_ref = 0  (no change at base year, by definition)
#   max_ref = max value at 2035  (worst observed deterioration)
# This preserves the 0 = "no change" anchor and keeps the scale meaningful.
message("Computing reference min/max values for normalization...")

base_cells <- indicators %>% filter(year == YEAR_BASE)
end_cells  <- indicators %>% filter(year == max(YEARS_ANALYSIS))

norm_ref <- tibble(indicator = RAW_COLS) %>%
  mutate(
    min_2025 = map_dbl(indicator, function(col) min(base_cells[[col]], na.rm = TRUE)),
    max_2025 = map_dbl(indicator, function(col) max(base_cells[[col]], na.rm = TRUE)),
    # For change-based indicators (DP2, DP3): override max with 2035 max
    max_ref  = case_when(
      indicator %in% c("DP2_raw", "DP3_raw") ~
        map_dbl(indicator, function(col) max(end_cells[[col]], na.rm = TRUE)),
      TRUE ~ max_2025
    ),
    min_ref  = case_when(
      indicator %in% c("DP2_raw", "DP3_raw") ~ 0,  # min = 0 (no change at base year)
      TRUE ~ min_2025
    ),
    range_ref  = max_ref - min_ref,
    degenerate = range_ref < 1e-12
  )

message("  Normalization reference values:")
message(capture.output(
  print(as.data.frame(norm_ref %>% select(indicator, min_ref, max_ref, range_ref, degenerate)),
        row.names = FALSE)) %>%
  paste(collapse = "\n"))

if (any(norm_ref$degenerate)) {
  message("  WARNING — degenerate indicators (all reference cells equal → set to 0.5): ",
          paste(norm_ref$indicator[norm_ref$degenerate], collapse = ", "))
}

# ── Step 2: Apply min-max normalization ───────────────────────────────────────
message("Applying min-max normalization (fixed 2025 reference)...")

indicators_norm <- indicators

for (i in seq_along(RAW_COLS)) {
  raw_col  <- RAW_COLS[i]
  norm_col <- NORM_COLS[i]
  ref      <- norm_ref[norm_ref$indicator == raw_col, ]

  if (ref$degenerate) {
    indicators_norm[[norm_col]] <- if_else(!is.na(indicators[[raw_col]]), 0.5, NA_real_)
  } else {
    raw_norm <- (indicators[[raw_col]] - ref$min_ref) / ref$range_ref
    # SP indicators: unbounded — preserves structural deterioration signal beyond 2025
    # DP and AS indicators: clipped to [0, 1] — bounded by design or data availability
    indicators_norm[[norm_col]] <- if (norm_col %in% c("SP1", "SP2", "SP3")) {
      raw_norm
    } else {
      pmin(pmax(raw_norm, 0), 1)
    }
  }
}

# ── Step 3: Validation ────────────────────────────────────────────────────────
message("Validating normalized indicators...")

# At 2025: all normalized values should be in [0, 1]
base_norm <- indicators_norm %>% filter(year == YEAR_BASE)

for (nc in NORM_COLS) {
  x   <- base_norm[[nc]]
  out <- x[!is.na(x) & (x < 0 | x > 1)]
  if (length(out) > 0)
    message("  WARNING: ", nc, " has ", length(out),
            " 2025 values outside [0,1] — range: ",
            round(min(out), 4), " – ", round(max(out), 4))
}

# Summary of normalized ranges across all years
norm_ranges <- indicators_norm %>%
  summarise(across(all_of(NORM_COLS),
                   list(min = ~ min(., na.rm = TRUE),
                        max = ~ max(., na.rm = TRUE)),
                   .names = "{.col}_{.fn}"))

message("\n  Normalized indicator ranges (all years):")
norm_summary <- tibble(
  indicator = NORM_COLS,
  min_all   = as.numeric(norm_ranges[, paste0(NORM_COLS, "_min")]),
  max_all   = as.numeric(norm_ranges[, paste0(NORM_COLS, "_max")])
)
message(capture.output(print(as.data.frame(norm_summary), row.names = FALSE)) %>%
          paste(collapse = "\n"))

# Count cells > 1 in 2030/2035 (valid: deterioration past 2025 worst case)
n_exceed <- indicators_norm %>%
  filter(year > YEAR_BASE) %>%
  summarise(across(all_of(NORM_COLS), ~ sum(. > 1, na.rm = TRUE)))

message("\n  Cells > 1.0 in future years (valid signal of worsening):")
message(capture.output(print(as.data.frame(n_exceed), row.names = FALSE)) %>%
          paste(collapse = "\n"))

# ── Step 4: Save ──────────────────────────────────────────────────────────────
saveRDS(indicators_norm,
        file.path(PROCESSED_DIR, "indicators_norm.rds"))
saveRDS(norm_ref %>% select(indicator, min_ref, max_ref, range_ref, degenerate),
        file.path(PROCESSED_DIR, "norm_reference.rds"))

message("\n=== 07_normalize.R complete ===")
message("  indicators_norm.rds : ", nrow(indicators_norm), " rows")
message("  norm_reference.rds  : 9 indicator min/max reference values (2025)")
message("  DP + AS indicators: clipped to [0,1]; SP indicators: unbounded")
