# Script: 11_hotspots.R — Purpose: Hotspot classification
#
# A cell (p, s, t) is a HOTSPOT if ALL THREE conditions hold:
#   1. CGI_{p,s,t} > 0.66        (top tercile of 2025 distribution)
#   2. CGI_lo90_{p,s,t} > 0.50   (survives uncertainty — lower 90% PI > 0.5)
#   3. ALL three sub-indices > 0.50 (multi-pillar stress; for 2-pillar cells: DP > 0.50 & SP > 0.50)
#
# Output: hotspots.rds    — province × category × year with hotspot_flag
#         cgi_rollup.rds  — updated with hotspot_count per province × year

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
cgi           <- readRDS(file.path(PROCESSED_DIR, "cgi.rds"))
cgi_intervals <- readRDS(file.path(PROCESSED_DIR, "cgi_intervals.rds"))
cgi_rollup    <- readRDS(file.path(PROCESSED_DIR, "cgi_rollup.rds"))

# ── Step 1: CGI threshold — 2025 empirical maximum ────────────────────────────
# Since all indicators are clipped to [0,1], future CGI values are bounded by
# the 2025 normalization ceiling. The 2025 empirical maximum is therefore the
# most meaningful threshold: a cell is classified as a hotspot when its projected
# CGI exceeds the worst case observed across any province × specialism in 2025.
# This anchors the hotspot definition directly to observed baseline conditions
# rather than an arbitrary point on the [0,1] scale.
message("Computing hotspot thresholds...")

cgi_2025_vals <- cgi %>%
  filter(year == 2025, !is.na(CGI)) %>%
  pull(CGI)

CGI_THRESHOLD <- max(cgi_2025_vals)
message("  CGI threshold (2025 empirical max): ", round(CGI_THRESHOLD, 4))
message("  CGI 2025 distribution: ",
        "p33=", round(quantile(cgi_2025_vals, 0.33), 3),
        " p66=", round(quantile(cgi_2025_vals, 0.66), 3),
        " p90=", round(quantile(cgi_2025_vals, 0.90), 3),
        " max=", round(CGI_THRESHOLD, 3))

# ── Step 2: Join PI lower bound ────────────────────────────────────────────────
hotspots <- cgi %>%
  left_join(
    cgi_intervals %>% select(province, category, year, CGI_lo90),
    by = c("province", "category", "year")
  ) %>%
  mutate(
    # Condition 1: CGI > top-tercile threshold
    cond1_cgi     = CGI > CGI_THRESHOLD,

    # Condition 2: lower 90% PI > 0.50 (robust under uncertainty)
    cond2_lo90    = CGI_lo90 > 0.50,

    # Condition 3: multi-pillar stress
    # For 3-pillar cells: all of DP, SP, AS > 0.50
    # For 2-pillar cells: DP > 0.50 AND SP > 0.50 (AS absent)
    cond3_pillars = case_when(
      n_pillars_cgi == 3 ~ DP > 0.50 & SP > 0.50 & AS > 0.50,
      n_pillars_cgi == 2 ~ DP > 0.50 & SP > 0.50,
      TRUE ~ FALSE
    ),

    hotspot_flag = cond1_cgi & cond2_lo90 & cond3_pillars & !is.na(CGI)
  )

# ── Step 3: Summary ────────────────────────────────────────────────────────────
message("Hotspot summary:")

for (yr in YEARS_ANALYSIS) {
  n_hs <- sum(hotspots$hotspot_flag[hotspots$year == yr], na.rm = TRUE)
  message("  ", yr, ": ", n_hs, " hotspot cells")
}

# Top hotspots at 2035
top_hs <- hotspots %>%
  filter(year == 2035, hotspot_flag) %>%
  arrange(desc(CGI)) %>%
  select(province, category, CGI, DP, SP, AS, CGI_lo90, n_pillars_cgi) %>%
  head(20)

message("\n  Top hotspot cells (2035, sorted by CGI):")
message(capture.output(print(as.data.frame(top_hs), row.names = FALSE)) %>%
          paste(collapse = "\n"))

# Hotspot provinces at 2035
hs_by_province <- hotspots %>%
  filter(year == 2035) %>%
  group_by(province) %>%
  summarise(
    n_hotspots    = sum(hotspot_flag, na.rm = TRUE),
    max_cgi       = round(max(CGI, na.rm = TRUE), 3),
    .groups = "drop"
  ) %>%
  arrange(desc(n_hotspots))

message("\n  Hotspot count by province (2035):")
message(capture.output(print(as.data.frame(hs_by_province), row.names = FALSE)) %>%
          paste(collapse = "\n"))

# ── Step 4: Update cgi_rollup with hotspot counts ─────────────────────────────
hotspot_counts <- hotspots %>%
  group_by(province, year) %>%
  summarise(hotspot_count = sum(hotspot_flag, na.rm = TRUE), .groups = "drop")

cgi_rollup_updated <- cgi_rollup %>%
  select(-hotspot_count) %>%
  left_join(hotspot_counts, by = c("province", "year"))

# ── Step 5: Save ───────────────────────────────────────────────────────────────
saveRDS(hotspots,           file.path(PROCESSED_DIR, "hotspots.rds"))
saveRDS(cgi_rollup_updated, file.path(PROCESSED_DIR, "cgi_rollup.rds"))

message("\n=== 11_hotspots.R complete ===")
message("  hotspots.rds    : ", nrow(hotspots), " rows")
message("  CGI threshold   : ", round(CGI_THRESHOLD, 4))
message("  Hotspots 2025   : ",
        sum(hotspots$hotspot_flag[hotspots$year == 2025], na.rm = TRUE))
message("  Hotspots 2035   : ",
        sum(hotspots$hotspot_flag[hotspots$year == 2035], na.rm = TRUE))
