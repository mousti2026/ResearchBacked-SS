# Script: 08_aggregate.R — Purpose: Pillar + composite aggregation → CGI
#
# Within-pillar: arithmetic mean of 3 normalized indicators
#   DP = (DP1 + DP2 + DP3) / 3
#   SP = (SP1 + SP2 + SP3) / 3
#   AS = (AS1 + AS2 + AS3) / 3      (NA for incomplete cells)
#
# Across-pillar composite:
#   If 3 pillars available: CGI = (DP + SP + AS) / 3
#   If 2 pillars (DP + SP only, AS = NA): CGI = (DP + SP) / 2
#   If < 2 pillars: CGI = NA
#
# Province-level rollup (volume-weighted across specialisms):
#   CGI_{p,t} = Σ_s [ (DBC_{p,s,2025} / Σ_s DBC_{p,s,2025}) × CGI_{p,s,t} ]
#
# Output: cgi.rds         — province × category × year (full panel with DP, SP, AS, CGI)
#         cgi_rollup.rds  — province × year (volume-weighted rollup)

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
indicators_norm <- readRDS(file.path(PROCESSED_DIR, "indicators_norm.rds"))

# ── Step 1: Pillar aggregation ─────────────────────────────────────────────────
message("Aggregating pillars (arithmetic mean within pillar)...")

cgi <- indicators_norm %>%
  mutate(
    # Demand Pressure pillar
    DP = (DP1 + DP2 + DP3) / 3,

    # Supply Pressure pillar
    SP = (SP1 + SP2 + SP3) / 3,

    # Access Stress pillar — NA for cells without wachttijden data
    AS = if_else(has_access_data, (AS1 + AS2 + AS3) / 3, NA_real_)
  )

# ── Step 2: Composite CGI ──────────────────────────────────────────────────────
message("Computing composite CGI...")

cgi <- cgi %>%
  mutate(
    # n_pillars_available is already in the data from 06_indicators.R
    # but recalculate based on actual NA status after normalization
    n_pillars_cgi = as.integer(!is.na(DP)) +
                    as.integer(!is.na(SP)) +
                    as.integer(!is.na(AS)),

    CGI = case_when(
      n_pillars_cgi == 3 ~ (DP + SP + AS) / 3,
      n_pillars_cgi == 2 & !is.na(DP) & !is.na(SP) ~ (DP + SP) / 2,
      TRUE ~ NA_real_
    ),

    # Flag: cells using only 2 pillars
    incomplete = n_pillars_cgi < 3
  )

message("  CGI computed for ", sum(!is.na(cgi$CGI)), " / 900 cells")
message("  3-pillar cells: ", sum(cgi$n_pillars_cgi == 3))
message("  2-pillar cells: ", sum(cgi$n_pillars_cgi == 2))
message("  Incomplete (<2): ", sum(cgi$n_pillars_cgi < 2))

# ── Step 3: Province-level rollup ──────────────────────────────────────────────
message("Computing province-level volume-weighted rollup...")

# DBC weights from 2025 expected demand (province × category share)
dbc_weights <- cgi %>%
  filter(year == YEAR_BASE) %>%
  group_by(province) %>%
  mutate(
    w_s = expected_dbc / sum(expected_dbc, na.rm = TRUE)  # share within province
  ) %>%
  ungroup() %>%
  select(province, category, w_s)

# Weighted rollup across specialisms for each province × year
cgi_rollup <- cgi %>%
  left_join(dbc_weights, by = c("province", "category")) %>%
  group_by(province, year) %>%
  summarise(
    CGI  = sum(w_s * CGI,  na.rm = TRUE),
    DP   = sum(w_s * DP,   na.rm = TRUE),
    SP   = sum(w_s * SP,   na.rm = TRUE),
    AS   = if_else(
             all(is.na(AS)),
             NA_real_,
             sum(w_s * AS, na.rm = TRUE) / sum(w_s[!is.na(AS)], na.rm = TRUE)
           ),
    hotspot_count = 0L,  # populated in 11_hotspots.R
    .groups = "drop"
  )

# ── Step 4: Sanity checks ──────────────────────────────────────────────────────
message("Running sanity checks...")

# 4a. CGI range at 2025
cgi_2025 <- cgi %>% filter(year == 2025, !is.na(CGI))
message("  CGI range 2025 (cell level): ",
        round(min(cgi_2025$CGI), 3), " – ", round(max(cgi_2025$CGI), 3))
message("  CGI mean 2025              : ", round(mean(cgi_2025$CGI), 3))

# 4b. Province rollup at 2025 and 2035 (ordered by 2035 CGI)
rollup_wide <- cgi_rollup %>%
  select(province, year, CGI) %>%
  pivot_wider(names_from = year, values_from = CGI, names_prefix = "CGI_") %>%
  arrange(desc(CGI_2035))

message("\n  Province CGI rollup:")
message(capture.output(print(as.data.frame(rollup_wide), row.names = FALSE)) %>%
          paste(collapse = "\n"))

# 4c. Top 10 CGI cells at 2035
top_cgi <- cgi %>%
  filter(year == 2035, !is.na(CGI)) %>%
  arrange(desc(CGI)) %>%
  head(10) %>%
  select(province, category, CGI, DP, SP, AS, incomplete)

message("\n  Top 10 CGI cells (2035):")
message(capture.output(print(as.data.frame(top_cgi), row.names = FALSE)) %>%
          paste(collapse = "\n"))

# 4d. Face validity: are Flevoland, Utrecht, Noord-Holland, Zuid-Holland in top quartile 2035?
top_quartile <- cgi_rollup %>%
  filter(year == 2035) %>%
  mutate(rank_cgi = rank(-CGI)) %>%
  filter(province %in% FACE_VALIDITY_PROVINCES)

message("\n  Face-validity check — expected provinces in top quartile 2035:")
message(capture.output(print(as.data.frame(top_quartile %>% select(province, CGI, rank_cgi)),
                              row.names = FALSE)) %>%
          paste(collapse = "\n"))
n_top3 <- sum(top_quartile$rank_cgi <= 3)
message("  (", n_top3, "/4 of face-validity provinces in top 3 rank)")

# ── Step 5: Save ───────────────────────────────────────────────────────────────
cgi_out <- cgi %>%
  select(province, category, year,
         DP1, DP2, DP3, DP,
         SP1, SP2, SP3, SP,
         AS1, AS2, AS3, AS,
         CGI, n_pillars_cgi, incomplete,
         # carry raw for export
         DP1_raw, DP2_raw, DP3_raw,
         SP1_raw, SP2_raw, SP3_raw,
         AS1_raw, AS2_raw, AS3_raw,
         expected_dbc, pop_total, share_65plus,
         supply_proxy, has_access_data)

saveRDS(cgi_out,    file.path(PROCESSED_DIR, "cgi.rds"))
saveRDS(cgi_rollup, file.path(PROCESSED_DIR, "cgi_rollup.rds"))

message("\n=== 08_aggregate.R complete ===")
message("  cgi.rds        : ", nrow(cgi_out), " rows (province × category × year)")
message("  cgi_rollup.rds : ", nrow(cgi_rollup), " rows (province × year)")
message("  CGI range (all years): ",
        round(min(cgi_out$CGI, na.rm = TRUE), 3), " – ",
        round(max(cgi_out$CGI, na.rm = TRUE), 3))
