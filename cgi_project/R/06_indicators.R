# Script: 06_indicators.R — Purpose: Compute all 9 raw CGI indicators
#
# Demand Pressure (DP):
#   DP1 — Per-capita need intensity   : ExpectedDBC_{p,s,t} / Pop_{p,t}
#   DP2 — Ageing momentum             : Δshare_65plus_{p,2025→t} × epsilon_s
#   DP3 — Demand growth rate          : (ExpectedDBC_{p,s,t} - ExpectedDBC_{p,s,2025})
#                                       / ExpectedDBC_{p,s,2025}
#
# Supply Pressure (SP):
#   SP1 — Shortage rate               : tekort_relevant / fte_relevant  (proportion)
#   SP2 — Retirement pressure         : share_55plus_wt  (proportion in [0,1])
#   SP3 — Productivity-adjusted gap   : (φ × tekort_relevant) / ExpectedDBC  (proportion)
#
# Access Stress (AS) — 2025 snapshot, held constant across years:
#   AS1 — Treeknorm breach rate       : see 05_access.R
#   AS2 — Mean wait days (z-scored)   : see 05_access.R
#   AS3 — Provider density (inverted) : see 05_access.R
#
# Output: indicators.rds — province × category × year with all 9 raw indicators
#         (DP1_raw…AS3_raw) plus metadata (n_pillars_available, incomplete)

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
supply  <- readRDS(file.path(PROCESSED_DIR, "supply.rds"))   # 900 rows: p×s×t backbone
access  <- readRDS(file.path(PROCESSED_DIR, "access.rds"))   # 300 rows: p×s (no year)

# supply already contains: province, category, year, fte_relevant, tekort_relevant,
#   share_55plus_wt, phi, effective_supply, expected_dbc, pop_total, share_65plus, epsilon

# ── Step 1: DP indicators ──────────────────────────────────────────────────────
message("Computing DP indicators...")

# Share_65plus at base year (needed for DP2 delta) — one row per province
# share_65plus is province×year level (same across categories), so distinct() is safe
share_65_base <- supply %>%
  filter(year == YEAR_BASE) %>%
  distinct(province, share_65plus) %>%
  rename(share_65plus_base = share_65plus)

dp <- supply %>%
  left_join(share_65_base, by = "province") %>%
  mutate(
    # DP1: per-capita DBC need intensity [DBCs per person]
    DP1_raw = expected_dbc / pop_total,

    # DP2: ageing momentum [dimensionless — percentage-point change × elasticity]
    # Δshare_65plus from base year, multiplied by specialism age-elasticity ε_s
    # At base year (2025): DP2 = 0 by definition
    DP2_raw = (share_65plus - share_65plus_base) * epsilon,

    # DP3: cumulative demand growth from 2025 [proportion; 0 at base year]
    # computed in Step 1b via a join — placeholder here
    DP3_raw = NA_real_
  )

# DP3 requires base-year expected_dbc as denominator
dbc_base <- supply %>%
  filter(year == YEAR_BASE) %>%
  select(province, category, expected_dbc_base = expected_dbc)

dp <- dp %>%
  left_join(dbc_base, by = c("province", "category")) %>%
  mutate(
    DP3_raw = if_else(
      expected_dbc_base > 0,
      (expected_dbc - expected_dbc_base) / expected_dbc_base,
      NA_real_
    )
  ) %>%
  select(province, category, year,
         DP1_raw, DP2_raw, DP3_raw,
         # carry-forward columns needed for SP
         fte_relevant, tekort_relevant, share_55plus_wt,
         phi, phi_source,
         expected_dbc, pop_total, share_65plus, epsilon, supply_proxy)

message("  DP1 range (2025): ",
        round(min(dp$DP1_raw[dp$year == 2025]), 4), " – ",
        round(max(dp$DP1_raw[dp$year == 2025]), 4), " DBCs/person")
message("  DP2 range (2035): ",
        round(min(dp$DP2_raw[dp$year == 2035], na.rm = TRUE), 4), " – ",
        round(max(dp$DP2_raw[dp$year == 2035], na.rm = TRUE), 4))
message("  DP3 range (2035): ",
        round(min(dp$DP3_raw[dp$year == 2035], na.rm = TRUE), 4), " – ",
        round(max(dp$DP3_raw[dp$year == 2035], na.rm = TRUE), 4))

# ── Step 2: SP indicators ──────────────────────────────────────────────────────
message("Computing SP indicators...")

sp <- dp %>%
  mutate(
    # SP1: shortage rate [proportion; positive = deficit, negative = surplus]
    # tekort_relevant / fte_relevant
    SP1_raw = if_else(fte_relevant > 0, tekort_relevant / fte_relevant, NA_real_),

    # SP2: retirement pressure [proportion of workforce aged 55+]
    SP2_raw = share_55plus_wt,

    # SP3: productivity-adjusted FTE gap [proportion relative to expected DBC volume]
    # (φ × tekort_relevant) / ExpectedDBC
    # Positive SP3 = supply shortage in DBC-equivalent units relative to demand
    SP3_raw = if_else(
      expected_dbc > 0,
      (phi * tekort_relevant) / expected_dbc,
      NA_real_
    )
  )

message("  SP1 range (2025): ",
        round(min(sp$SP1_raw[sp$year == 2025], na.rm = TRUE), 4), " – ",
        round(max(sp$SP1_raw[sp$year == 2025], na.rm = TRUE), 4))
message("  SP2 range (2025): ",
        round(min(sp$SP2_raw[sp$year == 2025], na.rm = TRUE), 4), " – ",
        round(max(sp$SP2_raw[sp$year == 2025], na.rm = TRUE), 4))
message("  SP3 range (2025): ",
        round(min(sp$SP3_raw[sp$year == 2025], na.rm = TRUE), 4), " – ",
        round(max(sp$SP3_raw[sp$year == 2025], na.rm = TRUE), 4))

# ── Step 3: AS indicators — join snapshot (repeated for 2030, 2035) ───────────
message("Joining AS indicators (2025 snapshot held constant)...")

indicators <- sp %>%
  left_join(
    access %>% select(province, category,
                      AS1_raw, AS2_raw, AS3_raw,
                      mean_wait_days, density_per_100k,
                      n_providers, n_observations, has_access_data),
    by = c("province", "category")
  )

# ── Step 4: Completeness flags ─────────────────────────────────────────────────
message("Computing completeness flags...")

indicators <- indicators %>%
  mutate(
    # Count available pillars for this cell
    dp_available = !is.na(DP1_raw) & !is.na(DP2_raw) & !is.na(DP3_raw),
    sp_available = !is.na(SP1_raw) & !is.na(SP2_raw) & !is.na(SP3_raw),
    as_available = has_access_data,   # TRUE if AS data exists for this p×s cell

    n_pillars_available = as.integer(dp_available) +
                          as.integer(sp_available) +
                          as.integer(as_available),

    incomplete = n_pillars_available < 3L
  )

message("  Cells with all 3 pillars: ",
        sum(indicators$n_pillars_available == 3 & indicators$year == 2025),
        " / 300 (2025)")
message("  Cells with 2 pillars (DP+SP only): ",
        sum(indicators$n_pillars_available == 2 & indicators$year == 2025),
        " / 300 (2025)")
message("  Cells with <2 pillars: ",
        sum(indicators$n_pillars_available < 2 & indicators$year == 2025),
        " / 300 (2025)")

# ── Step 5: Sanity checks ──────────────────────────────────────────────────────
message("Running sanity checks...")

# 5a. At 2025: DP2 and DP3 should be 0 everywhere
dp2_2025 <- indicators %>%
  filter(year == YEAR_BASE) %>%
  summarise(max_abs_dp2 = max(abs(DP2_raw), na.rm = TRUE),
            max_abs_dp3 = max(abs(DP3_raw), na.rm = TRUE))
message("  Base year DP2 max |deviation from 0|: ", round(dp2_2025$max_abs_dp2, 8))
message("  Base year DP3 max |deviation from 0|: ", round(dp2_2025$max_abs_dp3, 8))

# 5b. Check NAs in core indicators at 2025
na_counts <- indicators %>%
  filter(year == 2025) %>%
  summarise(across(c(DP1_raw, DP2_raw, DP3_raw,
                     SP1_raw, SP2_raw, SP3_raw,
                     AS1_raw, AS2_raw, AS3_raw),
                   ~ sum(is.na(.))))
message("\n  NA counts per indicator (2025):")
message(capture.output(print(as.data.frame(na_counts), row.names = FALSE)) %>%
          paste(collapse = "\n"))

# 5c. Spot-check: top DP1 cells (highest per-capita need) at 2025
top_dp1 <- indicators %>%
  filter(year == 2025) %>%
  arrange(desc(DP1_raw)) %>%
  head(5) %>%
  select(province, category, DP1_raw, expected_dbc, pop_total)
message("\n  Top 5 DP1 (per-capita DBC need) 2025:")
message(capture.output(print(as.data.frame(top_dp1), row.names = FALSE)) %>%
          paste(collapse = "\n"))

# 5d. Province-level SP1 at 2025: check for extreme shortages
top_sp1 <- indicators %>%
  filter(year == 2025) %>%
  group_by(province) %>%
  summarise(mean_SP1 = mean(SP1_raw, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(mean_SP1))
message("\n  Province-level mean SP1 (shortage rate) 2025:")
message(capture.output(print(as.data.frame(top_sp1), row.names = FALSE)) %>%
          paste(collapse = "\n"))

# ── Step 6: Save ───────────────────────────────────────────────────────────────
indicators_out <- indicators %>%
  select(province, category, year,
         DP1_raw, DP2_raw, DP3_raw,
         SP1_raw, SP2_raw, SP3_raw,
         AS1_raw, AS2_raw, AS3_raw,
         # metadata
         expected_dbc, pop_total, share_65plus, epsilon,
         fte_relevant, tekort_relevant, phi, phi_source,
         mean_wait_days, density_per_100k, n_providers,
         supply_proxy, has_access_data,
         dp_available, sp_available, as_available,
         n_pillars_available, incomplete)

stopifnot("indicators: 900 rows" = nrow(indicators_out) == 900)

saveRDS(indicators_out, file.path(PROCESSED_DIR, "indicators.rds"))

message("\n=== 06_indicators.R complete ===")
message("  indicators.rds: ", nrow(indicators_out), " rows (province × category × year)")
message("  9 raw indicators: DP1–DP3, SP1–SP3, AS1–AS3")
message("  ", sum(indicators_out$incomplete & indicators_out$year == 2025),
        " province×category cells flagged incomplete (2-pillar CGI) at 2025")
