# Script: 04_supply.R — Purpose: DAAN workforce → specialism-level effective supply
#
# Implements Equation 2 — productivity bridge (φ):
#   φ_{s,p} = ExpectedDBC_{p,s,2025} / FTE_relevant_{p,s,2025}
#
#   FTE_relevant_{p,s,t} = Σ_sector M[s,sector] × FTE_{p,sector,t}
#
# Spreads sector-level DAAN FTE across specialisms using the M matrix as weights.
# φ is calibrated at 2025 base year, then held fixed for 2030/2035 projections.
# Province × specialism cells with zero FTE fall back to national median φ.
#
# Output: supply.rds  — province × category × year with supply backbone columns
#         phi.rds     — province × category (baseline productivity ratios)

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
workforce            <- readRDS(file.path(PROCESSED_DIR, "workforce.rds"))
demand               <- readRDS(file.path(PROCESSED_DIR, "demand.rds"))
M_long               <- readRDS(file.path(PROCESSED_DIR, "M_long.rds"))
supply_proxy_cats    <- readRDS(file.path(PROCESSED_DIR, "supply_proxy_categories.rds"))

# ── Step 1: Validate workforce years ──────────────────────────────────────────
message("Workforce year coverage: ", paste(sort(unique(workforce$year)), collapse = ", "))

years_in_wf      <- unique(workforce$year)
years_missing    <- setdiff(YEARS_ANALYSIS, years_in_wf)
if (length(years_missing) > 0) {
  stop("Workforce data missing required years: ", paste(years_missing, collapse = ", "),
       "\n  Available years: ", paste(sort(years_in_wf), collapse = ", "))
}

# ── Step 2: Filter to DBC-producing sectors and analysis years ─────────────
wf_dbc <- workforce %>%
  filter(sector %in% DBC_SECTORS, year %in% YEARS_ANALYSIS)

message("DBC-sector workforce rows: ", nrow(wf_dbc),
        " | sectors: ", paste(sort(unique(wf_dbc$sector)), collapse = ", "))

# Check sector coverage per province
sector_coverage <- wf_dbc %>%
  distinct(province, sector) %>%
  count(province) %>%
  filter(n < length(DBC_SECTORS))

if (nrow(sector_coverage) > 0) {
  message("  WARNING — some provinces have fewer than ", length(DBC_SECTORS),
          " DBC sectors:")
  message("  ", paste(sector_coverage$province, "(", sector_coverage$n, "sectors)"),
          collapse = ", ")
}

# ── Step 3: Apply M matrix — compute FTE_relevant per province × category × year
# FTE_relevant_{p,s,t} = Σ_sector M[s,sector] × werkende_{p,sector,t}
# tekort_relevant uses same M weights
# share_55plus weighted by (share × werkende) to give a specialism-specific
# workforce-age profile
message("Computing FTE_relevant via M matrix...")

fte_panel <- wf_dbc %>%
  # Join with M_long: each workforce row (province,sector,year) matched to
  # all specialism categories that use that sector
  inner_join(M_long, by = "sector",
             relationship = "many-to-many") %>%  # expected: each sector row matches many categories
  group_by(province, category, year) %>%
  summarise(
    fte_relevant        = sum(share * werkende,   na.rm = TRUE),
    tekort_relevant     = sum(share * tekort,     na.rm = TRUE),
    # Workforce-age: weighted average share_55plus (weight = share × werkende)
    wt_sum              = sum(share * werkende,   na.rm = TRUE),
    wt_55plus_num       = sum(share * werkende * share_55plus, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    share_55plus_wt = if_else(wt_sum > 0, wt_55plus_num / wt_sum, NA_real_)
  ) %>%
  select(-wt_sum, -wt_55plus_num)

# Validate: 12 × 25 × 3 = 900 rows
stopifnot("fte_panel: 900 rows expected" = nrow(fte_panel) == 900)

message("  fte_panel: ", nrow(fte_panel), " rows")
message("  FTE_relevant range (2025): ",
        round(min(fte_panel$fte_relevant[fte_panel$year == 2025]), 0), " – ",
        round(max(fte_panel$fte_relevant[fte_panel$year == 2025]), 0))

# ── Step 4: Calibrate productivity bridge φ at base year ──────────────────────
message("Calibrating productivity bridge φ (base year ", YEAR_BASE, ")...")

phi_raw <- fte_panel %>%
  filter(year == YEAR_BASE) %>%
  left_join(
    demand %>%
      filter(year == YEAR_BASE) %>%
      select(province, category, expected_dbc),
    by = c("province", "category")
  ) %>%
  mutate(
    phi = if_else(fte_relevant > 0, expected_dbc / fte_relevant, NA_real_)
  )

# National median φ fallback (per category) for zero-FTE cells
phi_national <- phi_raw %>%
  group_by(category) %>%
  summarise(phi_national = median(phi, na.rm = TRUE), .groups = "drop")

# Impute missing / infinite φ with national median
phi_imputed <- phi_raw %>%
  left_join(phi_national, by = "category") %>%
  mutate(
    phi_source = case_when(
      is.na(phi) | !is.finite(phi) ~ "national_median",
      TRUE                          ~ "province"
    ),
    phi = if_else(phi_source == "national_median", phi_national, phi)
  ) %>%
  select(province, category,
         fte_base   = fte_relevant,
         tekort_base = tekort_relevant,
         phi, phi_source)

n_imputed <- sum(phi_imputed$phi_source == "national_median")
if (n_imputed > 0)
  message("  φ imputed (zero FTE at base year): ", n_imputed, " province×category cells")

# Sanity: φ should be in a plausible range for DBCs per FTE per year
phi_range <- range(phi_imputed$phi, na.rm = TRUE)
message("  φ range: ", round(phi_range[1], 0), " – ", round(phi_range[2], 0),
        " DBCs per FTE per year")
message("  φ median across all cells: ", round(median(phi_imputed$phi, na.rm = TRUE), 0))

# Flag implausibly high or low φ (>2× or <0.5× national median per category)
phi_outliers <- phi_imputed %>%
  left_join(phi_national, by = "category") %>%
  filter(phi > 2 * phi_national | phi < 0.5 * phi_national,
         phi_source == "province") %>%
  select(province, category, phi, phi_national)

if (nrow(phi_outliers) > 0) {
  message("  WARNING — ", nrow(phi_outliers), " φ outliers (>2× or <0.5× national median):")
  phi_outliers %>%
    arrange(desc(phi / phi_national)) %>%
    head(10) %>%
    { message(capture.output(print(as.data.frame(.), row.names = FALSE)) %>%
                paste(collapse = "\n")) }
}

# ── Step 5: Compute effective supply for all years ────────────────────────────
message("Computing effective supply for all analysis years...")

supply <- fte_panel %>%
  left_join(phi_imputed %>% select(province, category, phi, phi_source),
            by = c("province", "category")) %>%
  mutate(
    effective_supply = phi * fte_relevant,
    supply_proxy     = category %in% supply_proxy_cats
  ) %>%
  # Join demand columns for convenience (used in 06_indicators.R)
  left_join(
    demand %>% select(province, category, year, expected_dbc,
                      pop_total, share_65plus, epsilon),
    by = c("province", "category", "year")
  ) %>%
  select(province, category, year,
         fte_relevant, tekort_relevant, share_55plus_wt,
         phi, phi_source,
         effective_supply, expected_dbc,
         pop_total, share_65plus, epsilon,
         supply_proxy)

stopifnot("supply panel: 900 rows expected" = nrow(supply) == 900)

# ── Step 6: Sanity checks ─────────────────────────────────────────────────────
message("Running sanity checks...")

# 6a. At base year, effective_supply should equal expected_dbc by construction
supply_demand_check <- supply %>%
  filter(year == YEAR_BASE) %>%
  summarise(
    max_abs_diff = max(abs(effective_supply - expected_dbc), na.rm = TRUE),
    n_imputed_cells = sum(phi_source == "national_median")
  )

message("  Base-year supply ≈ demand check (max |diff|): ",
        round(supply_demand_check$max_abs_diff, 3),
        " (should be ~0 for province-calibrated cells, non-zero for imputed cells)")

# 6b. FTE growth 2025→2035 by sector (validation — expect modest growth or decline)
fte_growth <- fte_panel %>%
  filter(category == "Cardiology") %>%
  select(province, year, fte_relevant) %>%
  pivot_wider(names_from = year, values_from = fte_relevant,
              names_prefix = "y") %>%
  mutate(fte_growth_pct = round((y2035 - y2025) / y2025 * 100, 1)) %>%
  arrange(fte_growth_pct)

message("\n  Cardiology FTE_relevant growth 2025→2035 by province (%):")
message(capture.output(print(as.data.frame(fte_growth), row.names = FALSE)) %>%
          paste(collapse = "\n"))

# 6c. tekort_relevant 2025 — top shortage specialisms nationally
top_tekort <- supply %>%
  filter(year == YEAR_BASE) %>%
  group_by(category) %>%
  summarise(nl_tekort = sum(tekort_relevant, na.rm = TRUE), .groups = "drop") %>%
  arrange(nl_tekort) %>%   # most negative = biggest shortage
  head(10)

message("\n  Top shortage categories (tekort_relevant sum, 2025):")
message(capture.output(print(as.data.frame(top_tekort), row.names = FALSE)) %>%
          paste(collapse = "\n"))

# ── Step 7: Save ───────────────────────────────────────────────────────────────
saveRDS(supply,       file.path(PROCESSED_DIR, "supply.rds"))
saveRDS(phi_imputed,  file.path(PROCESSED_DIR, "phi.rds"))

message("\n=== 04_supply.R complete ===")
message("  supply.rds : ", nrow(supply), " rows (province × category × year)")
message("  phi.rds    : ", nrow(phi_imputed), " rows (province × category)")
message("  Key columns: fte_relevant, tekort_relevant, share_55plus_wt,",
        " phi, effective_supply, supply_proxy")
