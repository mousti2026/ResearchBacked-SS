# Script: 09_uncertainty.R — Purpose: Monte Carlo uncertainty (1,000 draws)
#
# Perturbs four stochastic inputs independently per draw:
#   1. tekort      ~ N(tekort_relevant, sd_tekort_relevant)
#                    [sd from cross-gemeente variation in 01_ingest.R via supply.rds]
#   2. phi (φ)     ~ LogNormal(log(phi), MC_PHI_CV)    [CV = 15%]
#   3. psi_a (ψ_a) ~ N(psi_a, MC_CONTACT_CV × psi_a)  [CV = 10% per age band]
#   4. pillar weights ~ Dirichlet(1, 1, 1)              [uniform over simplex]
#
# For each draw: recompute FTE_relevant → SP1/SP3 → expected_dbc → DP1/DP3
#   → re-normalize → re-aggregate → store CGI_{p,s,t}
#
# Output: mc_results.rds   — province × category × year × draw (long, N_MC_DRAWS rows/cell)
#         cgi_intervals.rds — province × category × year with CGI_lo90, CGI_hi90, CGI_mean

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
indicators      <- readRDS(file.path(PROCESSED_DIR, "indicators.rds"))
indicators_norm <- readRDS(file.path(PROCESSED_DIR, "indicators_norm.rds"))
supply      <- readRDS(file.path(PROCESSED_DIR, "supply.rds"))
phi_data    <- readRDS(file.path(PROCESSED_DIR, "phi.rds"))
population  <- readRDS(file.path(PROCESSED_DIR, "population.rds"))
workforce   <- readRDS(file.path(PROCESSED_DIR, "workforce.rds"))
M_long      <- readRDS(file.path(PROCESSED_DIR, "M_long.rds"))
norm_ref    <- readRDS(file.path(PROCESSED_DIR, "norm_reference.rds"))

# ── Setup: base values and standard deviations ─────────────────────────────────
message("Preparing Monte Carlo inputs...")

# 1. Base tekort_relevant and its SD
#    SD estimated from within-province cross-sector variation via workforce SD
#    Approximation: use sd_tekort from workforce (province × sector level)
#    and propagate through M weighting
workforce_dbc_sd <- workforce %>%
  filter(sector %in% DBC_SECTORS, year %in% YEARS_ANALYSIS) %>%
  select(province, sector, year, sd_tekort) %>%
  replace_na(list(sd_tekort = 0))

# Compute SD of tekort_relevant by propagating M weights
sd_panel <- workforce_dbc_sd %>%
  inner_join(M_long, by = "sector", relationship = "many-to-many") %>%
  group_by(province, category, year) %>%
  summarise(
    # Variance of a weighted sum of independent normals: Σ (M_s,k)^2 × var_k
    sd_tekort_relevant = sqrt(sum(share^2 * sd_tekort^2, na.rm = TRUE)),
    .groups = "drop"
  )

# 2. Base phi per province × category (central estimates)
phi_base <- phi_data %>% select(province, category, phi_base = phi)

# 3. Population baseline for resampling ψ_a
pop_base <- contacts_per_age  # tibble: age_band, contacts_per_year

# 4. Pre-join demand backbone needed per draw
#    (expected_dbc = contact_potential × mu_s)
#    Need pop_summary (contact_potential) and dbc_shares (mu_s) at province×year level
pop_summary <- readRDS(file.path(PROCESSED_DIR, "pop_summary.rds"))
dbc_shares  <- readRDS(file.path(PROCESSED_DIR, "dbc_shares.rds"))
age_elast   <- readRDS(file.path(PROCESSED_DIR, "age_elasticity.rds"))

# Pre-compute share_65plus_base for DP2
share_65_base <- pop_summary %>%
  filter(year == YEAR_BASE) %>%
  distinct(province, share_65plus) %>%
  rename(share_65plus_base = share_65plus)

# ── MC helper: single draw ─────────────────────────────────────────────────────
# Returns a long data frame with CGI values for all (p, s, t) cells
run_mc_draw <- function(draw_id) {

  # ── 1. Perturb φ (log-normal) ──────────────────────────────────────────────
  phi_draw <- phi_base %>%
    mutate(
      phi = rlnorm(n(), meanlog = log(pmax(phi_base, 1e-6)), sdlog = MC_PHI_CV)
    )

  # ── 2. Perturb ψ_a (normal, CV = 10%) ─────────────────────────────────────
  psi_draw <- contacts_per_age %>%
    mutate(
      contacts_per_year = pmax(
        rnorm(n(), mean = contacts_per_year, sd = MC_CONTACT_CV * contacts_per_year),
        0.1   # floor to avoid non-positive contacts
      )
    )

  # Recompute contact_potential from perturbed ψ_a
  population_py <- population %>%
    left_join(psi_draw, by = "age_band")

  contact_pot_draw <- population_py %>%
    group_by(province, year) %>%
    summarise(
      contact_potential_d = sum(population * contacts_per_year, na.rm = TRUE),
      pop_total_d         = sum(population, na.rm = TRUE),
      pop_65plus_d        = sum(population[age_band == "65_plus"], na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(share_65plus_d = pop_65plus_d / pop_total_d)

  # Recompute expected_dbc from perturbed contact_potential
  expected_draw <- contact_pot_draw %>%
    cross_join(dbc_shares %>% select(category, mu_s)) %>%
    mutate(expected_dbc_d = contact_potential_d * mu_s) %>%
    left_join(age_elast %>% select(category, epsilon), by = "category") %>%
    left_join(share_65_base, by = "province")

  # ── 3. Perturb tekort_relevant (normal) ───────────────────────────────────
  tekort_draw <- supply %>%
    select(province, category, year, tekort_relevant) %>%
    left_join(sd_panel, by = c("province", "category", "year")) %>%
    replace_na(list(sd_tekort_relevant = 0)) %>%
    mutate(
      tekort_d = rnorm(n(), mean = tekort_relevant, sd = sd_tekort_relevant)
    ) %>%
    select(province, category, year, tekort_d)

  # ── 4. Pillar weights ~ Dirichlet(1,1,1) ──────────────────────────────────
  raw_w <- rexp(3, rate = 1)     # Dirichlet(1,1,1) = normalized Exponential(1)
  w     <- raw_w / sum(raw_w)    # w_DP, w_SP, w_AS

  # ── 5. Recompute indicators ────────────────────────────────────────────────
  draw_df <- supply %>%
    select(province, category, year,
           fte_relevant, share_55plus_wt, phi_source,
           has_access_data = supply_proxy) %>%    # use supply; AS fixed
    left_join(phi_draw,      by = c("province", "category")) %>%
    left_join(tekort_draw,   by = c("province", "category", "year")) %>%
    left_join(expected_draw %>%
                select(province, category, year,
                       expected_dbc_d, pop_total_d, share_65plus_d,
                       share_65plus_base, epsilon),
              by = c("province", "category", "year")) %>%
    # Recompute SP raw
    mutate(
      SP1_d = if_else(fte_relevant > 0, tekort_d / fte_relevant, NA_real_),
      SP2_d = share_55plus_wt,   # unchanged by perturbation
      SP3_d = if_else(expected_dbc_d > 0,
                      (phi * tekort_d) / expected_dbc_d,
                      NA_real_),
      # Recompute DP raw
      DP1_d = if_else(pop_total_d > 0, expected_dbc_d / pop_total_d, NA_real_),
      DP2_d = (share_65plus_d - share_65plus_base) * epsilon,
      DP3_d = if_else(
        year == YEAR_BASE, 0,
        NA_real_   # requires base expected_dbc — handled below
      )
    )

  # Compute DP3 base-year expected_dbc from draw
  dbc_base_d <- draw_df %>%
    filter(year == YEAR_BASE) %>%
    select(province, category, expected_dbc_base_d = expected_dbc_d)

  draw_df <- draw_df %>%
    left_join(dbc_base_d, by = c("province", "category")) %>%
    mutate(
      DP3_d = if_else(
        expected_dbc_base_d > 0,
        (expected_dbc_d - expected_dbc_base_d) / expected_dbc_base_d,
        NA_real_
      )
    )

  # ── 6. Normalize with same reference ──────────────────────────────────────
  .norm <- function(x, col_name) {
    ref <- norm_ref[norm_ref$indicator == col_name, ]
    if (ref$degenerate) return(if_else(!is.na(x), 0.5, NA_real_))
    (x - ref$min_ref) / ref$range_ref
  }

  draw_df <- draw_df %>%
    mutate(
      DP1_n = .norm(DP1_d, "DP1_raw"),
      DP2_n = .norm(DP2_d, "DP2_raw"),
      DP3_n = .norm(DP3_d, "DP3_raw"),
      SP1_n = .norm(SP1_d, "SP1_raw"),
      SP2_n = .norm(SP2_d, "SP2_raw"),
      SP3_n = .norm(SP3_d, "SP3_raw")
    )

  # AS is fixed (2025 snapshot, not perturbed) — use normalized values
  as_fixed <- indicators_norm %>%
    filter(year == YEAR_BASE) %>%
    select(province, category, AS1, AS2, AS3, has_access_data) %>%
    rename(has_access = has_access_data)

  draw_df <- draw_df %>%
    left_join(as_fixed, by = c("province", "category"))

  # ── 7. Aggregate with perturbed weights ───────────────────────────────────
  draw_df <- draw_df %>%
    mutate(
      DP_d = (DP1_n + DP2_n + DP3_n) / 3,
      SP_d = (SP1_n + SP2_n + SP3_n) / 3,
      AS_d = if_else(has_access, (AS1 + AS2 + AS3) / 3, NA_real_),

      n_pil = as.integer(!is.na(DP_d)) +
              as.integer(!is.na(SP_d)) +
              as.integer(!is.na(AS_d)),

      CGI_d = case_when(
        n_pil == 3 ~ w[1] * DP_d + w[2] * SP_d + w[3] * AS_d,
        n_pil == 2 & !is.na(DP_d) & !is.na(SP_d) ~ (DP_d + SP_d) / 2,
        TRUE ~ NA_real_
      )
    ) %>%
    select(province, category, year, CGI_d)

  draw_df$draw <- draw_id
  draw_df
}

# ── Run MC ─────────────────────────────────────────────────────────────────────
message("Running Monte Carlo (", N_MC_DRAWS, " draws)...")
set.seed(MC_SEED)

mc_list <- vector("list", N_MC_DRAWS)
for (i in seq_len(N_MC_DRAWS)) {
  mc_list[[i]] <- run_mc_draw(i)
  if (i %% 100 == 0) message("  Completed draw ", i, " / ", N_MC_DRAWS)
}
mc_results <- bind_rows(mc_list)

message("  MC complete: ", nrow(mc_results), " rows (",
        N_MC_DRAWS, " draws × 900 cells)")

# ── Compute 90% prediction intervals ──────────────────────────────────────────
message("Computing 90% prediction intervals...")

cgi_intervals <- mc_results %>%
  group_by(province, category, year) %>%
  summarise(
    CGI_mean  = mean(CGI_d,                       na.rm = TRUE),
    CGI_lo90  = quantile(CGI_d, probs = 0.05,     na.rm = TRUE),
    CGI_hi90  = quantile(CGI_d, probs = 0.95,     na.rm = TRUE),
    CGI_sd    = sd(CGI_d,                         na.rm = TRUE),
    .groups   = "drop"
  )

# Join central CGI for reference
cgi_central <- readRDS(file.path(PROCESSED_DIR, "cgi.rds")) %>%
  select(province, category, year, CGI_central = CGI)

cgi_intervals <- cgi_intervals %>%
  left_join(cgi_central, by = c("province", "category", "year"))

# ── Sanity check ───────────────────────────────────────────────────────────────
message("  PI width summary (CGI_hi90 - CGI_lo90):")
pi_summary <- cgi_intervals %>%
  mutate(pi_width = CGI_hi90 - CGI_lo90) %>%
  filter(year == 2025) %>%
  summarise(
    mean_width   = round(mean(pi_width,   na.rm = TRUE), 3),
    median_width = round(median(pi_width, na.rm = TRUE), 3),
    max_width    = round(max(pi_width,    na.rm = TRUE), 3)
  )
message("  Mean PI width (2025): ", pi_summary$mean_width,
        " | Median: ", pi_summary$median_width,
        " | Max: ", pi_summary$max_width)

# Central vs MC mean correlation
cor_check <- cgi_intervals %>%
  filter(year == 2025, !is.na(CGI_central), !is.na(CGI_mean)) %>%
  summarise(r = cor(CGI_central, CGI_mean, method = "spearman")) %>%
  pull()
message("  Spearman ρ (central vs MC mean, 2025): ", round(cor_check, 4))

# ── Province-level MC rollup (for app uncertainty markers) ────────────────────
message("Computing province-level MC rollup...")

# DBC weights from 2025 point estimates
dbc_weights <- cgi_central %>%
  filter(year == YEAR_BASE) %>%
  left_join(
    readRDS(file.path(PROCESSED_DIR, "demand.rds")) %>%
      filter(year == YEAR_BASE) %>%
      select(province, category, expected_dbc),
    by = c("province", "category")
  ) %>%
  group_by(province) %>%
  mutate(w = expected_dbc / sum(expected_dbc, na.rm = TRUE)) %>%
  ungroup() %>%
  select(province, category, w)

# Weighted province CGI per draw
province_draws <- mc_results %>%
  left_join(dbc_weights, by = c("province", "category")) %>%
  filter(!is.na(CGI_d), !is.na(w)) %>%
  group_by(province, year, draw) %>%
  summarise(CGI_prov = sum(CGI_d * w), .groups = "drop")

# Summarise into intervals + tier
province_mc <- province_draws %>%
  group_by(province, year) %>%
  summarise(
    CGI_mean = mean(CGI_prov,                    na.rm = TRUE),
    CGI_lo90 = quantile(CGI_prov, probs = 0.05,  na.rm = TRUE),
    CGI_hi90 = quantile(CGI_prov, probs = 0.95,  na.rm = TRUE),
    CGI_sd   = sd(CGI_prov,                      na.rm = TRUE),
    .groups  = "drop"
  ) %>%
  mutate(PI_width = CGI_hi90 - CGI_lo90) %>%
  group_by(year) %>%
  mutate(uncertainty_tier = case_when(
    PI_width <= quantile(PI_width, 1/3) ~ "Low",
    PI_width <= quantile(PI_width, 2/3) ~ "Medium",
    TRUE                                ~ "High"
  )) %>%
  ungroup()

# ── Save ───────────────────────────────────────────────────────────────────────
saveRDS(mc_results,    file.path(PROCESSED_DIR, "mc_results.rds"))
saveRDS(cgi_intervals, file.path(PROCESSED_DIR, "cgi_intervals.rds"))
saveRDS(province_mc,   file.path(PROCESSED_DIR, "province_mc.rds"))

message("\n=== 09_uncertainty.R complete ===")
message("  mc_results.rds    : ", nrow(mc_results), " rows")
message("  cgi_intervals.rds : ", nrow(cgi_intervals), " rows (province × category × year)")
message("  province_mc.rds   : ", nrow(province_mc), " rows (province × year, for app uncertainty markers)")
message("  90% PI computed; Spearman ρ(central, MC mean) = ", round(cor_check, 4))
