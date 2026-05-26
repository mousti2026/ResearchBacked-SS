# Script: 10_sensitivity.R — Purpose: Weight/normalization/aggregation robustness
#
# Runs six systematic sensitivity checks per CGI spec Part 5:
#   5.1 Normalization: (a) min-max [central], (b) z-score, (c) percentile rank
#   5.2 Weighting:    (a) equal [central], (b) demand-heavy, (c) supply-heavy,
#                     (d) access-heavy, (e) PCA-derived
#   5.3 Aggregation:  (a) arithmetic mean [central], (b) geometric mean
#   5.4 Leave-one-out: drop each of 9 indicators
#   5.5 M-matrix perturbation: ±10pp on zkh/umc shares
#   5.6 φ stress test: φ × 0.85 and φ × 1.15
#
# Report: Spearman ρ vs central, # top-4 provinces that change, max rank shift.
# Output: sensitivity_results.rds, sensitivity_summary.rds

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
indicators  <- readRDS(file.path(PROCESSED_DIR, "indicators.rds"))
cgi_central <- readRDS(file.path(PROCESSED_DIR, "cgi.rds"))
cgi_rollup  <- readRDS(file.path(PROCESSED_DIR, "cgi_rollup.rds"))
supply      <- readRDS(file.path(PROCESSED_DIR, "supply.rds"))
M_long      <- readRDS(file.path(PROCESSED_DIR, "M_long.rds"))
norm_ref    <- readRDS(file.path(PROCESSED_DIR, "norm_reference.rds"))
workforce_dbc <- readRDS(file.path(PROCESSED_DIR, "workforce.rds")) %>%
  filter(sector %in% DBC_SECTORS, year == YEAR_BASE)

# Province rollup weights (2025 DBC volume shares)
dbc_weights <- cgi_central %>%
  filter(year == YEAR_BASE) %>%
  group_by(province) %>%
  mutate(w_s = expected_dbc / sum(expected_dbc, na.rm = TRUE)) %>%
  ungroup() %>%
  select(province, category, w_s)

# Raw indicator matrix (2025 cells only)
RAW_COLS  <- c("DP1_raw","DP2_raw","DP3_raw","SP1_raw","SP2_raw","SP3_raw",
               "AS1_raw","AS2_raw","AS3_raw")
NORM_COLS <- c("DP1","DP2","DP3","SP1","SP2","SP3","AS1","AS2","AS3")

ind_2025 <- indicators %>%
  filter(year == YEAR_BASE) %>%
  select(province, category, all_of(RAW_COLS),
         has_access_data, n_pillars_available, incomplete, expected_dbc)

# ── Helper: normalize a matrix of raw indicators ──────────────────────────────
norm_minmax <- function(df, raw_cols, ref_tbl) {
  for (i in seq_along(raw_cols)) {
    rc  <- raw_cols[i]
    nc  <- NORM_COLS[i]
    ref <- ref_tbl[ref_tbl$indicator == rc, ]
    if (ref$degenerate) {
      df[[nc]] <- if_else(!is.na(df[[rc]]), 0.5, NA_real_)
    } else {
      df[[nc]] <- (df[[rc]] - ref$min_ref) / ref$range_ref
    }
  }
  df
}

norm_zscore <- function(df, raw_cols) {
  for (i in seq_along(raw_cols)) {
    rc <- raw_cols[i]
    nc <- NORM_COLS[i]
    x  <- df[[rc]]
    m  <- mean(x, na.rm = TRUE)
    s  <- sd(x,   na.rm = TRUE)
    df[[nc]] <- if (s > 0) (x - m) / s else rep(0, length(x))
  }
  df
}

norm_pctrank <- function(df, raw_cols) {
  for (i in seq_along(raw_cols)) {
    rc <- raw_cols[i]
    nc <- NORM_COLS[i]
    x  <- df[[rc]]
    df[[nc]] <- rank(x, na.last = "keep", ties.method = "average") / sum(!is.na(x))
  }
  df
}

# ── Helper: aggregate normalized indicators to CELL-level CGI (~300 rows) ─────
# Returns province × category with CGI — NOT rolled up to province.
# n=300 gives a statistically meaningful base for Spearman ρ.
aggregate_cgi <- function(df, w_dp = 1/3, w_sp = 1/3, w_as = 1/3,
                           geom = FALSE) {
  df %>%
    mutate(
      DP = (DP1 + DP2 + DP3) / 3,
      SP = (SP1 + SP2 + SP3) / 3,
      AS = if_else(has_access_data, (AS1 + AS2 + AS3) / 3, NA_real_),
      n_pil = as.integer(!is.na(DP)) + as.integer(!is.na(SP)) + as.integer(!is.na(AS)),
      CGI = case_when(
        geom & n_pil == 3 ~ (pmax(DP, 0) * pmax(SP, 0) * pmax(AS, 0))^(1/3),
        !geom & n_pil == 3 ~ w_dp * DP + w_sp * SP + w_as * AS,
        n_pil == 2 & !is.na(DP) & !is.na(SP) ~ (DP + SP) / 2,
        TRUE ~ NA_real_
      )
    ) %>%
    select(province, category, CGI)   # cell-level output: ~300 rows
}

# ── Helper: roll up cell-level CGI to province (for rank-based stats only) ────
rollup_to_province <- function(cell_df) {
  cell_df %>%
    left_join(dbc_weights, by = c("province", "category")) %>%
    group_by(province) %>%
    summarise(CGI_prov = sum(w_s * CGI, na.rm = TRUE), .groups = "drop") %>%
    mutate(rank_cgi = rank(-CGI_prov))
}

# Central reference: cell-level for ρ; province-level for rank stats
central_cells <- aggregate_cgi(norm_minmax(ind_2025, RAW_COLS, norm_ref))
central_prov  <- rollup_to_province(central_cells)

# ── Helper: compute sensitivity stats ─────────────────────────────────────────
# Spearman ρ is computed at province × specialism cell level (~300 rows).
# n_top4_changed and max_rank_shift remain province-level (12 rows).
sens_stats <- function(label, alt_cells) {
  # ρ at cell level: more statistical power, less prone to ties
  joined_cells <- central_cells %>%
    left_join(alt_cells, by = c("province", "category"), suffix = c("_c", "_a"))
  rho <- cor(joined_cells$CGI_c, joined_cells$CGI_a,
             method = "spearman", use = "complete.obs")

  # Province rank stats via rollup
  alt_prov    <- rollup_to_province(alt_cells)
  joined_prov <- central_prov %>%
    left_join(alt_prov, by = "province", suffix = c("_c", "_a"))
  top4_c     <- joined_prov$province[joined_prov$rank_cgi_c <= 4]
  top4_a     <- joined_prov$province[joined_prov$rank_cgi_a <= 4]
  n_top4_chg <- length(setdiff(top4_c, top4_a))
  max_shift  <- max(abs(joined_prov$rank_cgi_c - joined_prov$rank_cgi_a), na.rm = TRUE)

  tibble(sensitivity_test = label,
         spearman_rho      = round(rho, 4),
         n_top4_changed    = n_top4_chg,
         max_rank_shift    = max_shift)
}

results <- list()

# ── 5.1 Normalization alternatives ────────────────────────────────────────────
message("5.1 Normalization alternatives...")

results[["norm_zscore"]] <- sens_stats(
  "5.1b Normalization: z-score",
  aggregate_cgi(norm_zscore(ind_2025, RAW_COLS))
)
results[["norm_pctrank"]] <- sens_stats(
  "5.1c Normalization: percentile rank",
  aggregate_cgi(norm_pctrank(ind_2025, RAW_COLS))
)

# ── 5.2 Weighting alternatives ────────────────────────────────────────────────
message("5.2 Weighting alternatives...")

weight_scenarios <- list(
  `5.2b Demand-heavy (0.5,0.25,0.25)`  = c(0.50, 0.25, 0.25),
  `5.2c Supply-heavy (0.25,0.5,0.25)`  = c(0.25, 0.50, 0.25),
  `5.2d Access-heavy (0.25,0.25,0.5)`  = c(0.25, 0.25, 0.50)
)

for (nm in names(weight_scenarios)) {
  w <- weight_scenarios[[nm]]
  base_norm <- norm_minmax(ind_2025, RAW_COLS, norm_ref)
  results[[nm]] <- sens_stats(nm, aggregate_cgi(base_norm, w[1], w[2], w[3]))
}

# 5.2e PCA-derived weights
message("  PCA weight derivation...")
ind_matrix_full <- ind_2025 %>%
  select(all_of(RAW_COLS)) %>%
  mutate(across(everything(), ~ if_else(is.na(.), median(., na.rm = TRUE), .)))

# Remove degenerate (zero-variance) columns before PCA (DP2_raw/DP3_raw = 0 at 2025)
non_degenerate <- sapply(ind_matrix_full, function(x) sd(x, na.rm = TRUE) > 0)
ind_matrix <- ind_matrix_full[, non_degenerate, drop = FALSE]
degenerate_cols <- names(non_degenerate)[!non_degenerate]
if (length(degenerate_cols) > 0)
  message("  Removing degenerate columns from PCA: ", paste(degenerate_cols, collapse = ", "))

pca <- tryCatch({
  prcomp(ind_matrix, center = TRUE, scale. = TRUE)
}, error = function(e) { message("  PCA failed: ", e$message); NULL })

if (!is.null(pca)) {
  loadings_sq  <- pca$rotation^2
  var_share    <- summary(pca)$importance[2, ]
  pca_raw_w    <- as.numeric(loadings_sq %*% var_share)
  # Map back to all 9 raw columns (degenerate = 0 weight)
  pca_weights_full <- setNames(rep(0, length(RAW_COLS)), RAW_COLS)
  pca_weights_full[colnames(ind_matrix)] <- pca_raw_w / sum(pca_raw_w)

  pca_weights  <- pca_weights_full

  # Group by pillar (first 3 = DP, next 3 = SP, last 3 = AS)
  w_dp_pca <- sum(pca_weights[1:3])
  w_sp_pca <- sum(pca_weights[4:6])
  w_as_pca <- sum(pca_weights[7:9])
  total_pca <- w_dp_pca + w_sp_pca + w_as_pca
  w_dp_pca <- w_dp_pca / total_pca
  w_sp_pca <- w_sp_pca / total_pca
  w_as_pca <- w_as_pca / total_pca

  message("  PCA pillar weights: DP=", round(w_dp_pca, 3),
          " SP=", round(w_sp_pca, 3), " AS=", round(w_as_pca, 3))

  base_norm <- norm_minmax(ind_2025, RAW_COLS, norm_ref)
  results[["pca_weights"]] <- sens_stats(
    "5.2e Weighting: PCA-derived",
    aggregate_cgi(base_norm, w_dp_pca, w_sp_pca, w_as_pca)
  )
} else {
  results[["pca_weights"]] <- tibble(
    sensitivity_test = "5.2e Weighting: PCA-derived (failed)",
    spearman_rho = NA_real_, n_top4_changed = NA_integer_, max_rank_shift = NA_integer_
  )
}

# ── 5.3 Aggregation alternatives ──────────────────────────────────────────────
message("5.3 Aggregation alternatives (geometric mean)...")

base_norm <- norm_minmax(ind_2025, RAW_COLS, norm_ref)
results[["geom_mean"]] <- sens_stats(
  "5.3b Aggregation: geometric mean",
  aggregate_cgi(base_norm, geom = TRUE)
)

# ── 5.4 Leave-one-out ─────────────────────────────────────────────────────────
message("5.4 Leave-one-out indicators...")

pillar_map <- c(DP1 = "DP", DP2 = "DP", DP3 = "DP",
                SP1 = "SP", SP2 = "SP", SP3 = "SP",
                AS1 = "AS", AS2 = "AS", AS3 = "AS")

loo_aggregate <- function(df, drop_col) {
  pillar   <- pillar_map[[drop_col]]
  rem_cols <- setdiff(names(pillar_map)[pillar_map == pillar], drop_col)

  df <- df %>%
    mutate(
      DP = case_when(
        pillar == "DP" ~ rowMeans(select(., all_of(setdiff(c("DP1","DP2","DP3"), drop_col))),
                                  na.rm = TRUE),
        TRUE ~ (DP1 + DP2 + DP3) / 3
      ),
      SP = case_when(
        pillar == "SP" ~ rowMeans(select(., all_of(setdiff(c("SP1","SP2","SP3"), drop_col))),
                                  na.rm = TRUE),
        TRUE ~ (SP1 + SP2 + SP3) / 3
      ),
      AS = case_when(
        pillar == "AS" ~ if_else(
          has_access_data,
          rowMeans(select(., all_of(setdiff(c("AS1","AS2","AS3"), drop_col))),
                   na.rm = TRUE),
          NA_real_
        ),
        TRUE ~ if_else(has_access_data, (AS1 + AS2 + AS3) / 3, NA_real_)
      ),
      n_pil = as.integer(!is.na(DP)) + as.integer(!is.na(SP)) + as.integer(!is.na(AS)),
      CGI = case_when(
        n_pil == 3 ~ (DP + SP + AS) / 3,
        n_pil == 2 & !is.na(DP) & !is.na(SP) ~ (DP + SP) / 2,
        TRUE ~ NA_real_
      )
    ) %>%
    select(province, category, CGI)   # cell-level output for sens_stats()
  df
}

base_norm <- norm_minmax(ind_2025, RAW_COLS, norm_ref)

for (nc in NORM_COLS) {
  results[[paste0("loo_", nc)]] <- sens_stats(
    paste0("5.4 LOO: drop ", nc),
    loo_aggregate(base_norm, nc)
  )
}

# ── 5.5 M-matrix ±10pp perturbation ──────────────────────────────────────────
message("5.5 M-matrix perturbation (±10pp)...")

for (direction in c("plus", "minus")) {
  shift <- if (direction == "plus") 0.10 else -0.10

  M_perturbed <- M_long %>%
    group_by(category) %>%
    mutate(
      # Shift zkh by ±10pp, adjust umc to maintain sum=1; ggz unchanged
      share_adj = case_when(
        sector == "zkh" ~ pmin(pmax(share + shift, 0), 1),
        sector == "umc" ~ pmin(pmax(share - shift, 0), 1),
        TRUE ~ share
      )
    ) %>%
    # Renormalize to ensure sum=1 per category
    mutate(share_adj = share_adj / sum(share_adj)) %>%
    ungroup() %>%
    mutate(share = share_adj) %>%
    select(-share_adj)

  # Recompute FTE_relevant with perturbed M (workforce_dbc loaded at top of script)
  fte_perturbed <- workforce_dbc %>%
    inner_join(M_perturbed, by = "sector", relationship = "many-to-many") %>%
    group_by(province, category) %>%
    summarise(
      fte_perturbed = sum(share * werkende, na.rm = TRUE),
      tekort_perturbed = sum(share * tekort, na.rm = TRUE),
      .groups = "drop"
    )

  # Recompute SP1 with perturbed FTE
  sp_perturbed <- fte_perturbed %>%
    mutate(
      SP1_raw_p = if_else(fte_perturbed > 0, tekort_perturbed / fte_perturbed, NA_real_)
    )

  ind_perturbed <- ind_2025 %>%
    left_join(sp_perturbed %>% select(province, category, SP1_raw_p),
              by = c("province", "category")) %>%
    mutate(SP1_raw = coalesce(SP1_raw_p, SP1_raw)) %>%
    select(-SP1_raw_p)

  nm <- paste0("5.5 M-matrix: zkh ", if (direction == "plus") "+10pp" else "-10pp")
  base_norm <- norm_minmax(ind_perturbed, RAW_COLS, norm_ref)
  results[[paste0("M_", direction)]] <- sens_stats(nm, aggregate_cgi(base_norm))
}

# ── 5.6 Productivity φ stress test ───────────────────────────────────────────
message("5.6 φ stress test (×0.85, ×1.15)...")

phi_data <- readRDS(file.path(PROCESSED_DIR, "phi.rds"))

for (scale in c(0.85, 1.15)) {
  phi_scaled <- phi_data %>% mutate(phi = phi * scale)

  sp3_scaled <- supply %>%
    filter(year == YEAR_BASE) %>%
    left_join(phi_scaled %>% select(province, category, phi_s = phi),
              by = c("province", "category")) %>%
    mutate(
      SP3_raw_s = if_else(expected_dbc > 0,
                           (phi_s * tekort_relevant) / expected_dbc,
                           NA_real_)
    ) %>%
    select(province, category, SP3_raw_s)

  ind_scaled <- ind_2025 %>%
    left_join(sp3_scaled, by = c("province", "category")) %>%
    mutate(SP3_raw = coalesce(SP3_raw_s, SP3_raw)) %>%
    select(-SP3_raw_s)

  nm <- paste0("5.6 φ stress: ×", scale)
  base_norm <- norm_minmax(ind_scaled, RAW_COLS, norm_ref)
  results[[paste0("phi_", scale)]] <- sens_stats(nm, aggregate_cgi(base_norm))
}

# ── Compile summary ────────────────────────────────────────────────────────────
message("Compiling sensitivity summary table...")

sensitivity_summary <- bind_rows(results) %>%
  arrange(spearman_rho)

message("\n  Sensitivity summary (sorted by Spearman ρ, ascending = most impact):")
message(capture.output(
  print(as.data.frame(sensitivity_summary), row.names = FALSE)) %>%
  paste(collapse = "\n"))

# ── Save ───────────────────────────────────────────────────────────────────────
saveRDS(sensitivity_summary, file.path(PROCESSED_DIR, "sensitivity_summary.rds"))
write.csv(sensitivity_summary,
          file.path(PROCESSED_DIR, "sensitivity_summary.csv"),
          row.names = FALSE)

message("\n=== 10_sensitivity.R complete ===")
message("  sensitivity_summary.rds: ", nrow(sensitivity_summary), " tests")
message("  Min Spearman ρ: ", round(min(sensitivity_summary$spearman_rho, na.rm = TRUE), 4))
message("  Max rank shift: ", max(sensitivity_summary$max_rank_shift, na.rm = TRUE))
