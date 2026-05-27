# =============================================================================
# FULL PIPELINE WALKTHROUGH — Limburg × Orthopedics
# Traces every computation step from raw inputs to final CGI score
# =============================================================================

library(tidyverse)

PROCESSED <- "C:/Users/Samsung/OneDrive/Desktop/ProjectMay/ResearchBacked_SS_proj/cgi_project/data/processed"

PROV  <- "Limburg"
SPEC  <- "Orthopedics"
YEARS <- c(2025, 2030, 2035)

# ── Load all processed inputs ─────────────────────────────────────────────────
pop_summary     <- readRDS(file.path(PROCESSED, "pop_summary.rds"))
dbc_shares      <- readRDS(file.path(PROCESSED, "dbc_shares.rds"))
workforce       <- readRDS(file.path(PROCESSED, "workforce.rds"))
access          <- readRDS(file.path(PROCESSED, "access.rds"))
phi_tbl         <- readRDS(file.path(PROCESSED, "phi.rds"))
indicators      <- readRDS(file.path(PROCESSED, "indicators.rds"))
indicators_norm <- readRDS(file.path(PROCESSED, "indicators_norm.rds"))
norm_ref        <- readRDS(file.path(PROCESSED, "norm_reference.rds"))
cgi             <- readRDS(file.path(PROCESSED, "cgi.rds"))

sep <- function() cat(paste0(rep("─", 68), collapse = ""), "\n")

# =============================================================================
cat("\n")
sep(); cat(" STEP 0 — RAW INPUTS for", PROV, "\n"); sep()

pop_lim <- pop_summary %>% filter(province == PROV, year %in% YEARS) %>%
  select(year, pop_total, pop_65plus, share_65plus, contact_potential)
cat("\nDemographics (CBS forecast):\n")
print(as.data.frame(pop_lim), row.names = FALSE)

mu_s <- dbc_shares %>% filter(category == SPEC) %>% pull(mu_s)
n_s  <- dbc_shares %>% filter(category == SPEC) %>% pull(n_subtraject_base)
cat(sprintf("\nNZa DBC share for %s:  mu_s = %.6f  (based on %s national DBCs)\n",
            SPEC, mu_s, format(n_s, big.mark = ",")))

wf_lim <- workforce %>%
  filter(province == PROV, sector %in% c("zkh","umc"), year %in% YEARS) %>%
  select(year, sector, werkende, tekort, share_55plus)
cat("\nDAAN workforce — zkh + umc sectors:\n")
print(as.data.frame(wf_lim), row.names = FALSE)

# =============================================================================
sep(); cat(" STEP 1 — DEMAND BACKBONE  (Eq. 1)\n"); sep()
cat("\n ExpectedDBC_{p,s,t} = contact_potential_{p,t} × mu_s\n\n")

demand_lim <- pop_lim %>%
  mutate(
    specialism    = SPEC,
    expected_dbc  = contact_potential * mu_s
  ) %>%
  select(year, contact_potential, mu_s = contact_potential, expected_dbc)

# recompute cleanly
demand_lim <- pop_lim %>%
  mutate(expected_dbc = contact_potential * mu_s) %>%
  select(year, contact_potential, expected_dbc)

for (i in seq_len(nrow(demand_lim))) {
  r <- demand_lim[i, ]
  cat(sprintf("  %d:  %s × %.6f = %s expected DBCs\n",
              r$year,
              format(round(r$contact_potential), big.mark = ","),
              mu_s,
              format(round(r$expected_dbc), big.mark = ",")))
}

# =============================================================================
sep(); cat(" STEP 2 — DEMAND PRESSURE INDICATORS  (DP1, DP2, DP3)\n"); sep()

ind_lim <- indicators %>%
  filter(province == PROV, category == SPEC, year %in% YEARS)

cat("\nDP1  =  ExpectedDBC / pop_total  (per-capita need intensity)\n\n")
for (i in seq_len(nrow(ind_lim))) {
  r <- ind_lim[i, ]
  cat(sprintf("  %d:  %s / %s = %.6f\n",
              r$year,
              format(round(r$expected_dbc), big.mark = ","),
              format(round(r$pop_total),    big.mark = ","),
              r$DP1_raw))
}

eps <- ind_lim$epsilon[1]
cat(sprintf("\nDP2  =  Δshare_65plus (2025→t) × ε_s      [ε_%s = %.2f]\n\n", SPEC, eps))
s2025 <- pop_lim %>% filter(year == 2025) %>% pull(share_65plus)
for (i in seq_len(nrow(ind_lim))) {
  r    <- ind_lim[i, ]
  ds   <- r$share_65plus - s2025
  cat(sprintf("  %d:  (%.4f − %.4f) × %.2f = %.6f\n",
              r$year, r$share_65plus, s2025, eps, r$DP2_raw))
}

cat("\nDP3  =  (ExpectedDBC_t − ExpectedDBC_2025) / ExpectedDBC_2025  (demand growth)\n\n")
dbc_2025 <- ind_lim %>% filter(year == 2025) %>% pull(expected_dbc)
for (i in seq_len(nrow(ind_lim))) {
  r <- ind_lim[i, ]
  cat(sprintf("  %d:  (%s − %s) / %s = %.6f\n",
              r$year,
              format(round(r$expected_dbc), big.mark = ","),
              format(round(dbc_2025),       big.mark = ","),
              format(round(dbc_2025),       big.mark = ","),
              r$DP3_raw))
}

# =============================================================================
sep(); cat(" STEP 3 — PRODUCTIVITY BRIDGE  φ  (Eq. 2)\n"); sep()

phi_row <- phi_tbl %>% filter(province == PROV, category == SPEC)
cat(sprintf("\n φ_{%s, %s} = ExpectedDBC_2025 / FTE_relevant_2025\n", PROV, SPEC))
cat(sprintf("            = %s / %.1f  =  %.4f  DBCs per FTE\n",
            format(round(dbc_2025), big.mark = ","),
            phi_row$fte_base,
            phi_row$phi))
cat(sprintf(" Source: %s\n", phi_row$phi_source))
cat(sprintf(" FTE_relevant  =  Σ_k  M_{%s,k} × werkende_{%s,k,2025}\n", SPEC, PROV))
cat(sprintf("               =  0.85 × zkh_FTE  +  0.15 × umc_FTE  =  %.1f\n",
            phi_row$fte_base))

# =============================================================================
sep(); cat(" STEP 4 — SUPPLY PRESSURE INDICATORS  (SP1, SP2, SP3)\n"); sep()

cat("\nSP1  =  tekort_relevant / werkende_relevant  (shortage rate)\n\n")
for (i in seq_len(nrow(ind_lim))) {
  r <- ind_lim[i, ]
  cat(sprintf("  %d:  tekort_relevant=%.1f  /  werkende_relevant=%.1f  =  %.6f\n",
              r$year, r$tekort_relevant, r$fte_relevant, r$SP1_raw))
}

cat("\nSP2  =  Σ_k M_prov[k] × werkende_k × share_55plus_k  /  Σ_k M_prov[k] × werkende_k\n")
cat("       (M-weighted share of workforce aged 55+ → retirement pressure)\n\n")
for (i in seq_len(nrow(ind_lim))) {
  r <- ind_lim[i, ]
  cat(sprintf("  %d:  SP2_raw = %.4f  (%.1f%% of relevant workforce aged 55+)\n",
              r$year, r$SP2_raw, r$SP2_raw * 100))
}

cat(sprintf("\nSP3  =  φ × tekort_relevant / ExpectedDBC\n"))
cat(sprintf("       (fraction of expected care volume that cannot be delivered)\n"))
cat(sprintf("       φ = %.4f\n\n", phi_row$phi))
for (i in seq_len(nrow(ind_lim))) {
  r <- ind_lim[i, ]
  cat(sprintf("  %d:  %.4f × %.1f / %s = %.6f\n",
              r$year, phi_row$phi, r$tekort_relevant,
              format(round(r$expected_dbc), big.mark = ","),
              r$SP3_raw))
}

# =============================================================================
sep(); cat(" STEP 5 — ACCESS STRESS INDICATORS  (AS1, AS2, AS3)\n"); sep()

acc_lim <- access %>% filter(province == PROV, category == SPEC)
cat(sprintf("\n AS indicators are 2025 snapshot only (no temporal NZa data)\n\n"))
cat(sprintf("  AS1_raw  (Treeknorm breach rate)    = %.4f  (%.1f%% of providers exceed threshold)\n",
            acc_lim$AS1_raw, acc_lim$AS1_raw * 100))
cat(sprintf("  AS2_raw  (mean wait z-score)        = %.4f  (%.1f days mean; z vs national mean)\n",
            acc_lim$AS2_raw, acc_lim$mean_wait_days))
cat(sprintf("  AS3_raw  (inverted provider density)= %.4f  (%.2f providers per 100k pop)\n",
            acc_lim$AS3_raw, acc_lim$density_per_100k))
cat(sprintf("  n_providers observed                = %d\n", acc_lim$n_providers))
cat(sprintf("\n  AS3 is inverted: higher density = better access → we negate so high = bad\n"))

# =============================================================================
sep(); cat(" STEP 6 — MIN-MAX NORMALIZATION  (2025 anchors, no clipping)\n"); sep()

cat("\n Formula:  I_norm = (I_raw − min_2025) / (max_2025 − min_2025)\n")
cat(" Anchors are frozen at the 2025 cross-section. Values > 1 are valid.\n\n")
cat(" 2025 normalization reference values:\n")
print(as.data.frame(norm_ref %>% mutate(across(where(is.numeric), ~round(., 4)))),
      row.names = FALSE)

cat("\n Normalized values for", PROV, "×", SPEC, ":\n\n")
norm_lim <- indicators_norm %>%
  filter(province == PROV, category == SPEC, year %in% YEARS) %>%
  select(year, DP1, DP2, DP3, SP1, SP2, SP3, AS1, AS2, AS3)

print(as.data.frame(norm_lim %>% mutate(across(where(is.numeric), ~round(., 4)))),
      row.names = FALSE)

# =============================================================================
sep(); cat(" STEP 7 — PILLAR AGGREGATION  (equal weights)\n"); sep()

cat("\n DP = (DP1 + DP2 + DP3) / 3\n")
cat(" SP = (SP1 + SP2 + SP3) / 3\n")
cat(" AS = (AS1 + AS2 + AS3) / 3  [static 2025 snapshot]\n\n")

cgi_lim <- cgi %>%
  filter(province == PROV, category == SPEC, year %in% YEARS) %>%
  select(year, DP1, DP2, DP3, DP, SP1, SP2, SP3, SP, AS1, AS2, AS3, AS)

for (i in seq_len(nrow(cgi_lim))) {
  r <- cgi_lim[i, ]
  cat(sprintf(" %d:\n", r$year))
  cat(sprintf("   DP = (%.4f + %.4f + %.4f) / 3 = %.4f\n", r$DP1, r$DP2, r$DP3, r$DP))
  cat(sprintf("   SP = (%.4f + %.4f + %.4f) / 3 = %.4f\n", r$SP1, r$SP2, r$SP3, r$SP))
  if (!is.na(r$AS1)) {
    cat(sprintf("   AS = (%.4f + %.4f + %.4f) / 3 = %.4f  [held at 2025]\n", r$AS1, r$AS2, r$AS3, r$AS))
  } else {
    cat(sprintf("   AS = NA  (no wachttijden data)\n"))
  }
  cat("\n")
}

# =============================================================================
sep(); cat(" STEP 8 — FINAL CGI SCORE\n"); sep()

cat("\n CGI = (DP + SP + AS) / 3   [3-pillar average]\n\n")

cgi_final <- cgi %>%
  filter(province == PROV, category == SPEC, year %in% YEARS) %>%
  select(year, DP, SP, AS, CGI, n_pillars_cgi, incomplete)

for (i in seq_len(nrow(cgi_final))) {
  r <- cgi_final[i, ]
  if (!is.na(r$AS)) {
    cat(sprintf(" %d:  CGI = (%.4f + %.4f + %.4f) / 3 = %.4f\n",
                r$year, r$DP, r$SP, r$AS, r$CGI))
  } else {
    cat(sprintf(" %d:  CGI = (%.4f + %.4f + NA) / 2    = %.4f  [2-pillar fallback]\n",
                r$year, r$DP, r$SP, r$CGI))
  }
}

cat("\n")
sep(); cat(" SUMMARY TABLE — Limburg × Orthopedics\n"); sep()
cat("\n")
print(as.data.frame(cgi_final %>% mutate(across(where(is.numeric), ~round(., 4)))),
      row.names = FALSE)
cat("\n")
sep()
cat(sprintf(" INTERPRETATION\n"))
sep()
cat(sprintf("\n 2025  CGI = %.2f  — Moderate pressure, AS already elevated (waiting times)\n",
            cgi_final$CGI[1]))
cat(sprintf(" 2030  CGI = %.2f  — Demand growth + rising shortage compound\n",
            cgi_final$CGI[2]))
cat(sprintf(" 2035  CGI = %.2f  — SP exceeds 1 (worse than 2025 worst case); CGI > 0.66\n",
            cgi_final$CGI[3]))
hotspot <- cgi_final$CGI[3] > 0.66
cat(sprintf("\n Hotspot flag (CGI_2035 > 0.66): %s\n\n", if (hotspot) "YES" else "NO"))
