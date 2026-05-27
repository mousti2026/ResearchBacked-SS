# =============================================================================
# DEMO: Why Supply Pressure (SP) can exceed 1
# Case study: Limburg × Orthopedics, 2025 → 2030 → 2035
#
# Key point: SP > 1 means supply pressure has deteriorated PAST the worst case
# observed anywhere in the Netherlands in 2025. It is not a bug — it is the
# alarm signal the index is designed to emit.
# =============================================================================

library(tidyverse)

PROCESSED <- "C:/Users/Samsung/OneDrive/Desktop/ProjectMay/ResearchBacked_SS_proj/cgi_project/data/processed"

PROVINCE  <- "Limburg"
SPEC      <- "Orthopedics"
YEARS     <- c(2025, 2030, 2035)

# ── Load pipeline outputs ──────────────────────────────────────────────────────
indicators      <- readRDS(file.path(PROCESSED, "indicators.rds"))
indicators_norm <- readRDS(file.path(PROCESSED, "indicators_norm.rds"))
norm_ref        <- readRDS(file.path(PROCESSED, "norm_reference.rds"))
cgi             <- readRDS(file.path(PROCESSED, "cgi.rds"))

# ── Step 1: Print the 2025 normalization anchors for SP ───────────────────────
cat("\n")
cat("================================================================\n")
cat(" STEP 1 — The 2025 normalization anchors (fixed reference)\n")
cat("================================================================\n")
cat(" These are computed ONCE from all provinces × specialisms in 2025.\n")
cat(" Every year — past or future — uses these same anchors.\n\n")

sp_ref <- norm_ref %>%
  filter(indicator %in% c("SP1_raw", "SP2_raw", "SP3_raw")) %>%
  mutate(across(c(min_ref, max_ref, range_ref), ~ round(., 4)))

print(as.data.frame(sp_ref), row.names = FALSE)

# ── Step 2: Raw SP values for Limburg × Orthopedics ──────────────────────────
cat("\n")
cat("================================================================\n")
cat(sprintf(" STEP 2 — Raw (unnormalised) SP values: %s × %s\n", PROVINCE, SPEC))
cat("================================================================\n")
cat(" These are the actual modelled values before any scaling.\n")
cat(" As DAAN's forecast tekort grows over time, raw values rise.\n\n")

raw <- indicators %>%
  filter(province == PROVINCE, specialism == SPEC, year %in% YEARS) %>%
  select(year, SP1_raw, SP2_raw, SP3_raw)

print(as.data.frame(raw), row.names = FALSE)

# ── Step 3: Apply the fixed 2025 min-max formula manually ────────────────────
cat("\n")
cat("================================================================\n")
cat(" STEP 3 — Normalization formula applied year by year\n")
cat("================================================================\n")
cat(" Formula:  SP_norm = (SP_raw - min_2025) / (max_2025 - min_2025)\n")
cat(" The anchors (min/max) do NOT change between years.\n\n")

ref <- norm_ref %>%
  filter(indicator %in% c("SP1_raw", "SP2_raw", "SP3_raw")) %>%
  select(indicator, min_ref, max_ref)

for (yr in YEARS) {
  row <- raw %>% filter(year == yr)

  sp1_r <- ref %>% filter(indicator == "SP1_raw")
  sp2_r <- ref %>% filter(indicator == "SP2_raw")
  sp3_r <- ref %>% filter(indicator == "SP3_raw")

  sp1_n <- (row$SP1_raw - sp1_r$min_ref) / (sp1_r$max_ref - sp1_r$min_ref)
  sp2_n <- (row$SP2_raw - sp2_r$min_ref) / (sp2_r$max_ref - sp2_r$min_ref)
  sp3_n <- (row$SP3_raw - sp3_r$min_ref) / (sp3_r$max_ref - sp3_r$min_ref)
  sp_p  <- (sp1_n + sp2_n + sp3_n) / 3

  flag_sp1 <- if (sp1_n > 1) "  *** EXCEEDS 1 ***" else ""
  flag_sp2 <- if (sp2_n > 1) "  *** EXCEEDS 1 ***" else ""
  flag_sp3 <- if (sp3_n > 1) "  *** EXCEEDS 1 ***" else ""
  flag_sp  <- if (sp_p  > 1) "  *** EXCEEDS 1 ***" else ""

  cat(sprintf(" Year %d:\n", yr))
  cat(sprintf("   SP1_norm = (%.4f - %.4f) / %.4f = %.4f%s\n",
              row$SP1_raw, sp1_r$min_ref, sp1_r$max_ref - sp1_r$min_ref, sp1_n, flag_sp1))
  cat(sprintf("   SP2_norm = (%.4f - %.4f) / %.4f = %.4f%s\n",
              row$SP2_raw, sp2_r$min_ref, sp2_r$max_ref - sp2_r$min_ref, sp2_n, flag_sp2))
  cat(sprintf("   SP3_norm = (%.4f - %.4f) / %.4f = %.4f%s\n",
              row$SP3_raw, sp3_r$min_ref, sp3_r$max_ref - sp3_r$min_ref, sp3_n, flag_sp3))
  cat(sprintf("   SP pillar = (%.4f + %.4f + %.4f) / 3 = %.4f%s\n\n",
              sp1_n, sp2_n, sp3_n, sp_p, flag_sp))
}

# ── Step 4: Show CGI panel for this cell ─────────────────────────────────────
cat("================================================================\n")
cat(sprintf(" STEP 4 — Full CGI panel: %s × %s\n", PROVINCE, SPEC))
cat("================================================================\n\n")

cell <- cgi %>%
  filter(province == PROVINCE, specialism == SPEC, year %in% YEARS) %>%
  select(year, DP, SP, AS, CGI, incomplete_AS) %>%
  mutate(across(where(is.numeric), ~ round(., 3)))

print(as.data.frame(cell), row.names = FALSE)

# ── Step 5: Context — where does Limburg rank among all provinces in 2035? ───
cat("\n")
cat("================================================================\n")
cat(sprintf(" STEP 5 — Context: SP in 2035 across all provinces for %s\n", SPEC))
cat("================================================================\n\n")

context <- indicators_norm %>%
  filter(specialism == SPEC, year == 2035) %>%
  select(province, SP1, SP2, SP3) %>%
  mutate(SP = round((SP1 + SP2 + SP3) / 3, 3),
         SP1 = round(SP1, 3), SP2 = round(SP2, 3), SP3 = round(SP3, 3)) %>%
  arrange(desc(SP))

print(as.data.frame(context), row.names = FALSE)

# ── Step 6: Plain-language explanation ───────────────────────────────────────
cat("\n")
cat("================================================================\n")
cat(" WHAT DOES SP > 1 MEAN?\n")
cat("================================================================\n")
cat("\n")
cat(" The normalization anchors are frozen at the 2025 cross-sectional\n")
cat(" distribution (all 12 provinces × 25 specialisms). A score of:\n")
cat("\n")
cat("   0.0  = best supply situation observed anywhere in NL in 2025\n")
cat("   1.0  = worst supply situation observed anywhere in NL in 2025\n")
cat("   >1.0 = WORSE than the worst case the model saw in 2025\n")
cat("\n")
cat(" SP > 1 is not a calculation error. It is the intended signal:\n")
cat(" conditions have deteriorated past the 2025 calibration ceiling.\n")
cat("\n")
cat(" If we clipped SP to [0, 1], a province at SP = 1.4 and one at\n")
cat(" SP = 1.0 would look identical on the index — masking the gap.\n")
cat("\n")
cat(" Design reference: UNDP HDI uses fixed 'goalposts' (min/max) set\n")
cat(" once so annual values remain temporally comparable. We apply the\n")
cat(" same principle with the 2025 cross-section as our goalpost.\n")
cat("================================================================\n\n")
