# Indicator: Demand growth rate
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
pop_summary <- readRDS(file.path(PROCESSED, "pop_summary.rds"))  # province × year
dbc_shares  <- readRDS(file.path(PROCESSED, "dbc_shares.rds"))   # category, mu_s

# ── Step 1: Contact potential at 2025 and 2035 ────────────────────────────────
# contact_potential = Σ_a Pop_{p,a,t} × ψ_a  (Eq. 1 bracket term)
cp_wide <- pop_summary %>%
  filter(year %in% c(2025, 2035)) %>%
  select(province, year, contact_potential) %>%
  pivot_wider(names_from = year, values_from = contact_potential,
              names_prefix = "cp")  # → cp2025, cp2035

# ── Step 2: Demand growth = (DBC_2035 - DBC_2025) / DBC_2025 ─────────────────
# Since ExpectedDBC_{p,s,t} = cp_t × μ_s, μ_s cancels in the ratio.
# Growth is province-specific; specialism-specific variation enters via DP1 & DP2.
growth_prov <- cp_wide %>%
  mutate(DP3_raw = (cp2035 - cp2025) / cp2025) %>%  # fractional demand growth
  select(province, DP3_raw)

# ── Step 3: Expand to province × specialism (one row per specialism per province)
dp3_raw <- growth_prov %>%
  cross_join(dbc_shares %>% select(specialism = category)) %>%  # replicate per specialism
  select(province, specialism, DP3_raw)

# ── Step 4: Min-max normalisation anchored to 2025 cross-sectional distribution
min_2025 <- min(dp3_raw$DP3_raw, na.rm = TRUE)   # 2025 reference floor
max_2025 <- max(dp3_raw$DP3_raw, na.rm = TRUE)   # 2025 reference ceiling

dp3_norm <- dp3_raw %>%
  mutate(DP3_norm = if (max_2025 == min_2025) 0.5
                    else (DP3_raw - min_2025) / (max_2025 - min_2025)) %>%
  select(province, specialism, DP3_norm)

# ── Step 5: Save and verify ────────────────────────────────────────────────────
write_csv(dp3_norm, file.path(OUT_DIR, "DP3_norm.csv"))

cat("min_2025 =", round(min_2025, 6), "| max_2025 =", round(max_2025, 6), "\n")
cat("DP3_norm range:", paste(round(range(dp3_norm$DP3_norm, na.rm = TRUE), 4), collapse = " – "), "\n")
print(head(dp3_norm, 5))
message("03_DP3.R complete — ", nrow(dp3_norm), " rows → DP3_norm.csv")
