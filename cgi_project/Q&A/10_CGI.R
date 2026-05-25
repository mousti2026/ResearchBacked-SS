# Indicator: Care Gap Index — composite aggregation
# Pillar: All (DP + SP + AS)
# Output: CGI_final.csv with all 9 normalised sub-indicators and composite scores

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
OUT_DIR   <- file.path(.here, "output")
PROCESSED <- normalizePath(file.path(.here, "..", "data", "processed"), winslash = "/")
dir.create(OUT_DIR, showWarnings = FALSE, recursive = TRUE)

# ── Specialisms without NZa waiting time data → AS pillar = NA ────────────────
# These specialisms are managed outside the MSZ waiting time system.
NO_AS_SPECS <- c("Oncology & Radiotherapy", "Psychiatry & Mental Health")

# ── Load all nine normalised sub-indicator CSVs ────────────────────────────────
read_ind <- function(fname) read_csv(file.path(OUT_DIR, fname),  # helper to load one CSV
                                     show_col_types = FALSE)

dp1 <- read_ind("DP1_norm.csv")
dp2 <- read_ind("DP2_norm.csv")
dp3 <- read_ind("DP3_norm.csv")
sp1 <- read_ind("SP1_norm.csv")
sp2 <- read_ind("SP2_norm.csv")
sp3 <- read_ind("SP3_norm.csv")
as1 <- read_ind("AS1_norm.csv")
as2 <- read_ind("AS2_norm.csv")
as3 <- read_ind("AS3_norm.csv")

# ── Join all indicators on province × specialism ───────────────────────────────
# Use full_join so cells present in demand but absent from AS are retained
cgi_wide <- dp1 %>%
  full_join(dp2, by = c("province", "specialism")) %>%
  full_join(dp3, by = c("province", "specialism")) %>%
  full_join(sp1, by = c("province", "specialism")) %>%
  full_join(sp2, by = c("province", "specialism"))

cgi_wide <- cgi_wide %>%
  full_join(sp3, by = c("province", "specialism")) %>%
  left_join(as1, by = c("province", "specialism")) %>%  # left_join: AS only where available
  left_join(as2, by = c("province", "specialism")) %>%
  left_join(as3, by = c("province", "specialism"))

# ── Compute pillar scores ──────────────────────────────────────────────────────
cgi_wide <- cgi_wide %>%
  mutate(
    DP = (DP1_norm + DP2_norm + DP3_norm) / 3,          # Demand Pressure pillar
    SP = (SP1_norm + SP2_norm + SP3_norm) / 3,          # Supply Pressure pillar
    AS = (AS1_norm + AS2_norm + AS3_norm) / 3           # Access Stress pillar
  )

# ── Compute CGI, handling missing AS for 2 specialisms ────────────────────────
# Specialisms without waiting time data: CGI = (DP + SP) / 2, flag incomplete_AS
cgi_final <- cgi_wide %>%
  mutate(
    incomplete_AS = specialism %in% NO_AS_SPECS | is.na(AS),   # flag incomplete pillar
    CGI = if_else(incomplete_AS,
                  (DP + SP) / 2,                               # two-pillar average
                  (DP + SP + AS) / 3)                          # three-pillar average
  ) %>%
  select(province, specialism,
         DP1_norm, DP2_norm, DP3_norm,
         SP1_norm, SP2_norm, SP3_norm,
         AS1_norm, AS2_norm, AS3_norm,
         DP, SP, AS, CGI, incomplete_AS)

# ── Save cell-level output ─────────────────────────────────────────────────────
write_csv(cgi_final, file.path(OUT_DIR, "CGI_final.csv"))

# ── Province rollup: DBC-volume-weighted (mirrors 08_aggregate.R) ─────────────
# CGI_{p} = Σ_s [ (DBC_{p,s,2025} / Σ_s DBC_{p,s,2025}) × CGI_{p,s} ]
pop_summary <- readRDS(file.path(PROCESSED, "pop_summary.rds"))
dbc_shares  <- readRDS(file.path(PROCESSED, "dbc_shares.rds"))

expected_dbc_2025 <- pop_summary %>%
  filter(year == 2025) %>%
  select(province, contact_potential) %>%
  cross_join(dbc_shares %>% select(specialism = category, mu_s)) %>%
  mutate(expected_dbc = contact_potential * mu_s) %>%
  select(province, specialism, expected_dbc)

dbc_weights <- expected_dbc_2025 %>%
  group_by(province) %>%
  mutate(w_s = expected_dbc / sum(expected_dbc, na.rm = TRUE)) %>%
  ungroup() %>%
  select(province, specialism, w_s)

province_rollup <- cgi_final %>%
  left_join(dbc_weights, by = c("province", "specialism")) %>%
  group_by(province) %>%
  summarise(
    CGI = sum(w_s * CGI, na.rm = TRUE),
    DP  = sum(w_s * DP,  na.rm = TRUE),
    SP  = sum(w_s * SP,  na.rm = TRUE),
    AS  = if_else(all(is.na(AS)), NA_real_,
                  sum(w_s * AS, na.rm = TRUE) / sum(w_s[!is.na(AS)], na.rm = TRUE)),
    .groups = "drop"
  ) %>%
  arrange(desc(CGI))

write_csv(province_rollup, file.path(OUT_DIR, "CGI_province_rollup.csv"))

# ── Summary output ─────────────────────────────────────────────────────────────
n_complete   <- sum(!cgi_final$incomplete_AS, na.rm = TRUE)
n_incomplete <- sum( cgi_final$incomplete_AS, na.rm = TRUE)

cat("\n=== CGI Aggregation Summary ===\n")
cat("Complete rows   (DP + SP + AS) / 3 :", n_complete,   "\n")
cat("Incomplete rows (DP + SP) / 2      :", n_incomplete, "\n")
cat("Total rows                          :", nrow(cgi_final), "\n")
cat("CGI range: [",
    round(min(cgi_final$CGI, na.rm = TRUE), 3), ",",
    round(max(cgi_final$CGI, na.rm = TRUE), 3), "]\n\n")

cat("Top 10 province × specialism by CGI:\n")
cgi_final %>%
  arrange(desc(CGI)) %>%
  slice_head(n = 10) %>%
  select(province, specialism, DP, SP, AS, CGI, incomplete_AS) %>%
  mutate(across(where(is.numeric), \(x) round(x, 3))) %>%
  print(n = 10)

cat("\nProvince rollup (DBC-volume-weighted):\n")
province_rollup %>%
  mutate(across(where(is.numeric), \(x) round(x, 3))) %>%
  print(n = nrow(province_rollup))

message("\n10_CGI.R complete — CGI_final.csv + CGI_province_rollup.csv saved")
