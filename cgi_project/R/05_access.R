# Script: 05_access.R — Purpose: NZa wachttijden → Access Stress raw indicators
#
# Computes three raw Access Stress indicators (2025 snapshot; held constant across years):
#
#   AS1 — Treeknorm breach rate  : share of observations in (p,s) where wait > threshold
#   AS2 — Mean waiting days      : mean wait days per (p,s), z-scored vs national mean
#   AS3 — Provider density (inv) : -[unique_providers / (Pop_{p,2025} / 100k)]
#                                   (negated so higher = more stress)
#
# Categories without wachttijden: AS1 = AS2 = AS3 = NA → CGI computed from DP + SP only.
#
# Output: access.rds — province × category with AS1_raw, AS2_raw, AS3_raw,
#                      n_providers, n_observations, has_access_data flag

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
wachttijden <- readRDS(file.path(PROCESSED_DIR, "wachttijden.rds"))
pop_summary  <- readRDS(file.path(PROCESSED_DIR, "pop_summary.rds"))

# ── Step 1: Full province × category grid ─────────────────────────────────────
message("Building full province × category grid...")

all_provinces <- sort(unique(pop_summary$province))
full_grid <- expand_grid(
  province = all_provinces,
  category = ALL_CATEGORIES
)

# ── Step 2: AS1 — Treeknorm breach rate ───────────────────────────────────────
# share of all wachttijden observations in (p,s) where wait > treeknorm threshold
message("Computing AS1 (Treeknorm breach rate)...")

as1_raw <- wachttijden %>%
  group_by(province, category) %>%
  summarise(
    n_obs             = n(),
    n_breach          = sum(exceeds_treeknorm, na.rm = TRUE),
    AS1_raw           = n_breach / n_obs,  # breach rate [0, 1]
    .groups = "drop"
  ) %>%
  select(province, category, AS1_raw, n_obs)

message("  AS1 computed for ", nrow(as1_raw), " province × category pairs")
message("  AS1 range: ", round(min(as1_raw$AS1_raw), 3),
        " – ", round(max(as1_raw$AS1_raw), 3),
        " | median: ", round(median(as1_raw$AS1_raw), 3))

# ── Step 3: AS2 — Mean waiting days (z-scored vs national mean) ───────────────
# Compute mean wait days per (province, category), then z-score against
# the specialism's national mean so that provinces with longer-than-average
# waits score high.
message("Computing AS2 (mean wait days, z-scored)...")

# Province-level mean
wait_mean_province <- wachttijden %>%
  group_by(province, category) %>%
  summarise(
    mean_wait_days = mean(wachttijd, na.rm = TRUE),
    .groups = "drop"
  )

# National mean and SD per category (used for z-scoring)
wait_national <- wait_mean_province %>%
  group_by(category) %>%
  summarise(
    national_mean_wait = mean(mean_wait_days,  na.rm = TRUE),
    national_sd_wait   = sd(mean_wait_days,    na.rm = TRUE),
    .groups = "drop"
  )

# Z-score: (province_mean - national_mean) / national_SD
# If national_sd = 0 (all provinces identical), set AS2 = 0
as2_raw <- wait_mean_province %>%
  left_join(wait_national, by = "category") %>%
  mutate(
    AS2_raw = if_else(
      !is.na(national_sd_wait) & national_sd_wait > 0,
      (mean_wait_days - national_mean_wait) / national_sd_wait,
      0
    )
  ) %>%
  select(province, category, mean_wait_days, AS2_raw)

message("  AS2 computed for ", nrow(as2_raw), " province × category pairs")
message("  AS2 (z-score) range: ",
        round(min(as2_raw$AS2_raw), 2), " – ",
        round(max(as2_raw$AS2_raw), 2))

# ── Step 4: AS3 — Provider density (inverted) ─────────────────────────────────
# Unique providers per (province, category) per 100k population
# Higher density = better access → INVERT (× -1) so higher = more stress
message("Computing AS3 (provider density, inverted)...")

# 2025 province population
pop_2025 <- pop_summary %>%
  filter(year == 2025) %>%
  select(province, pop_total)

# Count distinct providers per (province, category)
provider_counts <- wachttijden %>%
  group_by(province, category) %>%
  summarise(
    n_providers = n_distinct(naam_instelling),
    .groups = "drop"
  )

as3_raw <- provider_counts %>%
  left_join(pop_2025, by = "province") %>%
  mutate(
    density_per_100k = n_providers / (pop_total / 100000),  # providers per 100k
    AS3_raw          = -density_per_100k   # invert: lower density = higher stress
  ) %>%
  select(province, category, n_providers, density_per_100k, AS3_raw)

message("  AS3 computed for ", nrow(as3_raw), " province × category pairs")
message("  Provider density range: ",
        round(min(as3_raw$density_per_100k), 3), " – ",
        round(max(as3_raw$density_per_100k), 3), " providers per 100k")
message("  AS3_raw (negated) range: ",
        round(min(as3_raw$AS3_raw), 3), " – ",
        round(max(as3_raw$AS3_raw), 3))

# ── Step 5: Assemble full access table ────────────────────────────────────────
message("Assembling access indicator table...")

access <- full_grid %>%
  left_join(as1_raw   %>% select(province, category, AS1_raw, n_obs),
            by = c("province", "category")) %>%
  left_join(as2_raw   %>% select(province, category, mean_wait_days, AS2_raw),
            by = c("province", "category")) %>%
  left_join(as3_raw   %>% select(province, category, n_providers, density_per_100k, AS3_raw),
            by = c("province", "category")) %>%
  mutate(
    has_access_data = !is.na(AS1_raw),
    n_observations  = if_else(is.na(n_obs), 0L, as.integer(n_obs))
  ) %>%
  select(province, category,
         AS1_raw, AS2_raw, AS3_raw,
         mean_wait_days, density_per_100k,
         n_providers, n_observations, has_access_data)

# ── Step 6: Validate ───────────────────────────────────────────────────────────
stopifnot("access: 300 rows expected" = nrow(access) == 300)  # 12 × 25

n_with_data    <- sum(access$has_access_data)
n_without_data <- sum(!access$has_access_data)
message("\n  Rows with access data    : ", n_with_data)
message("  Rows without access data : ", n_without_data, " (AS = NA → 2-pillar CGI)")

cats_without <- access %>%
  filter(!has_access_data) %>%
  distinct(category) %>%
  pull()
message("  Categories with NO access data: ",
        paste(sort(cats_without), collapse = "; "))

# Sanity: categories with most provinces missing data
missing_by_cat <- access %>%
  group_by(category) %>%
  summarise(n_missing = sum(!has_access_data), .groups = "drop") %>%
  filter(n_missing > 0) %>%
  arrange(desc(n_missing))

if (nrow(missing_by_cat) > 0) {
  message("\n  Categories with some missing provinces:")
  message(capture.output(print(as.data.frame(missing_by_cat), row.names = FALSE)) %>%
            paste(collapse = "\n"))
}

# Top breach-rate province × category combinations
message("\n  Top 10 highest breach rates (AS1_raw):")
access %>%
  filter(!is.na(AS1_raw)) %>%
  arrange(desc(AS1_raw)) %>%
  head(10) %>%
  select(province, category, AS1_raw, mean_wait_days, n_providers) %>%
  { message(capture.output(print(as.data.frame(.), row.names = FALSE)) %>%
              paste(collapse = "\n")) }

# ── Step 7: Save ───────────────────────────────────────────────────────────────
saveRDS(access, file.path(PROCESSED_DIR, "access.rds"))

message("\n=== 05_access.R complete ===")
message("  access.rds : ", nrow(access), " rows (province × category)")
message("  Columns    : AS1_raw (breach rate), AS2_raw (z-scored wait),",
        " AS3_raw (neg. provider density)")
message("  ", n_with_data, " cells with data; ", n_without_data,
        " cells flagged NA (2-pillar CGI)")
