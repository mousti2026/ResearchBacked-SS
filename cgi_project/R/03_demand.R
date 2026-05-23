# Script: 03_demand.R — Purpose: Age-adjusted DBC demand backbone
#
# Implements Equation 1:
#   ExpectedDBC_{p,s,t} = [ Σ_a Pop_{p,a,t} × ψ_a ] × μ_s
#
#   ψ_a  = specialist contacts per person per year for age band a (contacts_per_age)
#   μ_s  = national share of total contacts for specialism s (from dbc_shares)
#
# Also computes province × year population summaries and specialism age-elasticities
# (ε_s) needed for DP2 (ageing momentum indicator in 06_indicators.R).
#
# Output: demand.rds  — province × category × year with ExpectedDBC + pop summaries

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
population  <- readRDS(file.path(PROCESSED_DIR, "population.rds"))
dbc_shares  <- readRDS(file.path(PROCESSED_DIR, "dbc_shares.rds"))

# ── Step 1: Province × year population summaries ───────────────────────────────
message("Computing province × year population summaries...")

# Join contact rates onto population table
pop_contacts <- population %>%
  left_join(contacts_per_age, by = "age_band")

# For each province × year:
#   - total population
#   - 65+ share (for DP2 ageing momentum)
#   - age-weighted contact potential = Σ_a Pop_{p,a,t} × ψ_a  (Eq. 1 bracket term)
pop_summary <- pop_contacts %>%
  group_by(province, year) %>%
  summarise(
    pop_total        = sum(population,                           na.rm = TRUE),
    pop_65plus       = sum(population[age_band == "65_plus"],    na.rm = TRUE),
    share_65plus     = pop_65plus / pop_total,
    contact_potential = sum(population * contacts_per_year,      na.rm = TRUE),
    .groups = "drop"
  )

# Validate: 12 provinces × 3 years = 36 rows
stopifnot("pop_summary: 36 rows expected" = nrow(pop_summary) == 36)

message("  pop_summary: ", nrow(pop_summary), " rows | ",
        "NL mean 65+ share 2025: ",
        round(weighted.mean(
          pop_summary$share_65plus[pop_summary$year == 2025],
          pop_summary$pop_total[pop_summary$year == 2025]
        ) * 100, 1), "%",
        " → 2035: ",
        round(weighted.mean(
          pop_summary$share_65plus[pop_summary$year == 2035],
          pop_summary$pop_total[pop_summary$year == 2035]
        ) * 100, 1), "%")

# ── Step 2: Specialism age-elasticities (ε_s) ─────────────────────────────────
# ε_s captures how strongly ageing amplifies demand for specialism s.
# Defined as the relative over/under-representation of 65+ demand for that
# specialism compared to the population average.
#
# Derivation: clinically-informed lookup, consistent with Capaciteitsorgaan
# (2022) age-demand relationships per specialism group.
# ε = 1.0 is the neutral baseline (demand grows proportionally with 65+ share).
# ε > 1.0 means elderly-heavy; ε < 1.0 means younger-skewed.
# Used only in DP2; all ε values are sensitivity-tested in 10_sensitivity.R.

age_elasticity <- tribble(
  ~category,                              ~epsilon,
  # Strongly elderly-skewed (ε ≥ 1.4)
  "Geriatrics & Elderly Care",            2.00,  # by definition elderly-focused
  "Cardiology",                           1.50,  # CVRM burden accelerates with age
  "Orthopedics",                          1.45,  # prosthetics, osteoarthritis
  "Ophthalmology",                        1.40,  # cataract, macular degeneration
  "Neurology & Neurosurgery",             1.35,  # stroke, Parkinson, dementia surgery
  "Pulmonology",                          1.30,  # COPD, respiratory failure
  "Urology",                              1.30,  # prostate, incontinence
  "Rheumatology",                         1.25,  # osteoarthritis rises with age
  "Internal Medicine",                    1.20,  # diabetes, multimorbidity
  "Gastroenterology & Hepatology",        1.15,  # colorectal, hepatic disease
  "Cardiothoracic Surgery",               1.30,  # cardiac surgery older patients
  "Pain Management & Anaesthesiology",    1.15,  # chronic pain increases with age
  "Clinical Genetics",                    1.00,  # age-neutral (mostly congenital)
  "Radiology & Interventional Radiology", 1.10,  # mild age effect via referral mix
  "General & Trauma Surgery",             1.10,  # hernia, bowel resection; trauma any age
  "Rehabilitation Medicine",              1.20,  # post-stroke, post-fracture rehab
  "Plastic & Reconstructive Surgery",     1.00,  # mixed ages; reconstructive vs cosmetic
  # Mildly elderly-skewed or neutral
  "Dermatology & Allergology",            1.05,  # skin cancer increases; allergy flat
  "ENT",                                  1.00,  # flat age distribution
  "Sports Medicine & Other",              0.80,  # younger athletes, sports injuries
  "Audiology",                            1.20,  # hearing loss increases with age
  "Oncology & Radiotherapy",              1.20,  # cancer incidence rises with age
  # Younger-skewed (ε < 1.0)
  "Psychiatry & Mental Health",           0.90,  # peaks working age / young adults
  "Gynecology & Obstetrics",             0.60,  # reproductive-age concentrated
  "Pediatrics",                           0.20   # child-focused; declines with 65+ share
)

# Validate all 25 categories have an elasticity
missing_epsilon <- setdiff(ALL_CATEGORIES, age_elasticity$category)
if (length(missing_epsilon) > 0)
  stop("Missing age-elasticity for: ", paste(missing_epsilon, collapse = ", "))

message("  age_elasticity: ", nrow(age_elasticity), " categories defined")
message("  Range: ", min(age_elasticity$epsilon), " – ", max(age_elasticity$epsilon))

# ── Step 3: Expected DBC demand — full panel ───────────────────────────────────
message("Computing ExpectedDBC panel (province × category × year)...")

# Cross-join province×year summaries with specialism shares, then apply Eq. 1
demand <- pop_summary %>%
  # Cross-join with all categories (each province×year gets all 25 specialisms)
  cross_join(dbc_shares %>% select(category, mu_s)) %>%
  mutate(
    # Eq. 1: expected DBC volume for specialism s in province p at year t
    expected_dbc = contact_potential * mu_s   # [contacts] × [dimensionless share]
  ) %>%
  select(province, category, year, pop_total, pop_65plus,
         share_65plus, contact_potential, expected_dbc)

# Attach age-elasticity
demand <- demand %>%
  left_join(age_elasticity %>% select(category, epsilon),
            by = "category")

# Validate: 12 × 25 × 3 = 900 rows
stopifnot("demand panel: 900 rows expected" = nrow(demand) == 900)

# ── Step 4: Sanity checks ──────────────────────────────────────────────────────
message("Running sanity checks...")

# 4a. National total ExpectedDBC 2025 should be in the same order of magnitude
#     as the observed 16M national DBC subtrajecten.
#     Note: ψ_a are specialist contacts, not DBC episodes — ratio will differ.
nl_expected_2025 <- demand %>%
  filter(year == 2025) %>%
  group_by(province) %>%
  # sum over categories gives total contacts for that province (contact_potential × 1)
  slice(1) %>%   # one row per province (contact_potential is province×year level)
  ungroup() %>%
  summarise(nl_contacts = sum(contact_potential)) %>%
  pull()

message("  NL total contact potential 2025: ",
        format(round(nl_expected_2025 / 1e6, 1), nsmall = 1), "M specialist contacts")
message("  Implied contacts/DBC ratio: ",
        round(nl_expected_2025 / 16000858, 2),
        " (contacts per DBC episode — expected ~4–6)")

# 4b. Province shares of national demand should roughly match population shares
demand_shares <- demand %>%
  filter(year == 2025) %>%
  group_by(province) %>%
  summarise(province_demand = sum(expected_dbc), .groups = "drop") %>%
  mutate(demand_share = province_demand / sum(province_demand) * 100)

pop_shares <- pop_summary %>%
  filter(year == 2025) %>%
  mutate(pop_share = pop_total / sum(pop_total) * 100) %>%
  select(province, pop_share)

share_check <- demand_shares %>%
  left_join(pop_shares, by = "province") %>%
  mutate(diff_pp = round(demand_share - pop_share, 2))

message("\n  Province demand vs population shares (2025):")
share_check %>%
  arrange(desc(demand_share)) %>%
  { message(capture.output(print(as.data.frame(.), row.names = FALSE)) %>%
              paste(collapse = "\n")) }

# 4c. Demand growth 2025→2035 should be positive for ageing provinces
growth_check <- demand %>%
  filter(category == "Cardiology") %>%
  select(province, year, expected_dbc) %>%
  pivot_wider(names_from = year, values_from = expected_dbc,
              names_prefix = "y") %>%
  mutate(growth_pct = round((y2035 - y2025) / y2025 * 100, 1)) %>%
  arrange(desc(growth_pct))

message("\n  Cardiology demand growth 2025→2035 by province (%):")
message(capture.output(print(as.data.frame(growth_check), row.names = FALSE)) %>%
          paste(collapse = "\n"))

# ── Step 5: Save ───────────────────────────────────────────────────────────────
saveRDS(demand,         file.path(PROCESSED_DIR, "demand.rds"))
saveRDS(pop_summary,    file.path(PROCESSED_DIR, "pop_summary.rds"))
saveRDS(age_elasticity, file.path(PROCESSED_DIR, "age_elasticity.rds"))

message("\n=== 03_demand.R complete ===")
message("  demand.rds       : ", nrow(demand), " rows (province × category × year)")
message("  pop_summary.rds  : ", nrow(pop_summary), " rows")
message("  age_elasticity.rds: ", nrow(age_elasticity), " categories")
message("  Key columns: expected_dbc, pop_total, share_65plus, epsilon")
