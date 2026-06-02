# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project

**Smart Service Project — Maastricht University × Platform DAAN.**
Build a **Care Gap Index (CGI)**: a three-pillar composite indicator (OECD/JRC + HAQ-style) measuring healthcare demand, supply, and access pressure across 12 Dutch provinces × ~27 medical specialisms × years 2025–2035.

Code indentation: 2 spaces. Encoding: UTF-8. Tidyverse style throughout.

---

## R Project Structure

The R project lives at `ResearchBacked_SS_proj/`. Build output goes into `cgi_project/` (sibling or subfolder):

```
cgi_project/
├── R/
│   ├── 00_config.R       paths, constants, package loading
│   ├── 01_ingest.R       read all raw data
│   ├── 02_mapping.R      DBC↔DAAN sector mapping matrix M
│   ├── 03_demand.R       age-adjusted DBC demand backbone
│   ├── 04_supply.R       DAAN workforce + productivity bridge φ
│   ├── 05_access.R       NZa wachttijden processing
│   ├── 06_indicators.R   compute all 9 raw indicators
│   ├── 07_normalize.R    min-max against 2025 baseline
│   ├── 08_aggregate.R    pillar + composite aggregation → CGI
│   ├── 09_uncertainty.R  Monte Carlo (1,000 draws)
│   ├── 10_sensitivity.R  weight/normalization/aggregation robustness
│   ├── 11_hotspots.R     hotspot classification
│   ├── 12_validate.R     convergent + face validity
│   ├── 13_visualize.R    full visualization suite
│   └── run_all.R         master script sourcing 00–13
├── data/
│   ├── raw/              symlink or copy from ../DATAN/
│   └── processed/        intermediate .rds outputs
└── output/
    ├── tables/           CSV exports
    ├── figures/          PNG/PDF plots
    └── report/
```

Each script reads inputs from `data/processed/` (except `01_ingest.R` which reads `data/raw/`). All intermediate datasets saved as `.rds`. No hardcoded paths outside `00_config.R`.

---

## Data Directory

Raw data at `../DATAN/` (`C:/Users/Samsung/OneDrive/Desktop/ProjectMay/DATAN/`):

```
DATAN/
├── demographics/
│   ├── cbs_age_gender.xlsx
│   ├── cbs_areas_netherlands.xlsx
│   ├── cbs_neighborhoods_core_figures.xlsx
│   ├── cbs_personal_characteristics.xlsx
│   ├── cbs_population_forecast_2023_2050.xlsx     ← Dataset B (CBS 85171)
│   ├── daan_demographics_download.xlsx
│   └── daan_personal_characteristics_cbs85454.xlsx
│
├── geography/
│   ├── cbs_gemeente_corop_cache.rds               ← cached gemeente↔COROP crosswalk
│   ├── cbs_housing_stock.xlsx
│   ├── cbs_municipalities_2020_onwards.xlsx       ← Dataset F (gemeente→province lookup)
│   ├── cbs_neighborhoods.xlsx
│   ├── cbs_postcodes.xlsx
│   ├── cibg_institution_locations.xlsx
│   ├── passenger_rate.xlsx
│   └── vektis_open_municipalities.xlsx
│
├── healthcare_demand/
│   ├── cbs_dbcs_by_diagnosis.xlsx
│   ├── cbs_deceased_by_medical_decision.xlsx
│   ├── cbs_healthcare_costs_by_income.xlsx
│   ├── cbs_healthcare_costs_key_figures.xlsx
│   └── vtv_public_health_projections.xlsx
│
├── healthcare_supply/
│   ├── care_insight_quality.xlsx
│   ├── cbs_institutions_financial_key_figures.xlsx
│   ├── cbs_institutions_financials_personnel.xlsx
│   ├── cbs_institutions_key_figures.xlsx
│   ├── cbs_labor_market_regions.xlsx
│   ├── daan_healthcare_activity.xlsx              ← Dataset C (DAAN workforce, 82,764 rows)
│   └── provider_waiting_times_healthcare_card.xlsx
│
├── nza/
│   ├── nza_dbc_care_product_groups.xlsx
│   ├── nza_dbc_diagnoses.xlsx
│   ├── nza_dbc_diagnosis_productgroup_relation.xlsx
│   ├── nza_dbc_open_dis.xlsx                      ← Dataset A (NZa OpenDIS DBC)
│   ├── nza_dbc_rates_table.xlsx
│   ├── nza_dbc_tarieventabel.xlsx
│   └── nza_msz_waiting_times.csv                  ← Dataset D (NZa wachttijden MSZ)
│
├── Vektis/
│   ├── vektis_specialisms.xlsx
│   └── vektis_zvw_municipalities.xlsx
│
└── raw_downloads/                                 ← original archives, do not edit
    ├── extracted/
    │   ├── daan_download_2026-04-23_v1.xlsx
    │   ├── daan_download_2026-04-23_v2.xlsx
    │   └── daan_new_data_2026-04-28.xlsx
    └── *.zip
```

**Data sources:** CBS (Statistics Netherlands), NZa (Dutch Healthcare Authority), DAAN (workforce platform), Vektis (insurance claims), VTV (public health projections), CIBG (institution registry).

---

## Pre-Defined Constants (treat as given — do not re-derive)

### Dataset E — Contacts per age band
```r
contacts_per_age <- tibble::tribble(
  ~age_band,  ~contacts_per_year,
  "0_15",     2.60,
  "15_25",    3.15,
  "25_45",    4.45,
  "45_65",    4.85,
  "65_plus",  5.74
)
```

### The 27 specialism categories (LLM-clustered from 3,000+ DBC codes)
Cardiology, Gynecology & Obstetrics, Neurology & Neurosurgery, Orthopedics, Ophthalmology, Pediatrics, Internal Medicine, Dermatology & Allergology, Pulmonology, Urology, ENT, Gastroenterology & Hepatology, Rheumatology, General & Trauma Surgery, Plastic & Reconstructive Surgery, Rehabilitation Medicine, Geriatrics, Vascular & Cardiothoracic Surgery, Pain Management & Other, Psychiatry & Mental Health, Oncology (+ ~6 further categories in data).

### Sector mapping matrix M (DBC↔DAAN sectors)
Only `Ziekenhuizen` (zkh), `UMC`, and `GGZ` (for Psychiatry only) receive non-zero shares. All other DAAN sectors (Kinderopvang, Jeugdzorg, Sociaal werk, Thuiszorg, Gehandicaptenzorg, Huisartsen) = 0 for all specialisms.

Representative shares (full table built in `02_mapping.R`):
- Most surgical/medical: zkh ~85–90%, umc ~10–15%
- Neurology & Neurosurgery: zkh 0.70, umc 0.30
- Vascular & Cardiothoracic Surgery: zkh 0.60, umc 0.40
- Psychiatry & Mental Health: GGZ 0.90, zkh 0.10
- Rehabilitation Medicine: zkh 0.75, umc 0.15 (remaining 0.10 to overig if needed)

Validate: `M %>% group_by(specialism) %>% summarise(total = sum(share))` → all = 1.0.

### Treeknorm thresholds (in `00_config.R`)
- Outpatient visit / Diagnosis: 28 days
- Treatment (behandeling): 42 days

### Monte Carlo parameters
- N = 1,000 draws
- tekort SD: empirical cross-gemeente SD within (province, sector)
- φ: LogNormal(log(φ), 0.15)
- ψ_a contact rates: N(ψ_a, 0.10 × ψ_a)
- Pillar weights: Dirichlet(1,1,1)

---

## CGI Methodology

### Formula
```
CGI_{p,s,t} = (DP_{p,s,t} + SP_{p,s,t} + AS_{p,s}) / 3

Province rollup:  CGI_{p,t} = Σ_s [ (DBC_{p,s,2025} / DBC_{p,.,2025}) × CGI_{p,s,t} ]
```

### 9 Indicators

**Demand Pressure (DP)**
- DP1 Per-capita need intensity: `ExpectedDBC_{p,s,t} / Pop_{p,t}`
- DP2 Ageing momentum: Δ share_65plus from 2025→t, weighted by specialism age-elasticity
- DP3 Demand growth rate: `(ExpectedDBC_{p,s,t} - ExpectedDBC_{p,s,2025}) / ExpectedDBC_{p,s,2025}`

**Supply Pressure (SP)**
- SP1 Shortage rate: `tekort_{p,k→s,t} / werkende_{p,k→s,t}` via M
- SP2 Retirement pressure: share_55plus weighted by M
- SP3 Productivity-adjusted FTE gap: `(φ_{s,p} × tekort_{p,k→s,t}) / ExpectedDBC_{p,s,t}`

**Access Stress (AS)** — 2025 snapshot, held constant across years
- AS1 Treeknorm breach rate: share of providers where wait > threshold
- AS2 Mean waiting days: z-scored against specialism-national mean
- AS3 Provider density (inverted): `unique_providers_{p,s} / (Pop_{p,2025} / 100k)`, inverted before normalization

### Key intermediate quantities
```
# Demand backbone (Eq. 1)
ExpectedDBC_{p,s,t} = [ Σ_a Pop_{p,a,t} × ψ_a ] × μ_s
  where μ_s = DBC_national_s / Σ_s DBC_national_s  (from 2025 NZa OpenDIS)

# Productivity bridge (Eq. 2)
φ_{s,p} = ExpectedDBC_{p,s,2025} / FTE_relevant_{p,s,2025}
  where FTE_relevant_{p,s} = Σ_k [ M_{s,k} × werkende_{p,k,2025} ]
```

### Normalization
Min-max anchored to 2025 cross-sectional distribution (fixed reference):
```
I_i*_{p,s,t} = (I_i_{p,s,t} - min_2025) / (max_2025 - min_2025)
```
Point-estimate normalized indicator values are clipped to [0, 1] for display (07_normalize.R). Monte Carlo draws in 09_uncertainty.R are intentionally unbounded — future deterioration beyond the 2025 reference range is meaningful signal. If min = max (degenerate), set all to 0.5.

### Hotspot classification
A cell (p,s) is a hotspot at year t if ALL THREE hold:
1. `CGI_{p,s,t} > 0.66`
2. `CGI_lower_90PI_{p,s,t} > 0.50`
3. All three sub-indices > 0.50

---

## Key Data Notes

- **DAAN workforce file** (`daan_healthcare_activity.xlsx`): 82,764 rows — 342 gemeenten × 11 sectors × 11 years (2025–2035). Key columns: `gemeente`, `sector`, `year`, `prognose_aantal_werkende` (FTE), `prognose_arbeids_vraag_tekort` (shortage, can be negative = surplus), `prognose_perc_werkende_0_25` … `prognose_perc_werkende_65plus`.
- **NZa wachttijden** (`nza_msz_waiting_times.csv`): point-in-time snapshot → t=2025 baseline only. Key columns: `wachttijd_type`, `waiting_time` (days), `specialism`, `postal_code`, `location`.
- **No wachttijden** for Oncology or Psychiatry & Mental Health → AS = NA for those specialisms → CGI = (DP + SP) / 2, flag `incomplete = TRUE`, `n_pillars = 2`.
- **DBC data** is national per specialism; allocate to provinces via age-adjusted population share.
- **Gemeente→province lookup**: `cbs_municipalities_2020_onwards.xlsx` or via `cbsodataR`.
- If any raw file is missing or has unexpected columns — **stop and ask** rather than guessing.

---

## Edge Cases (handle explicitly)

- `φ = Inf/NaN`: FTE_relevant = 0 → set φ = NA, SP3 = NA, flag `incomplete`
- `SP1` division by zero: werkende = 0 → SP1 = NA, flag
- Negative tekort: valid (surplus) — let normalization handle it, scores near 0
- AS missing: CGI = (DP + SP) / 2, `incomplete = TRUE`
- CGI > 1 in future years: cannot occur in point estimates (clipped), but can occur in MC draws — expected and meaningful

---

## Validation Targets

- Convergent validity: Spearman ρ between `CGI_{p,2025}` and DAAN `tekort/werkende` > 0.50
- Face validity: Flevoland, Utrecht, Noord-Holland, Zuid-Holland should appear in top quartile of `CGI_{p,2035}` (from prior analysis — used for sanity check only, not ground truth)
- Sub-index correlation: if any pair (DP, SP, AS) > 0.85 → flag as potential redundancy (document, do not remove)

---

## Required Packages

```r
c("tidyverse", "data.table", "sf", "COINr", "leaflet", "gt",
  "patchwork", "scales", "viridis", "ggrepel", "mc2d")
```
If `COINr` unavailable, implement composite construction manually per the formulas above.

---

## Expected Outputs

| File | Dimensions |
|------|-----------|
| `output/tables/cgi_panel.csv` | ~900 rows (12 provinces × ~25 specialisms × 3 years) |
| `output/tables/cgi_province_rollup.csv` | 36 rows (12 × 3 years) |
| `output/tables/hotspots_2035.csv` | hotspot cells only |
| `output/tables/sensitivity_summary.csv` | ≥6 rows |
| `output/figures/choropleth_2035.png` + `.html` | province-level CGI map |
| `output/figures/heatmap_province_specialism.png` | 12×25 tile heatmap |
| `output/figures/decomposition_bars.png` | DP/SP/AS stacked by province |
| `output/figures/trajectory_top12.png` | top 12 cells with 90% PI ribbon |
| `output/figures/sensitivity_tornado.png` | robustness summary |
| `output/figures/demand_bubble.png` | DBCs vs wait days vs Treeknorm breach |

---

## Common Commands

```bash
Rscript R/run_all.R                                      # full pipeline
Rscript R/00_config.R                                    # test config loads
Rscript -e "source('R/01_ingest.R')"                     # run single script
Rscript -e "testthat::test_dir('tests/testthat')"        # run tests
Rscript -e "renv::restore()"                             # restore packages
```
