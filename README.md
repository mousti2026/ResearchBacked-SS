# Care Gap Index (CGI)

**Smart Service Project — Maastricht University × Platform DAAN**

A three-pillar composite indicator measuring healthcare demand, supply, and access pressure
across 12 Dutch provinces × 25 medical specialisms × years 2025, 2030, and 2035.
Methodology follows OECD/JRC composite indicator guidelines and the HAQ-index tradition.

---

## Repository layout

```
ResearchBacked_SS_proj/
├── cgi_project/
│   ├── R/                  Pipeline scripts (00–13 + run_all.R)
│   ├── Q&A/                Standalone presentation scripts (one per indicator)
│   ├── data/
│   │   ├── raw/            Input data (not committed — symlink to ../DATAN/)
│   │   └── processed/      Intermediate .rds outputs (pipeline artefacts)
│   ├── output/
│   │   ├── figures/        PNG / HTML visualisations
│   │   └── tables/         CSV exports
│   └── app.R               Shiny drill-down diagnostic tool
├── renv/                   R package library (reproducible environment)
├── renv.lock
└── ResearchBacked_SS_proj.Rproj
```

Raw data lives at `../DATAN/` (not in this repository).
Sources: CBS, NZa, DAAN, Vektis, VTV, CIBG.

---

## Methodology

### CGI formula

```
CGI_{p,s,t} = (DP_{p,s,t} + SP_{p,s,t} + AS_{p,s}) / 3

Province rollup:  CGI_{p,t} = Σ_s [ w_{p,s} × CGI_{p,s,t} ]
  where  w_{p,s} = ExpectedDBC_{p,s,2025} / Σ_s ExpectedDBC_{p,s,2025}
```

Fallback for specialisms without NZa waiting-time data (Audiology, Clinical Genetics,
Oncology & Radiotherapy, Sports Medicine & Other, and partially Cardiothoracic Surgery /
Psychiatry & Mental Health): `CGI = (DP + SP) / 2`, flagged `incomplete = TRUE`.

### Three pillars, nine indicators

| Pillar | Code | Indicator | Formula |
|--------|------|-----------|---------|
| Demand Pressure | DP1 | Per-capita DBC need | `ExpectedDBC_{p,s,t} / pop_{p,t}` |
| | DP2 | Ageing momentum | `Δ share_65plus_{p,2025→t} × ε_s` |
| | DP3 | Demand growth rate | `(DBC_{p,s,t} − DBC_{p,s,2025}) / DBC_{p,s,2025}` |
| Supply Pressure | SP1 | Shortage rate | `tekort_relevant / fte_relevant` |
| | SP2 | Retirement pressure | M-weighted `share_55plus` |
| | SP3 | Productivity-adjusted FTE gap | `(φ_{p,s} × tekort_relevant) / ExpectedDBC` |
| Access Stress | AS1 | Treeknorm breach rate | `n_breach / n_records` |
| | AS2 | Mean wait z-score | `(mean_wait − μ_s) / σ_s` |
| AS3 | Provider density (inv.) | `−(unique_providers / (pop / 100k))` |

### Key intermediate quantities

```r
# Demand backbone (Eq. 1)
ExpectedDBC_{p,s,t} = [ Σ_a Pop_{p,a,t} × ψ_a ] × μ_s

# Productivity bridge (Eq. 2) — calibrated at 2025, held fixed
φ_{p,s} = ExpectedDBC_{p,s,2025} / FTE_relevant_{p,s,2025}
  where FTE_relevant_{p,s} = Σ_k M_prov[p,s,k] × werkende_{p,k}

# Province-calibrated M matrix
M_prov[p,s,k] ∝ M[s,k] × actual_sector_share[p,k]   (renormalised per p×s)
```

### Normalisation

Min-max anchored to the 2025 cross-sectional distribution (fixed reference):

```
I*_{p,s,t} = (I_{p,s,t} − min_2025) / (max_2025 − min_2025)
```

**Exception — DP2 and DP3**: both are 0 at 2025 by construction (change from 2025→2025 = 0).
Reference: `min_ref = 0`, `max_ref = max value at 2035`.

Values > 1 or < 0 in 2030/2035 are valid and intentional — they signal pressure beyond
the 2025 worst case. Do **not** clip.

---

## Pipeline

Each script reads from `data/processed/` and writes back to `data/processed/`.
`01_ingest.R` is the only script that reads `data/raw/`.

| Script | Purpose | Key output |
|--------|---------|------------|
| `00_config.R` | Paths, constants, packages | — |
| `01_ingest.R` | Load all raw data | `workforce.rds`, `dbc_shares.rds`, `wachttijden.rds`, … |
| `02_mapping.R` | Build sector allocation matrix M | `M_long.rds`, `M_wide.rds` |
| `03_demand.R` | Age-adjusted DBC demand backbone | `demand.rds`, `pop_summary.rds` |
| `04_supply.R` | Province-calibrated M, φ bridge | `supply.rds`, `phi.rds` |
| `05_access.R` | NZa wachttijden → AS1/AS2/AS3 | `access.rds` |
| `06_indicators.R` | Compute all 9 raw indicators | `indicators.rds` |
| `07_normalize.R` | Min-max normalisation | `indicators_norm.rds`, `norm_reference.rds` |
| `08_aggregate.R` | Pillar means + CGI composite | `cgi.rds`, `cgi_rollup.rds` |
| `09_uncertainty.R` | Monte Carlo (1,000 draws) | `mc_results.rds`, `cgi_intervals.rds` |
| `10_sensitivity.R` | Robustness tests (20 scenarios) | `sensitivity_summary.rds` |
| `11_hotspots.R` | Hotspot classification | `hotspots.rds` |
| `12_validate.R` | Convergent + face validity | console report |
| `13_visualize.R` | Full visualisation suite | figures + tables |

### Run the full pipeline

```bash
cd cgi_project
Rscript R/run_all.R          # ~1.3 minutes
```

Or run a single script:

```bash
Rscript R/06_indicators.R
```

### Validation targets (from `12_validate.R`)

| Check | Target | Latest result |
|-------|--------|---------------|
| Convergent ρ: SP pillar vs DAAN `tekort/werkende` | > 0.50 | **0.51 ✓** |
| Face validity: Utrecht/Flevoland/Zuid-Holland/Noord-Holland in top quartile 2035 | ≥ 3/4 | **3/4 ✓** |
| Sub-index pair correlation (DP, SP, AS) | all < 0.85 | **max 0.07 ✓** |

---

## Shiny app

A four-level drill-down diagnostic tool: **Map → Pillar → Indicator → Root Cause**.

**Live app:** https://mousticoder.shinyapps.io/cgi-diagnostic/

### Data sources

The app reads exactly two files at startup:

| File | Path | Content |
|------|------|---------|
| `cgi_rollup.rds` | `cgi_project/data/processed/` | Province × year DBC-weighted CGI rollup |
| `cgi.rds` | `cgi_project/data/processed/` | Province × specialism × year full indicator panel |

Province boundaries are fetched live from the CBS PDOK WFS
(`https://service.pdok.nl/cbs/gebiedsindelingen/2024/wfs/v1_0`).
The app degrades gracefully if the WFS is unreachable.

### Launch

```bash
Rscript -e ".libPaths(c('C:/Users/Samsung/AppData/Local/R/win-library/4.5', .libPaths())); \
  shiny::runApp('cgi_project/app.R', port=7778)"
```

The pipeline runs automatically on first launch (renv activates `run_all.R`).
Subsequent launches skip the pipeline if processed files are already present.

---

## Q&A scripts

Standalone presentation scripts in `cgi_project/Q&A/` — one per indicator.
Each script is self-contained (no pipeline dependency beyond `data/processed/`)
and prints `min`, `max`, normalised range, and a sample.

All Q&A scripts are fully aligned with the pipeline:
same province-calibrated M matrix, same φ computation, same normalisation references.

```bash
# Run all ten in sequence
for s in 01 02 03 04 05 06 07 08 09 10; do
  Rscript "cgi_project/Q&A/${s}_"*.R
done
```

Outputs (CSVs) are written to `cgi_project/Q&A/output/`.

---

## Outputs

### Figures (`cgi_project/output/figures/`)

| File | Description |
|------|-------------|
| `choropleth_2035.png` / `.html` | Province-level CGI map, 2035 |
| `heatmap_province_specialism.png` | 12 × 25 CGI tile heatmap |
| `decomposition_bars.png` | DP / SP / AS stacked by province |
| `trajectory_top12.png` | Top-12 cells with 90 % PI ribbon |
| `sensitivity_tornado.png` | Robustness summary |
| `demand_bubble.png` | DBCs vs wait days vs Treeknorm breach |

### Tables (`cgi_project/output/tables/`)

| File | Dimensions | Description |
|------|-----------|-------------|
| `cgi_panel.csv` | 900 rows | Province × specialism × year, all indicators |
| `cgi_province_rollup.csv` | 36 rows | Province × year DBC-weighted rollup |
| `hotspots_2035.csv` | variable | Hotspot cells at 2035 |
| `sensitivity_summary.csv` | 20 rows | Spearman ρ under each robustness scenario |

---

## Hotspot classification

A cell (province × specialism) is classified as a **hotspot** at year *t* if all three hold:

1. `CGI_{p,s,t} > 0.66`
2. `CGI_lower_90PI_{p,s,t} > 0.50`
3. All three sub-indices `> 0.50`

Results at 2035: **30 hotspot cells** across 10 provinces (led by Flevoland with 8).

---

## Reproducibility

```r
renv::restore()   # restore exact package versions from renv.lock
```

R version: 4.5.x. Key packages: tidyverse, sf, leaflet, shiny, bslib, plotly, mc2d.
