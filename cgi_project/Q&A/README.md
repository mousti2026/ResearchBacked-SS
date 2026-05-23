# CGI Q&A Pipeline

Standalone presentation scripts for the Care Gap Index.
One script per sub-indicator — designed for live walkthroughs and Q&A sessions.

## Prerequisites

Run the main pipeline first (through at least `03_demand.R`) so that
`cgi_project/data/processed/` contains:

| File | Produced by |
|------|-------------|
| `pop_summary.rds` | `R/03_demand.R` |
| `dbc_shares.rds` | `R/01_ingest.R` |
| `workforce.rds` | `R/01_ingest.R` |
| `wachttijden.rds` | `R/01_ingest.R` |

## Scripts

| Script | Indicator | Pillar | Raw formula |
|--------|-----------|--------|-------------|
| `01_DP1.R` | Per-capita DBC need | Demand | `(contact_potential × μ_s) / pop_total` |
| `02_DP2.R` | Ageing momentum | Demand | `Δ share_65plus × ε_s` |
| `03_DP3.R` | Demand growth | Demand | `(cp_2035 − cp_2025) / cp_2025` |
| `04_SP1.R` | Shortage rate | Supply | `tekort_relevant / werkende_relevant` |
| `05_SP2.R` | Retirement pressure | Supply | M-weighted `share_55plus` |
| `06_SP3.R` | FTE gap | Supply | `φ_s × tekort_relevant / ExpectedDBC` |
| `07_AS1.R` | Treeknorm breach rate | Access | `n_breach / n_records` |
| `08_AS2.R` | Mean wait z-score | Access | `(mean_wait − μ_s) / σ_s` per specialism |
| `09_AS3.R` | Provider density (inverted) | Access | `−(providers / (pop / 100k))` |
| `10_CGI.R` | CGI composite | All | `(DP + SP + AS) / 3` |

Each script (01–09):
- Loads only the data it needs
- Prints `min_2025`, `max_2025`, normalised range, and a 5-row sample
- Saves one CSV to `output/` with columns `province`, `specialism`, `*_norm`

Script 10 joins all nine CSVs and prints the top-10 province × specialism
combinations by CGI. Specialisms without NZa waiting time data
(Oncology & Radiotherapy, Psychiatry & Mental Health) fall back to
`CGI = (DP + SP) / 2` and are flagged `incomplete_AS = TRUE`.

## Run

```bash
# Run all ten in sequence
for s in 01 02 03 04 05 06 07 08 09 10; do
  Rscript "cgi_project/Q&A/${s}_*.R"
done

# Or run a single script
Rscript "cgi_project/Q&A/01_DP1.R"
```

## Output

All CSVs are written to `output/` (git-ignored — regenerate by running the scripts).

| File | Columns |
|------|---------|
| `DP1_norm.csv` … `AS3_norm.csv` | `province`, `specialism`, `*_norm` |
| `phi_lookup.csv` | `specialism`, `phi` (national φ_s reference) |
| `CGI_final.csv` | `province`, `specialism`, `DP1_norm`…`AS3_norm`, `DP`, `SP`, `AS`, `CGI`, `incomplete_AS` |

## Design notes

- **SP1 vs SP3**: SP1 measures shortage as a fraction of the *existing workforce*;
  SP3 converts the shortage to undeliverable DBC volume using a national
  productivity benchmark φ_s, so the two indicators are analytically distinct.
- **Normalisation**: min-max anchored to the 2025 cross-sectional distribution.
  Values outside [0, 1] are valid in future projections and are not clipped.
- **No plots**: these scripts are numbers-only by design. Visualisations live
  in `R/13_visualize.R`.
