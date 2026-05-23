# Script: run_all.R — Master script: sources 00–13 in order
# Run this to execute the full CGI pipeline from scratch.
# Each script reads from data/processed/ (except 01 which reads raw data).
# Estimated runtime: ~5–10 minutes (dominated by MC in 09_uncertainty.R)

.script_dir <- tryCatch(
  dirname(normalizePath(sys.frames()[[1]]$ofile, winslash = "/")),
  error = function(e) {
    args <- commandArgs(trailingOnly = FALSE)
    f    <- sub("--file=", "", args[grep("--file=", args)])
    if (length(f) == 1) dirname(normalizePath(f, winslash = "/"))
    else "C:/Users/Samsung/OneDrive/Desktop/ProjectMay/ResearchBacked_SS_proj/cgi_project/R"
  }
)

scripts <- c(
  "00_config.R",
  "01_ingest.R",
  "02_mapping.R",
  "03_demand.R",
  "04_supply.R",
  "05_access.R",
  "06_indicators.R",
  "07_normalize.R",
  "08_aggregate.R",
  "09_uncertainty.R",
  "10_sensitivity.R",
  "11_hotspots.R",
  "12_validate.R",
  "13_visualize.R"
)

t_start <- proc.time()
message("\n", strrep("=", 60))
message("CGI Pipeline — run_all.R")
message(strrep("=", 60))

for (s in scripts) {
  message("\n--- Running ", s, " ---")
  t0 <- proc.time()
  source(file.path(.script_dir, s), local = new.env())
  elapsed <- round((proc.time() - t0)[["elapsed"]], 1)
  message("--- ", s, " done (", elapsed, "s) ---")
}

total <- round((proc.time() - t_start)[["elapsed"]] / 60, 1)
message("\n", strrep("=", 60))
message("CGI Pipeline complete! Total: ", total, " minutes")
message(strrep("=", 60))
