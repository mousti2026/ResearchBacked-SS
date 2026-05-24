# Script: 13_visualize.R — Purpose: Full visualization suite + data exports
#
# Generates:
#   7.1 Province choropleth map (CGI 2035) — PNG + interactive HTML
#   7.2 Province × specialism heatmap
#   7.3 Sub-index decomposition stacked bar
#   7.4 Trajectory plot — top 12 hotspot cells
#   7.5 Sensitivity tornado plot (Spearman ρ)
#   7.6 National demand bubble chart
#   Data exports: cgi_panel.csv, cgi_province_rollup.csv,
#                 hotspots_2035.csv, sensitivity_summary.csv

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
cgi                <- readRDS(file.path(PROCESSED_DIR, "cgi.rds"))
cgi_rollup         <- readRDS(file.path(PROCESSED_DIR, "cgi_rollup.rds"))
cgi_intervals      <- readRDS(file.path(PROCESSED_DIR, "cgi_intervals.rds"))
hotspots           <- readRDS(file.path(PROCESSED_DIR, "hotspots.rds"))
sensitivity        <- readRDS(file.path(PROCESSED_DIR, "sensitivity_summary.rds"))
access             <- readRDS(file.path(PROCESSED_DIR, "access.rds"))
dbc_shares         <- readRDS(file.path(PROCESSED_DIR, "dbc_shares.rds"))

# Province ordering (by 2035 CGI descending)
prov_order <- cgi_rollup %>%
  filter(year == 2035) %>%
  arrange(desc(CGI)) %>%
  pull(province)

# Specialism ordering (by national CGI descending at 2035)
cat_order <- cgi %>%
  filter(year == 2035) %>%
  group_by(category) %>%
  summarise(mean_cgi = mean(CGI, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(mean_cgi)) %>%
  pull(category)

# ── 7.1 Province choropleth (CBS WFS spatial data) ─────────────────────────────
message("7.1 Province choropleth...")

# Try to get CBS province boundaries via sf WFS
# Fallback: skip choropleth if spatial data unavailable
prov_sf <- tryCatch({
  sf::read_sf(
    "WFS:https://service.pdok.nl/cbs/gebiedsindelingen/2024/wfs/v1_0",
    layer = "gebiedsindelingen:provincie_gegeneraliseerd"
  ) %>%
    rename(province = statnaam) %>%
    mutate(province = case_when(
      province == "Friesland" ~ "Fryslân",
      TRUE ~ province
    ))
}, error = function(e) {
  message("  WFS unavailable (", conditionMessage(e), ") — skipping choropleth")
  NULL
})

if (!is.null(prov_sf)) {
  prov_cgi_2035 <- cgi_rollup %>%
    filter(year == 2035) %>%
    select(province, CGI)

  prov_sf_cgi <- prov_sf %>%
    left_join(prov_cgi_2035, by = "province")

  # Static PNG
  p_choropleth <- ggplot(prov_sf_cgi) +
    geom_sf(aes(fill = CGI), colour = "white", linewidth = 0.4) +
    scale_fill_viridis_c(name = "CGI",
                         option = "C",
                         limits = c(0.5, 0.7),
                         oob    = scales::squish,
                         labels = scales::number_format(accuracy = 0.01)) +
    labs(title = "Care Gap Index by Province, 2035 Forecast",
         subtitle = "Higher = greater demand–supply–access pressure",
         caption  = "CGI: Composite of Demand Pressure, Supply Pressure, Access Stress\nSmart Service Project, Maastricht University × Platform DAAN") +
    theme_void(base_size = 12) +
    theme(legend.position = "right",
          plot.title = element_text(face = "bold", size = 14),
          plot.subtitle = element_text(colour = "grey40"))

  ggsave(file.path(FIGURES_DIR, "choropleth_2035.png"),
         p_choropleth, width = 8, height = 6, dpi = 300)
  message("  Saved choropleth_2035.png")

  # Interactive leaflet
  # Transform to WGS84 for leaflet
  prov_wgs84 <- sf::st_transform(prov_sf_cgi, crs = 4326)
  pal <- leaflet::colorNumeric("YlOrRd", domain = prov_wgs84$CGI, na.color = "#aaaaaa")

  m <- leaflet::leaflet(prov_wgs84) %>%
    leaflet::addProviderTiles("CartoDB.Positron") %>%
    leaflet::addPolygons(
      fillColor   = ~ pal(CGI),
      fillOpacity = 0.75,
      color       = "white",
      weight      = 1,
      label       = ~ paste0(province, ": CGI=", round(CGI, 3))
    ) %>%
    leaflet::addLegend("bottomright", pal = pal, values = ~ CGI,
                       title = "CGI 2035", opacity = 0.9)

  htmlwidgets::saveWidget(m,
    file.path(FIGURES_DIR, "choropleth_2035.html"),
    selfcontained = TRUE)
  message("  Saved choropleth_2035.html")
} else {
  message("  Choropleth skipped — no spatial data")
}

# ── 7.2 Province × specialism heatmap ─────────────────────────────────────────
message("7.2 Heatmap (province × specialism)...")

heatmap_data <- cgi %>%
  filter(year == 2035) %>%
  mutate(
    province = factor(province, levels = rev(prov_order)),
    category = factor(category, levels = rev(cat_order)),
    hotspot  = hotspots$hotspot_flag[match(
      paste(province, category, year),
      paste(hotspots$province, hotspots$category, hotspots$year)
    )]
  )

p_heatmap <- ggplot(heatmap_data, aes(x = category, y = province, fill = CGI)) +
  geom_tile(colour = "white", linewidth = 0.3) +
  geom_tile(data = filter(heatmap_data, hotspot),
            colour = "black", linewidth = 0.8, fill = NA) +
  scale_fill_viridis_c(name = "CGI",
                       option = "C",
                       limits = c(0, 0.9),
                       oob    = scales::squish) +
  scale_x_discrete(position = "top") +
  labs(title = "Care Gap Index: Province × Specialism, 2035",
       subtitle = "Bold border = hotspot (CGI>0.66, PI lo90>0.50, all pillars>0.50)",
       x = NULL, y = NULL,
       caption = "Ordered by province CGI (top) and specialism national CGI (left)") +
  theme_minimal(base_size = 9) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 0, size = 7),
    axis.text.y = element_text(size = 9),
    panel.grid  = element_blank(),
    legend.position = "right",
    plot.title  = element_text(face = "bold", size = 12)
  )

ggsave(file.path(FIGURES_DIR, "heatmap_province_specialism.png"),
       p_heatmap, width = 16, height = 7, dpi = 300)
message("  Saved heatmap_province_specialism.png")

# ── 7.3 Sub-index decomposition stacked bar ────────────────────────────────────
message("7.3 Decomposition stacked bars...")

rollup_long <- cgi_rollup %>%
  filter(year == 2035) %>%
  mutate(province = factor(province, levels = rev(prov_order))) %>%
  select(province, DP, SP, AS) %>%
  pivot_longer(cols = c(DP, SP, AS),
               names_to = "pillar", values_to = "value") %>%
  mutate(pillar = factor(pillar, levels = c("AS", "SP", "DP")))

p_bars <- ggplot(rollup_long, aes(x = province, y = value, fill = pillar)) +
  geom_col(position = "stack", width = 0.7) +
  scale_fill_manual(
    values = c(DP = "#2166ac", SP = "#d6604d", AS = "#f4a582"),
    labels = c(DP = "Demand Pressure", SP = "Supply Pressure", AS = "Access Stress")
  ) +
  coord_flip() +
  labs(title = "What Drives Each Province's Care Gap? (2035)",
       subtitle = "Province CGI decomposed by sub-index contribution",
       x = NULL, y = "Sub-index contribution to CGI",
       fill = NULL,
       caption = "Smart Service Project, Maastricht University × Platform DAAN") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom",
        plot.title = element_text(face = "bold"))

ggsave(file.path(FIGURES_DIR, "decomposition_bars.png"),
       p_bars, width = 10, height = 6, dpi = 300)
message("  Saved decomposition_bars.png")

# ── Wide-ribbon identification (ribbon width > 0.40 at any year) ──────────────
message("Identifying wide-ribbon cells (PI width > 0.40)...")

ribbon_widths <- cgi_intervals %>%
  mutate(pi_width = CGI_hi90 - CGI_lo90)

wide_ribbon_cells <- ribbon_widths %>%
  group_by(province, category) %>%
  summarise(max_pi_width = max(pi_width, na.rm = TRUE), .groups = "drop") %>%
  filter(max_pi_width > 0.40) %>%
  arrange(desc(max_pi_width))

if (nrow(wide_ribbon_cells) > 0) {
  message("  Wide-ribbon cells (PI width > 0.40 at any year):")
  message(capture.output(print(as.data.frame(wide_ribbon_cells), row.names = FALSE)) %>%
            paste(collapse = "\n"))
} else {
  message("  No wide-ribbon cells found.")
}

# ── 7.4 Trajectory plot — top 12 hotspot cells (top 3 highlighted) ────────────
message("7.4 Trajectory plot...")

top12_cells <- hotspots %>%
  filter(year == 2035, hotspot_flag) %>%
  arrange(desc(CGI)) %>%
  head(12) %>%
  select(province, category)

trajectory_data <- hotspots %>%
  inner_join(top12_cells, by = c("province", "category")) %>%
  left_join(cgi_intervals %>% select(province, category, year, CGI_hi90),
            by = c("province", "category", "year")) %>%
  mutate(cell_label = paste0(province, " \u2014 ",
                              stringr::str_trunc(category, 22)))

# Split: top 3 by CGI at 2035 (highlighted) vs remaining 9 (grey)
top3_labels <- trajectory_data %>%
  filter(year == 2035) %>%
  arrange(desc(CGI)) %>%
  slice_head(n = 3) %>%
  pull(cell_label)

grey9_labels <- setdiff(unique(trajectory_data$cell_label), top3_labels)

traj_top3 <- trajectory_data %>% filter(cell_label %in% top3_labels)
traj_grey  <- trajectory_data %>% filter(cell_label %in% grey9_labels)

highlight_colors <- setNames(c("#d62728", "#ff7f0e", "#1f77b4"), top3_labels)
colour_vals      <- c(setNames(rep("grey65", length(grey9_labels)), grey9_labels),
                      highlight_colors)

p_traj <- ggplot() +
  # Ribbons for top 3 only
  geom_ribbon(data = traj_top3,
              aes(x = year, ymin = CGI_lo90, ymax = CGI_hi90, fill = cell_label),
              alpha = 0.18, colour = NA, show.legend = FALSE) +
  # Grey lines for bottom 9 (these appear in the legend)
  geom_line(data = traj_grey,
            aes(x = year, y = CGI, colour = cell_label),
            linewidth = 0.5) +
  # Colored lines + points for top 3 (no legend entry — labeled directly)
  geom_line(data = traj_top3,
            aes(x = year, y = CGI, colour = cell_label),
            linewidth = 1.3, show.legend = FALSE) +
  geom_point(data = traj_top3,
             aes(x = year, y = CGI, colour = cell_label),
             size = 2.5, show.legend = FALSE) +
  # Threshold line
  geom_hline(yintercept = 0.66, linetype = "dashed", colour = "grey50",
             linewidth = 0.5) +
  # Direct labels for top 3 at 2035
  ggrepel::geom_label_repel(
    data    = traj_top3 %>% filter(year == 2035),
    aes(x = year, y = CGI, label = cell_label, colour = cell_label),
    size = 2.8, show.legend = FALSE,
    nudge_x = 0.5, direction = "y", max.overlaps = 12
  ) +
  scale_fill_manual(values = highlight_colors, guide = "none") +
  scale_colour_manual(
    values = colour_vals,
    name   = NULL,
    breaks = grey9_labels   # only grey cells appear in legend
  ) +
  scale_x_continuous(breaks = YEARS_ANALYSIS,
                     expand = expansion(mult = c(0.02, 0.18))) +
  scale_y_continuous(limits = c(0, NA),
                     labels = scales::number_format(accuracy = 0.01)) +
  labs(title    = "CGI Trajectories — Top 12 Hotspot Cells",
       subtitle = "Ribbon = 90% PI (top 3 highlighted); dashed = hotspot threshold (0.66)",
       x = "Year", y = "CGI",
       caption  = "Smart Service Project") +
  theme_minimal(base_size = 11) +
  theme(legend.position   = "right",
        legend.text       = element_text(size = 7),
        legend.key.height = unit(0.4, "cm"),
        plot.title        = element_text(face = "bold"))

ggsave(file.path(FIGURES_DIR, "trajectory_top12.png"),
       p_traj, width = 12, height = 7, dpi = 300)
message("  Saved trajectory_top12.png")

# ── 7.5 Sensitivity tornado plot ──────────────────────────────────────────────
message("7.5 Sensitivity tornado plot...")

sens_plot_data <- sensitivity %>%
  filter(!is.na(spearman_rho)) %>%
  mutate(
    label      = sensitivity_test,
    is_central = spearman_rho == 1.0,
    bar_colour = case_when(
      spearman_rho < 0.85 ~ "High impact",
      spearman_rho < 0.95 ~ "Moderate impact",
      TRUE                ~ "Low / no impact"
    )
  ) %>%
  arrange(spearman_rho)

p_tornado <- ggplot(sens_plot_data,
                    aes(x = spearman_rho, y = reorder(label, spearman_rho),
                        fill = bar_colour)) +
  geom_col(width = 0.7) +
  geom_vline(xintercept = 1, linetype = "dashed", colour = "grey30") +
  scale_fill_manual(
    values = c("High impact"     = "#d62728",
               "Moderate impact" = "#ff7f0e",
               "Low / no impact" = "#1f77b4"),
    name = "Impact level"
  ) +
  scale_x_continuous(labels = scales::number_format(accuracy = 0.01)) +
  coord_cartesian(xlim = c(0.60, 1.05)) +
  labs(title = "Robustness of CGI Rankings",
       subtitle = "Spearman ρ vs central specification (province × specialism cell rankings (~300 cells), 2025)",
       x = "Spearman ρ vs central", y = NULL,
       caption = "ρ=1 = identical rankings; lower ρ = greater sensitivity") +
  theme_minimal(base_size = 10) +
  theme(legend.position = "bottom",
        plot.title = element_text(face = "bold"),
        axis.text.y = element_text(size = 8))

ggsave(file.path(FIGURES_DIR, "sensitivity_tornado.png"),
       p_tornado, width = 11, height = 8, dpi = 300)
message("  Saved sensitivity_tornado.png")

# ── 7.6 Demand bubble chart ───────────────────────────────────────────────────
message("7.6 Demand bubble chart...")

bubble_data <- dbc_shares %>%
  left_join(
    access %>%
      group_by(category) %>%
      summarise(
        mean_wait       = mean(mean_wait_days, na.rm = TRUE),
        pct_breach      = mean(AS1_raw,        na.rm = TRUE),
        .groups = "drop"
      ),
    by = "category"
  ) %>%
  filter(!is.na(mean_wait)) %>%
  mutate(
    label_short = case_when(
      nchar(category) > 15 ~ paste0(substr(category, 1, 14), "…"),
      TRUE ~ category
    )
  )

p_bubble <- ggplot(bubble_data,
                   aes(x = n_subtraject_base / 1e6,
                       y = mean_wait,
                       size = pct_breach,
                       colour = pct_breach,
                       label = label_short)) +
  geom_point(alpha = 0.8) +
  ggrepel::geom_text_repel(size = 2.8, max.overlaps = 20,
                            segment.colour = "grey70") +
  scale_size_continuous(name   = "% > Treeknorm",
                        range  = c(2, 12),
                        labels = scales::percent_format()) +
  scale_colour_viridis_c(name   = "% > Treeknorm",
                         option = "C",
                         labels = scales::percent_format()) +
  scale_x_continuous(labels = scales::number_format(suffix = "M")) +
  labs(title = "Demand Volume vs Mean Waiting Time by Specialism (2025)",
       subtitle = "Bubble size & colour = Treeknorm breach rate",
       x = "National DBC volume 2025 (millions)",
       y = "Mean waiting days (across all provinces)",
       caption = "Smart Service Project, Maastricht University × Platform DAAN") +
  theme_minimal(base_size = 11) +
  theme(legend.position = "right",
        plot.title = element_text(face = "bold"))

ggsave(file.path(FIGURES_DIR, "demand_bubble.png"),
       p_bubble, width = 12, height = 7, dpi = 300)
message("  Saved demand_bubble.png")

# ── 7.7 Data exports ──────────────────────────────────────────────────────────
message("7.7 Saving CSV exports...")

# Full CGI panel
cgi_panel <- cgi %>%
  left_join(cgi_intervals %>% select(province, category, year,
                                      CGI_mean, CGI_lo90, CGI_hi90),
            by = c("province", "category", "year")) %>%
  left_join(hotspots %>% select(province, category, year, hotspot_flag),
            by = c("province", "category", "year")) %>%
  left_join(wide_ribbon_cells %>% select(province, category) %>%
              mutate(wide_ribbon = TRUE),
            by = c("province", "category")) %>%
  mutate(wide_ribbon = coalesce(wide_ribbon, FALSE)) %>%
  select(province, category, year,
         DP1_raw, DP2_raw, DP3_raw, DP1, DP2, DP3, DP,
         SP1_raw, SP2_raw, SP3_raw, SP1, SP2, SP3, SP,
         AS1_raw, AS2_raw, AS3_raw, AS1, AS2, AS3, AS,
         CGI, CGI_lo90, CGI_hi90,
         n_pillars_cgi, incomplete, hotspot_flag, wide_ribbon,
         expected_dbc, pop_total, share_65plus, supply_proxy, has_access_data)

write_csv(cgi_panel, file.path(TABLES_DIR, "cgi_panel.csv"))
message("  cgi_panel.csv: ", nrow(cgi_panel), " rows")

# Province rollup
write_csv(cgi_rollup, file.path(TABLES_DIR, "cgi_province_rollup.csv"))
message("  cgi_province_rollup.csv: ", nrow(cgi_rollup), " rows")

# Hotspots 2035
hotspots_2035 <- hotspots %>%
  filter(year == 2035, hotspot_flag) %>%
  left_join(cgi_intervals %>% select(province, category, year, CGI_hi90),
            by = c("province", "category", "year")) %>%
  arrange(desc(CGI)) %>%
  select(province, category, CGI, DP, SP, AS, CGI_lo90, CGI_hi90,
         n_pillars_cgi, incomplete)

write_csv(hotspots_2035, file.path(TABLES_DIR, "hotspots_2035.csv"))
message("  hotspots_2035.csv: ", nrow(hotspots_2035), " rows")

# Sensitivity summary
write_csv(sensitivity, file.path(TABLES_DIR, "sensitivity_summary.csv"))
message("  sensitivity_summary.csv: ", nrow(sensitivity), " rows")

# ── Summary ───────────────────────────────────────────────────────────────────
message("\n=== 13_visualize.R complete ===")
message("  Figures saved to: ", FIGURES_DIR)
message("  Tables saved to:  ", TABLES_DIR)
