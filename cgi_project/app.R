# CGI Drill-Down Diagnostic Tool
# Smart Service Project — Maastricht University × Platform DAAN
# Shiny app: Map → Pillar → Indicator → Root Cause
# Data source: cgi_rollup.rds (province × year) + cgi.rds (province × category × year)

# Ensure user library is on the path (needed when running inside renv project)
candidate_lib <- file.path(
  Sys.getenv("LOCALAPPDATA", file.path(Sys.getenv("USERPROFILE"), "AppData", "Local")),
  "R", "win-library",
  paste0(R.Version()$major, ".", substr(R.Version()$minor, 1, 1))
)
if (dir.exists(candidate_lib) && !candidate_lib %in% .libPaths()) {
  .libPaths(c(candidate_lib, .libPaths()))
}

library(shiny)
library(bslib)
library(leaflet)
library(sf)
library(dplyr)
library(tidyr)
library(ggplot2)
library(plotly)
library(scales)

# plotly masks dplyr::filter — restore explicitly
filter <- dplyr::filter

# ── 1. Load pipeline RDS outputs ─────────────────────────────────────────────
processed_dir <- file.path(getwd(), "data", "processed")

cgi_rollup <- readRDS(file.path(processed_dir, "cgi_rollup.rds"))
# cols: province, year, CGI, DP, SP, AS, hotspot_count

cgi_detail <- readRDS(file.path(processed_dir, "cgi.rds"))
# cols: province, category, year, DP1-DP3, DP, SP1-SP3, SP, AS1-AS3, AS, CGI, ...

YEARS <- sort(unique(cgi_rollup$year))   # 2025, 2030, 2035

# Thresholds derived from the 2025 empirical CGI distribution (cell level)
# High = 2025 empirical maximum: hotspot = worse than 2025 worst-case
# Low  = 2025 median: separates moderate from low pressure
cgi_2025_vals   <- cgi_detail$CGI[cgi_detail$year == 2025 & !is.na(cgi_detail$CGI)]
CGI_THRESH_HIGH <- max(cgi_2025_vals)   # hotspot cutoff
CGI_THRESH_LOW  <- median(cgi_2025_vals) # moderate/low boundary

province_mc_data <- tryCatch(
  readRDS(file.path(processed_dir, "province_mc.rds")),
  error = function(e) {
    message("province_mc.rds not found — uncertainty markers disabled")
    NULL
  }
)

# ── 2. Indicator metadata ─────────────────────────────────────────────────────
ind_meta <- tibble::tribble(
  ~col,   ~label,                      ~pillar,  ~pillar_label,
  "DP1",  "Per-capita DBC need",       "DP",     "Demand Pressure",
  "DP2",  "Ageing momentum",           "DP",     "Demand Pressure",
  "DP3",  "Demand growth rate",        "DP",     "Demand Pressure",
  "SP1",  "Shortage rate",             "SP",     "Supply Pressure",
  "SP2",  "Retirement pressure",       "SP",     "Supply Pressure",
  "SP3",  "FTE gap",                   "SP",     "Supply Pressure",
  "AS1",  "Treeknorm breach rate",     "AS",     "Access Stress",
  "AS2",  "Mean wait z-score",         "AS",     "Access Stress",
  "AS3",  "Provider density (inv.)",   "AS",     "Access Stress"
)

ind_detail <- tibble::tribble(
  ~col,   ~formula,                                       ~interpretation,
  "DP1",  "(contact_potential × μ_s) / pop_total",       "Higher = more outpatient contacts per capita relative to the national norm. Driven by population size and specialism usage patterns.",
  "DP2",  "Δ share_65plus × age-elasticity",             "Higher = faster ageing of the provincial population weighted by how age-sensitive the specialism is.",
  "DP3",  "(DBC_2035 − DBC_2025) / DBC_2025",           "Higher = faster projected growth in DBC demand relative to the 2025 baseline.",
  "SP1",  "tekort / werkende (M-weighted)",              "Higher = larger workforce shortage as a share of the existing workforce, mapped via the sector allocation matrix M.",
  "SP2",  "M-weighted share of workers aged 55+",        "Higher = more workers near retirement age, signalling future supply erosion.",
  "SP3",  "φ_s × tekort / ExpectedDBC",                  "Higher = the productivity-adjusted workforce gap is large relative to expected DBC volume. Values >1 are valid at 2030/2035.",
  "AS1",  "n_breach / n_records (Treeknorm)",            "Higher = more providers exceed the 28-day (outpatient) or 42-day (treatment) Treeknorm threshold.",
  "AS2",  "(mean_wait − μ_s) / σ_s per specialism",     "Higher = waiting times are further above the national specialism average in z-score terms.",
  "AS3",  "−(unique providers / (pop / 100k))",          "Higher (after inversion) = lower provider density — fewer specialists per 100,000 population."
)

# ── 3. Load Netherlands province boundaries (CBS WFS) ─────────────────────────
message("Loading province boundaries...")
prov_sf <- tryCatch({
  sf::read_sf(
    "WFS:https://service.pdok.nl/cbs/gebiedsindelingen/2024/wfs/v1_0",
    layer = "gebiedsindelingen:provincie_gegeneraliseerd"
  ) %>%
    rename(province = statnaam) %>%
    mutate(province = case_when(
      province == "Friesland" ~ "Fryslân",
      TRUE ~ province
    )) %>%
    sf::st_transform(crs = 4326)
}, error = function(e) {
  message("WFS unavailable (", conditionMessage(e), ") — map will show fallback text")
  NULL
})

# Province centroids for uncertainty triangle markers
prov_centroids <- if (!is.null(prov_sf)) {
  suppressWarnings(sf::st_centroid(prov_sf)) %>%
    mutate(lng = sf::st_coordinates(.)[, 1],
           lat = sf::st_coordinates(.)[, 2]) %>%
    sf::st_drop_geometry() %>%
    select(province, lng, lat)
} else NULL

# SVG triangle icon factory
.triangle_icon <- function(fill_color, size = 22L) {
  half <- size %/% 2L
  svg  <- paste0(
    "<svg xmlns='http://www.w3.org/2000/svg' width='", size, "' height='", size, "'>",
    "<polygon points='", half, ",2 ", size - 2L, ",", size - 2L, " 2,", size - 2L,
    "' fill='", fill_color, "' stroke='white' stroke-width='1.5'/>",
    "</svg>"
  )
  uri <- paste0("data:image/svg+xml;charset=utf-8,",
    gsub("#", "%23", gsub("'", "%27", gsub(" ", "%20",
    gsub("<", "%3C",  gsub(">", "%3E", gsub("/", "%2F", svg)))))))
  makeIcon(iconUrl = uri, iconWidth = size, iconHeight = size,
           iconAnchorX = half, iconAnchorY = half)
}
unc_icons <- list(
  Low    = .triangle_icon("#4dac26"),
  Medium = .triangle_icon("#fc8d59"),
  High   = .triangle_icon("#d73027")
)

# CSS-triangle HTML legend for uncertainty tiers
unc_legend_html <- paste0(
  "<div style='background:white;padding:10px 14px;border-radius:8px;",
  "box-shadow:0 1px 5px rgba(0,0,0,.3);font-size:0.82rem;line-height:2;'>",
  "<b style='font-size:0.84rem;'>Forecast Uncertainty</b><br>",
  "<span style='display:inline-block;width:0;height:0;",
  "border-left:7px solid transparent;border-right:7px solid transparent;",
  "border-bottom:12px solid #4dac26;margin-right:5px;vertical-align:middle;'></span>Low<br>",
  "<span style='display:inline-block;width:0;height:0;",
  "border-left:7px solid transparent;border-right:7px solid transparent;",
  "border-bottom:12px solid #fc8d59;margin-right:5px;vertical-align:middle;'></span>Medium<br>",
  "<span style='display:inline-block;width:0;height:0;",
  "border-left:7px solid transparent;border-right:7px solid transparent;",
  "border-bottom:12px solid #d73027;margin-right:5px;vertical-align:middle;'></span>High<br>",
  "<span style='font-size:0.72rem;color:#777;'>(90% PI width of CGI)</span>",
  "</div>"
)

# Helper: append uncertainty triangle markers to leaflet / leafletProxy
.add_unc_markers <- function(map_obj, unc_data, centroids) {
  if (is.null(centroids) || is.null(unc_data) || nrow(unc_data) == 0) return(map_obj)
  pts <- centroids %>%
    left_join(
      unc_data %>% select(province, uncertainty_tier, CGI_lo90, CGI_hi90, PI_width),
      by = "province"
    ) %>%
    filter(!is.na(uncertainty_tier))
  for (tier_name in c("Low", "Medium", "High")) {
    sub <- pts %>% filter(uncertainty_tier == tier_name)
    if (nrow(sub) > 0) {
      map_obj <- map_obj %>%
        addMarkers(
          data  = sub,
          lng   = ~lng, lat = ~lat,
          icon  = unc_icons[[tier_name]],
          label = ~paste0(province, " — ", tier_name, " uncertainty",
                          " | 90% PI: [", round(CGI_lo90, 3), ", ",
                          round(CGI_hi90, 3), "]  (width: ", round(PI_width, 3), ")"),
          group = "uncertainty_markers"
        )
    }
  }
  map_obj
}

# ── 4. Color helpers ──────────────────────────────────────────────────────────
score_color <- function(x) {
  case_when(
    is.na(x)                ~ "#999999",
    x >= CGI_THRESH_HIGH    ~ "#d73027",
    x >= CGI_THRESH_LOW     ~ "#fc8d59",
    TRUE                    ~ "#4dac26"
  )
}

score_label <- function(x) {
  case_when(
    is.na(x)                ~ "No data",
    x >= CGI_THRESH_HIGH    ~ "High pressure",
    x >= CGI_THRESH_LOW     ~ "Moderate",
    TRUE                    ~ "Low pressure"
  )
}

# ── 5. UI ─────────────────────────────────────────────────────────────────────
ui <- page_fluid(
  theme = bs_theme(
    bootswatch = "flatly",
    base_font  = font_google("Inter"),
    `enable-shadows` = TRUE
  ),

  tags$head(tags$style(HTML("
    .breadcrumb-nav { font-size: 0.9rem; color: #555; margin-bottom: 10px; }
    .breadcrumb-nav span { cursor: pointer; color: #2c7bb6; }
    .breadcrumb-nav span:hover { text-decoration: underline; }
    .breadcrumb-sep { margin: 0 6px; color: #aaa; }
    .score-badge { display:inline-block; padding:3px 10px; border-radius:12px;
                   color:#fff; font-weight:600; font-size:0.85rem; }
    .metric-card { background:#f8f9fa; border-radius:8px; padding:14px;
                   margin-bottom:10px; border-left:4px solid #ccc; cursor:pointer; }
    .metric-card:hover { background:#e9ecef; }
    .rootcause-box { background:#fff3cd; border-left:4px solid #ffc107;
                     border-radius:6px; padding:16px; margin-top:10px; }
    .pillar-card { border-radius:8px; padding:20px; margin:8px; text-align:center;
                   cursor:pointer; transition:transform .15s; }
    .pillar-card:hover { transform:translateY(-3px); box-shadow:0 4px 12px rgba(0,0,0,.15); }
    .back-btn { margin-bottom:12px; }
    .title-bar { background:linear-gradient(135deg,#1a3a5c,#2c7bb6);
                 color:#fff; padding:14px 24px; border-radius:8px; margin-bottom:16px; }
    .year-badge { background:rgba(255,255,255,0.2); border:1px solid rgba(255,255,255,0.4);
                  border-radius:6px; padding:2px 8px; font-size:0.85rem; }
    .leaflet-container { border-radius: 8px; }
  "))),

  div(class = "title-bar",
    div(style = "display:flex; justify-content:space-between; align-items:center;",
      div(
        h3("Care Gap Index — Province Diagnostic Tool",
           style = "margin:0; font-weight:700;"),
        p("Smart Service Project · Maastricht University × Platform DAAN",
          style = "margin:4px 0 0; opacity:0.8; font-size:0.9rem;")
      ),
      div(style = "display:flex; align-items:center; gap:10px;",
        span("Forecast year:", class = "year-badge"),
        selectInput("sel_year", label = NULL,
                    choices  = setNames(YEARS, paste("Year", YEARS)),
                    selected = max(YEARS),
                    width    = "120px")
      )
    )
  ),

  uiOutput("breadcrumb"),
  uiOutput("main_panel")
)

# ── 6. Server ─────────────────────────────────────────────────────────────────
server <- function(input, output, session) {

  rv <- reactiveValues(
    view      = "map",
    province  = NULL,
    category  = "All",
    pillar    = NULL,
    indicator = NULL
  )

  # ── Year-filtered reactive slices ──────────────────────────────────────────
  sel_year <- reactive({ as.integer(input$sel_year %||% max(YEARS)) })

  # Province-level rollup for the selected year (directly from pipeline)
  prov_summary <- reactive({
    cgi_rollup %>% filter(year == sel_year())
  })

  # Continuous color palette — same scale used by both the map and the rankings
  map_pal <- reactive({
    ps <- prov_summary()
    colorNumeric(
      palette  = c("#4dac26", "#fee08b", "#d73027"),
      domain   = range(ps$CGI, na.rm = TRUE),
      na.color = "#999999"
    )
  })

  # Detail (province × category) for the selected year
  detail_yr <- reactive({
    cgi_detail %>% filter(year == sel_year())
  })

  # National averages of indicators across all provinces for the selected year
  nat_avg <- reactive({
    detail_yr() %>%
      summarise(
        CGI = mean(CGI, na.rm = TRUE),
        DP  = mean(DP,  na.rm = TRUE),
        SP  = mean(SP,  na.rm = TRUE),
        AS  = mean(AS,  na.rm = TRUE),
        across(all_of(ind_meta$col), ~ mean(.x, na.rm = TRUE))
      )
  })

  # Province + optional category filter
  filtered_data <- reactive({
    d <- detail_yr() %>% filter(province == rv$province)
    if (!is.null(input$sel_category) && input$sel_category != "All") {
      d <- d %>% filter(category == input$sel_category)
    }
    d
  })

  # Province-level MC intervals for the selected year
  prov_uncertainty <- reactive({
    if (is.null(province_mc_data)) return(tibble(province = character()))
    province_mc_data %>% filter(year == sel_year())
  })

  # Pillar scores that respect the category filter:
  # — "All": use DBC-weighted rollup (matches choropleth)
  # — specific category: use that category's own DP/SP/AS/CGI
  pillar_scores <- reactive({
    if (!is.null(input$sel_category) && input$sel_category != "All") {
      d <- filtered_data()
      list(CGI = mean(d$CGI, na.rm=TRUE),
           DP  = mean(d$DP,  na.rm=TRUE),
           SP  = mean(d$SP,  na.rm=TRUE),
           AS  = mean(d$AS,  na.rm=TRUE))
    } else {
      r <- prov_summary() %>% filter(province == rv$province)
      list(CGI = r$CGI, DP = r$DP, SP = r$SP, AS = r$AS)
    }
  })

  # ── Breadcrumb ──────────────────────────────────────────────────────────────
  output$breadcrumb <- renderUI({
    crumbs <- list(
      tags$span("Netherlands",
                onclick = "Shiny.setInputValue('nav_home',Math.random())")
    )
    if (!is.null(rv$province)) {
      crumbs <- c(crumbs, list(
        span(class = "breadcrumb-sep", ">"),
        tags$span(rv$province,
                  onclick = "Shiny.setInputValue('nav_province',Math.random())")
      ))
    }
    if (!is.null(rv$pillar)) {
      pl <- ind_meta$pillar_label[ind_meta$pillar == rv$pillar][1]
      crumbs <- c(crumbs, list(
        span(class = "breadcrumb-sep", ">"),
        tags$span(pl, onclick = "Shiny.setInputValue('nav_pillar',Math.random())")
      ))
    }
    if (!is.null(rv$indicator)) {
      crumbs <- c(crumbs, list(
        span(class = "breadcrumb-sep", ">"),
        tags$span(ind_meta$label[ind_meta$col == rv$indicator][1])
      ))
    }
    do.call(div, c(list(class = "breadcrumb-nav"), crumbs))
  })

  observeEvent(input$nav_home,     {
    rv$view <- "map"; rv$province <- NULL; rv$pillar <- NULL; rv$indicator <- NULL
  })
  observeEvent(input$nav_province, {
    rv$view <- "pillar"; rv$pillar <- NULL; rv$indicator <- NULL
  })
  observeEvent(input$nav_pillar,   {
    rv$view <- "indicator"; rv$indicator <- NULL
  })

  output$main_panel <- renderUI({
    switch(rv$view,
      "map"       = map_ui(),
      "pillar"    = pillar_ui(),
      "indicator" = indicator_ui(),
      "rootcause" = rootcause_ui()
    )
  })

  # ════════════════════════════════════════════════════════════════════════════
  # VIEW 1: MAP
  # ════════════════════════════════════════════════════════════════════════════
  map_ui <- function() {
    navset_tab(
      nav_panel("Province Map",
        fluidRow(
          column(8,
            card(
              card_header(paste("Care Gap Index —", sel_year(),
                                "| Click a province to explore")),
              leafletOutput("choropleth", height = "520px")
            )
          ),
          column(4,
            card(
              card_header("Province Rankings"),
              div(style = "font-size:0.78rem; color:#666; padding:4px 10px 2px;",
                  "Ranked by CGI score — 1 = highest care gap"),
              div(style = "max-height:500px; overflow-y:auto;",
                  uiOutput("prov_ranking"))
            )
          )
        )
      ),
      nav_panel("Cell-Level Heatmap",
        fluidRow(
          column(12,
            card(
              card_header(paste(
                "CGI by Province × Specialism —", sel_year(),
                "| CGI >", round(CGI_THRESH_HIGH, 2),
                "= hotspot (2025 empirical max) | Click a cell to explore province"
              )),
              p(style = "color:#666; font-size:0.85rem; padding:4px 16px 0;",
                "Province rollup scores (map view) are volume-weighted averages that mask
                 high-pressure specialism cells. This view shows the full cell-level picture."),
              plotlyOutput("cell_heatmap", height = "680px")
            )
          )
        )
      )
    )
  }

  output$choropleth <- renderLeaflet({
    req(rv$view == "map")
    ps <- prov_summary()

    if (is.null(prov_sf)) {
      leaflet() %>% addTiles() %>%
        setView(lng = 5.3, lat = 52.2, zoom = 7) %>%
        addControl("<b>Map unavailable</b><br>CBS WFS not reachable.",
                   position = "topright")
    } else {
      map_data <- prov_sf %>%
        left_join(ps %>% select(province, CGI), by = "province")

      pal <- map_pal()

      base_map <- leaflet(map_data) %>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        setView(lng = 5.3, lat = 52.3, zoom = 7) %>%
        addPolygons(
          layerId      = ~province,
          fillColor    = ~pal(CGI),
          fillOpacity  = 0.75,
          color        = "white",
          weight       = 1.5,
          highlight    = highlightOptions(
            weight = 3, color = "#1a3a5c", fillOpacity = 0.9, bringToFront = TRUE
          ),
          label        = ~paste0(province, ": ", round(CGI, 3)),
          labelOptions = labelOptions(style = list("font-size" = "13px"))
        ) %>%
        addLegend(
          pal       = pal,
          values    = ~CGI,
          title     = paste0("CGI Score (", sel_year(), ")<br><small style='font-weight:normal'>0 = low gap &bull; 1 = critical</small>"),
          position  = "bottomright",
          labFormat = labelFormat(digits = 2)
        ) %>%
        addControl(unc_legend_html, position = "topleft")

      .add_unc_markers(base_map, prov_uncertainty(), prov_centroids)
    }
  })

  # Re-draw polygons + uncertainty markers when year changes (without full map reset)
  observeEvent(sel_year(), {
    req(!is.null(prov_sf), rv$view == "map")
    ps       <- prov_summary()
    map_data <- prov_sf %>%
      left_join(ps %>% select(province, CGI), by = "province")
    pal <- map_pal()

    proxy <- leafletProxy("choropleth") %>%
      clearShapes() %>%
      clearMarkers() %>%
      clearControls() %>%
      addPolygons(
        data         = map_data,
        layerId      = ~province,
        fillColor    = ~pal(CGI),
        fillOpacity  = 0.75,
        color        = "white",
        weight       = 1.5,
        highlight    = highlightOptions(
          weight = 3, color = "#1a3a5c", fillOpacity = 0.9, bringToFront = TRUE
        ),
        label        = ~paste0(province, ": ", round(CGI, 3)),
        labelOptions = labelOptions(style = list("font-size" = "13px"))
      ) %>%
      addLegend(
        pal       = pal,
        values    = map_data$CGI,
        title     = paste0("CGI Score (", sel_year(), ")<br><small style='font-weight:normal'>0 = low gap &bull; 1 = critical</small>"),
        position  = "bottomright",
        labFormat = labelFormat(digits = 2)
      ) %>%
      addControl(unc_legend_html, position = "topleft")

    .add_unc_markers(proxy, prov_uncertainty(), prov_centroids)
  }, ignoreInit = TRUE)

  observeEvent(input$choropleth_shape_click, {
    prov <- input$choropleth_shape_click$id
    if (!is.null(prov) && prov %in% cgi_rollup$province) {
      rv$province <- prov
      rv$view     <- "pillar"
    }
  })

  output$prov_ranking <- renderUI({
    df <- prov_summary() %>% arrange(desc(CGI)) %>% mutate(rank = row_number())
    lapply(seq_len(nrow(df)), function(i) {
      r <- df[i, ]
      div(
        style = paste0(
          "display:flex; justify-content:space-between; align-items:center;",
          "padding:8px 10px; margin-bottom:4px; border-radius:6px;",
          "background:", ifelse(i %% 2 == 0, "#f0f0f0", "#fff"), ";",
          "cursor:pointer;"
        ),
        onclick = paste0("Shiny.setInputValue('click_province','",
                         r$province, "',{priority:'event'})"),
        span(paste0(i, ". ", r$province), style = "font-weight:500;"),
        span(class = "score-badge",
             style = paste0("background:", map_pal()(r$CGI)),
             round(r$CGI, 3))
      )
    })
  })

  observeEvent(input$click_province, {
    rv$province <- input$click_province
    rv$view     <- "pillar"
  })

  # ════════════════════════════════════════════════════════════════════════════
  # VIEW 2: PILLAR
  # ════════════════════════════════════════════════════════════════════════════
  pillar_ui <- function() {
    cats <- sort(unique(cgi_detail$category))
    cat_choices <- c("All specialisms" = "All", setNames(cats, cats))

    tagList(
      actionButton("back_to_map", "< Back to Map",
                   class = "btn-outline-secondary back-btn"),
      fluidRow(
        column(4,
          card(
            card_header(paste("Province:", rv$province, "|", sel_year())),
            uiOutput("province_score_summary"),
            hr(),
            selectInput("sel_category", "Filter by specialism:",
                        choices = cat_choices, selected = "All")
          )
        ),
        column(8,
          card(
            card_header("Pillar Breakdown — click a pillar to inspect indicators"),
            plotlyOutput("pillar_plot", height = "360px"),
            uiOutput("pillar_cards")
          )
        )
      ),
      fluidRow(
        column(12,
          card(
            card_header("CGI by specialism — all pillars"),
            plotlyOutput("specialism_heatmap", height = "620px")
          )
        )
      ),
      fluidRow(
        column(12,
          card(
            card_header(
              "Monte Carlo Uncertainty — CGI 90% Prediction Interval"
            ),
            p(style = "color:#666; font-size:0.85rem; padding:4px 16px 0;",
              paste0("Based on 1,000 MC draws varying pillar weights (Dirichlet \u03b1=1),",
                     " productivity bridge \u03c6 (LogNormal \u00b10.15), and contact",
                     " rates \u03c8 (\u00b110%). Province-level aggregation using DBC",
                     " volume weights. Error bars show 90% PI (5th\u201395th percentile).")),
            plotlyOutput("uncertainty_plot", height = "260px")
          )
        )
      )
    )
  }

  observeEvent(input$back_to_map, {
    rv$view <- "map"; rv$province <- NULL; rv$pillar <- NULL; rv$indicator <- NULL
  })

  output$province_score_summary <- renderUI({
    ps      <- pillar_scores()
    cgi_val <- ps$CGI
    dp_val  <- ps$DP
    sp_val  <- ps$SP
    as_val  <- ps$AS

    pu      <- prov_uncertainty() %>% filter(province == rv$province)
    tier    <- if (nrow(pu) > 0) pu$uncertainty_tier[1] else NULL
    tier_color <- c(Low = "#4dac26", Medium = "#fc8d59", High = "#d73027")
    tier_col   <- if (!is.null(tier) && tier %in% names(tier_color))
                    tier_color[[tier]] else "#999"

    tagList(
      div(style = "text-align:center; margin:10px 0;",
        div(style = paste0("font-size:2.5rem; font-weight:700; color:",
                           score_color(pmin(cgi_val, 1))),
            round(cgi_val, 3)),
        div(style = "font-size:0.85rem; color:#555;",
            paste("CGI Score (", sel_year(), ")")),
        span(class = "score-badge",
             style = paste0("background:", score_color(pmin(cgi_val, 1))),
             score_label(pmin(cgi_val, 1))),
        if (!is.null(tier)) {
          div(style = "margin-top:8px;",
            tags$span(
              style = paste0("display:inline-flex;align-items:center;gap:5px;",
                             "font-size:0.78rem;color:#555;"),
              tags$span(
                style = paste0("display:inline-block;width:0;height:0;",
                               "border-left:6px solid transparent;",
                               "border-right:6px solid transparent;",
                               "border-bottom:10px solid ", tier_col, ";"),
              ),
              paste(tier, "uncertainty")
            )
          )
        }
      ),
      hr(),
      div(style = "font-size:0.85rem;",
        div(style = "display:flex; justify-content:space-between; margin:4px 0;",
          span("Demand Pressure:"),
          span(class = "score-badge",
               style = paste0("background:", score_color(pmin(dp_val,1)),
                              ";font-size:0.75rem"),
               round(dp_val, 3))),
        div(style = "display:flex; justify-content:space-between; margin:4px 0;",
          span("Supply Pressure:"),
          span(class = "score-badge",
               style = paste0("background:", score_color(pmin(sp_val,1)),
                              ";font-size:0.75rem"),
               round(sp_val, 3))),
        div(style = "display:flex; justify-content:space-between; margin:4px 0;",
          span("Access Stress:"),
          span(class = "score-badge",
               style = paste0("background:", score_color(pmin(as_val,1)),
                              ";font-size:0.75rem"),
               round(as_val, 3)))
      )
    )
  })

  output$pillar_plot <- renderPlotly({
    req(rv$view == "pillar", !is.null(rv$province))
    ps  <- pillar_scores()
    nat <- nat_avg()

    vals <- tibble(
      pillar = c("Demand\nPressure", "Supply\nPressure", "Access\nStress"),
      code   = c("DP", "SP", "AS"),
      value  = c(ps$DP, ps$SP, ps$AS),
      nat    = c(nat$DP, nat$SP, nat$AS)
    )

    y_max <- max(c(vals$value, vals$nat), na.rm = TRUE) * 1.08

    p <- ggplot(vals, aes(x = pillar)) +
      geom_col(aes(y = value, fill = code,
                   text = paste0(pillar,
                                 "\nScore: ", round(value, 3),
                                 "\nNational avg: ", round(nat, 3),
                                 "\nClick to drill down")),
               width = 0.5, alpha = 0.9) +
      geom_point(aes(y = nat), shape = 21, size = 4,
                 fill = "white", colour = "#333", stroke = 1.5) +
      geom_hline(yintercept = CGI_THRESH_HIGH, linetype = "dashed",
                 colour = "#d73027", alpha = 0.6) +
      scale_fill_manual(
        values = c(DP = "#e66101", SP = "#5e3c99", AS = "#1a9641"),
        guide  = "none"
      ) +
      scale_y_continuous(
        limits = c(0, max(y_max, 1.05)),
        labels = number_format(accuracy = 0.01)
      ) +
      labs(x = NULL, y = "Normalised score (values >1 valid at 2030/2035)",
           caption = paste0("White dot = national average | dashed = hotspot threshold (",
                            round(CGI_THRESH_HIGH, 2), " = 2025 empirical max)")) +
      theme_minimal(base_size = 13) +
      theme(panel.grid.minor = element_blank())

    ggplotly(p, tooltip = "text", source = "pillar_src") %>%
      event_register("plotly_click") %>%
      layout(
        clickmode  = "event+select",
        uirevision = paste(rv$province, sel_year(), input$sel_category %||% "All")
      ) %>%
      config(doubleClick = "reset", displayModeBar = TRUE)
  })

  observeEvent(event_data("plotly_click", source = "pillar_src"), {
    req(rv$view == "pillar")
    click <- event_data("plotly_click", source = "pillar_src")
    if (!is.null(click)) {
      pillar_map <- c("Demand\nPressure" = "DP",
                      "Supply\nPressure" = "SP",
                      "Access\nStress"   = "AS")
      matched <- pillar_map[as.character(click$x)]
      if (!is.na(matched)) { rv$pillar <- matched; rv$view <- "indicator" }
    }
  })

  output$pillar_cards <- renderUI({
    ps <- pillar_scores()
    pillars <- list(
      list(code = "DP", label = "Demand Pressure",
           desc = "Population need, ageing & growth",  val = ps$DP),
      list(code = "SP", label = "Supply Pressure",
           desc = "Shortages, retirement & FTE gaps",  val = ps$SP),
      list(code = "AS", label = "Access Stress",
           desc = "Wait times, breaches & density",    val = ps$AS)
    )
    fluidRow(lapply(pillars, function(pl) {
      col_hex <- c(DP = "#e66101", SP = "#5e3c99", AS = "#1a9641")[[pl$code]]
      column(4,
        div(class = "pillar-card",
            style = paste0("border:2px solid ", col_hex,
                           "; background:", col_hex, "11;"),
            onclick = paste0("Shiny.setInputValue('click_pillar','",
                             pl$code, "',{priority:'event'})"),
            div(style = paste0("font-size:1.4rem; font-weight:700; color:", col_hex),
                round(pl$val, 3)),
            div(style = "font-weight:600; margin:4px 0;", pl$label),
            div(style = "font-size:0.8rem; color:#666;", pl$desc),
            br(),
            span(class = "score-badge",
                 style = paste0("background:",
                                score_color(pmin(pl$val, 1)), ";font-size:0.75rem"),
                 score_label(pmin(pl$val, 1)))
        )
      )
    }))
  })

  observeEvent(input$click_pillar, {
    rv$pillar <- input$click_pillar; rv$view <- "indicator"
  })

  output$specialism_heatmap <- renderPlotly({
    req(rv$view == "pillar", !is.null(rv$province))
    d_wide <- detail_yr() %>%
      filter(province == rv$province) %>%
      select(category, DP, SP, AS, CGI)

    col_names  <- c("DP", "SP", "AS", "CGI")
    col_labels <- c("Demand Pressure", "Supply Pressure", "Access Stress", "CGI")

    # Descending CGI → plotly heatmap puts y[1] at top natively, so highest
    # CGI appears at the top without needing autorange="reversed" (which breaks
    # doubleClick reset by overwriting the reversed state with autorange=true)
    cat_order <- d_wide %>% arrange(desc(CGI)) %>% pull(category)
    d_ord     <- d_wide %>% arrange(match(category, cat_order))

    z_raw    <- as.matrix(d_ord[, col_names])
    z_capped <- pmin(z_raw, 1.5)

    # Build hover text matrix aligned with z[row, col]
    hover <- matrix("", nrow = nrow(z_raw), ncol = ncol(z_raw))
    for (i in seq_len(nrow(z_raw))) {
      for (j in seq_len(ncol(z_raw))) {
        hover[i, j] <- paste0(d_ord$category[i], " \u2014 ",
                               col_labels[j], ": ", round(z_raw[i, j], 3))
      }
    }

    plot_ly(
      x         = col_labels,
      y         = cat_order,
      z         = z_capped,
      type      = "heatmap",
      colorscale = list(
        list(0.0,        "#4dac26"),
        list(1 / 3,      "#fee08b"),
        list(1.0,        "#d73027")
      ),
      zmin      = 0,
      zmax      = 1.5,
      text      = hover,
      hoverinfo = "text",
      colorbar  = list(
        title     = "Score\n(\u22641.5)",
        titlefont = list(size = 12),
        len       = 0.6
      )
    ) %>%
      layout(
        xaxis      = list(title = "", tickfont = list(size = 12, color = "#333"),
                          side = "bottom"),
        yaxis      = list(title = "", tickfont = list(size = 10)),
        margin     = list(l = 230, t = 10, b = 80, r = 100),
        uirevision = paste(rv$province, sel_year())
      ) %>%
      config(doubleClick = "reset", scrollZoom = FALSE, displayModeBar = TRUE)
  })

  output$uncertainty_plot <- renderPlotly({
    req(rv$view == "pillar", !is.null(rv$province))
    ps <- pillar_scores()
    pu <- prov_uncertainty() %>% filter(province == rv$province)

    tier       <- if (nrow(pu) > 0) pu$uncertainty_tier[1] else "Unknown"
    tier_colors <- c(Low = "#4dac26", Medium = "#fc8d59", High = "#d73027", Unknown = "#999")
    tier_col   <- tier_colors[[tier]]

    cgi_lo <- if (nrow(pu) > 0) pu$CGI_lo90[1] else NA_real_
    cgi_hi <- if (nrow(pu) > 0) pu$CGI_hi90[1] else NA_real_

    metrics <- tibble(
      label  = c("CGI (composite)", "Demand Pressure", "Supply Pressure", "Access Stress"),
      value  = c(ps$CGI, ps$DP, ps$SP, ps$AS),
      bar_color = c(tier_col, "#e66101", "#5e3c99", "#1a9641"),
      err_hi = c(cgi_hi - ps$CGI, NA, NA, NA),
      err_lo = c(ps$CGI - cgi_lo, NA, NA, NA),
      hover  = c(
        paste0("CGI: ", round(ps$CGI, 3),
               if (nrow(pu) > 0)
                 paste0("\n90% PI: [", round(cgi_lo, 3), ", ", round(cgi_hi, 3), "]",
                        "\nPI width: ",  round(pu$PI_width[1], 3),
                        "\nUncertainty: ", tier)
               else "\n(no MC data)"),
        paste0("Demand Pressure: ", round(ps$DP, 3)),
        paste0("Supply Pressure: ", round(ps$SP, 3)),
        paste0("Access Stress: ",   round(ps$AS, 3))
      )
    )

    x_max <- max(c(metrics$value + replace(metrics$err_hi, is.na(metrics$err_hi), 0),
                   CGI_THRESH_HIGH), na.rm = TRUE) * 1.12

    plot_ly(
      data        = metrics,
      x           = ~value,
      y           = ~label,
      type        = "bar",
      orientation = "h",
      marker      = list(color = ~bar_color, line = list(color = "white", width = 1)),
      error_x     = list(
        type       = "data",
        symmetric  = FALSE,
        array      = ~err_hi,
        arrayminus = ~err_lo,
        color      = "#333",
        thickness  = 2,
        width      = 8
      ),
      text      = ~hover,
      hoverinfo = "text",
      showlegend = FALSE
    ) %>%
      layout(
        shapes = list(list(
          type = "line",
          x0 = CGI_THRESH_HIGH, x1 = CGI_THRESH_HIGH,
          y0 = -0.5, y1 = 3.5,
          line = list(color = "#d73027", dash = "dash", width = 1.5)
        )),
        annotations = list(
          list(x = CGI_THRESH_HIGH, y = 3.5, xref = "x", yref = "y",
               text = paste0("Hotspot<br>", round(CGI_THRESH_HIGH, 2)),
               showarrow = FALSE, font = list(size = 10, color = "#d73027"),
               xanchor = "left", yanchor = "bottom"),
          list(x = 0.01, y = 1.07, xref = "paper", yref = "paper",
               text = paste0("<b>Uncertainty tier: <span style='color:", tier_col,
                             "'>", tier, "</span></b>",
                             if (nrow(pu) > 0)
                               paste0(" &nbsp;|&nbsp; 90% PI: [",
                                      round(cgi_lo, 3), ", ", round(cgi_hi, 3),
                                      "]  width: ", round(pu$PI_width[1], 3))
                             else ""),
               showarrow = FALSE, font = list(size = 12), align = "left")
        ),
        xaxis = list(title = "Normalised score", range = c(0, x_max), zeroline = FALSE),
        yaxis = list(title = "",
                     categoryorder = "array",
                     categoryarray = rev(metrics$label)),
        margin    = list(l = 140, r = 20, t = 44, b = 50),
        uirevision = paste(rv$province, sel_year(), input$sel_category %||% "All")
      ) %>%
      config(displayModeBar = FALSE)
  })

  # ════════════════════════════════════════════════════════════════════════════
  # VIEW 3: INDICATOR
  # ════════════════════════════════════════════════════════════════════════════
  indicator_ui <- function() {
    pl_label <- ind_meta$pillar_label[ind_meta$pillar == rv$pillar][1]
    tagList(
      actionButton("back_to_pillar",
                   paste("< Back to", rv$province, "pillars"),
                   class = "btn-outline-secondary back-btn"),
      fluidRow(
        column(12,
          card(
            card_header(paste(pl_label, "— Indicator Breakdown for",
                              rv$province, "|", sel_year())),
            p(style = "color:#666; font-size:0.9rem; margin-bottom:12px;",
              "Normalised 0–1 against 2025 cross-sectional range.
               Values >1 (common at 2030/2035) signal pressure beyond worst-case 2025."),
            plotlyOutput("indicator_plot", height = "360px")
          )
        )
      ),
      fluidRow(
        column(12,
          card(
            card_header("Click an indicator for root-cause detail"),
            uiOutput("indicator_cards")
          )
        )
      )
    )
  }

  observeEvent(input$back_to_pillar, {
    rv$view <- "pillar"; rv$indicator <- NULL
  })

  output$indicator_plot <- renderPlotly({
    req(rv$view == "indicator", !is.null(rv$province), !is.null(rv$pillar))
    d    <- filtered_data()
    inds <- ind_meta %>% filter(pillar == rv$pillar)
    nat  <- nat_avg()

    prov_vals <- d %>%
      select(all_of(inds$col)) %>%
      summarise(across(everything(), ~ mean(.x, na.rm = TRUE))) %>%
      pivot_longer(everything(), names_to = "col", values_to = "value")

    nat_vals <- nat %>%
      select(all_of(inds$col)) %>%
      pivot_longer(everything(), names_to = "col", values_to = "nat_value")

    plot_df <- prov_vals %>%
      left_join(nat_vals, by = "col") %>%
      left_join(inds %>% select(col, label), by = "col") %>%
      mutate(delta_pct = round(
        (value - nat_value) / pmax(nat_value, 0.001) * 100, 1))

    y_max <- max(c(plot_df$value, plot_df$nat_value), na.rm = TRUE) * 1.08

    p <- ggplot(plot_df, aes(x = reorder(label, value))) +
      geom_col(aes(y = value, fill = pmin(value, 1),
                   text = paste0(label,
                                 "\nProvince: ", round(value, 3),
                                 "\nNational avg: ", round(nat_value, 3),
                                 "\nDelta: ",
                                 ifelse(delta_pct >= 0, "+", ""), delta_pct, "%")),
               width = 0.55, alpha = 0.9) +
      geom_point(aes(y = nat_value), shape = 21, size = 4,
                 fill = "white", colour = "#333", stroke = 1.5) +
      scale_fill_gradient2(low = "#4dac26", mid = "#fee08b", high = "#d73027",
                           midpoint = 0.5, limits = c(0, 1),
                           na.value = "#999", guide = "none") +
      geom_hline(yintercept = CGI_THRESH_HIGH, linetype = "dashed",
                 colour = "#d73027", alpha = 0.5) +
      coord_flip() +
      scale_y_continuous(limits = c(0, max(y_max, 1.05)),
                         labels = number_format(accuracy = 0.01)) +
      labs(x = NULL, y = "Normalised score",
           caption = "White dot = national average") +
      theme_minimal(base_size = 13) +
      theme(panel.grid.minor = element_blank())

    ggplotly(p, tooltip = "text") %>%
      layout(uirevision = paste(rv$province, rv$pillar, sel_year(),
                                input$sel_category %||% "All")) %>%
      config(doubleClick = "reset", displayModeBar = TRUE)
  })

  output$indicator_cards <- renderUI({
    d    <- filtered_data()
    nat  <- nat_avg()
    inds <- ind_meta %>% filter(pillar == rv$pillar)

    lapply(seq_len(nrow(inds)), function(i) {
      ind     <- inds[i, ]
      val     <- mean(d[[ind$col]], na.rm = TRUE)
      nat_val <- nat[[ind$col]]
      delta   <- (val - nat_val) / max(nat_val, 0.001) * 100

      div(class = "metric-card",
          style = paste0("border-left-color:", score_color(pmin(val, 1)), ";"),
          onclick = paste0("Shiny.setInputValue('click_indicator','",
                           ind$col, "',{priority:'event'})"),
          div(style = "display:flex; justify-content:space-between; align-items:center;",
            div(
              div(style = "font-weight:600; font-size:1rem;", ind$label),
              div(style = "font-size:0.8rem; color:#666; margin-top:2px;",
                  paste0("vs national avg: ",
                         ifelse(delta >= 0, "+", ""), round(delta, 1), "%"))
            ),
            div(
              span(class = "score-badge",
                   style = paste0("background:", score_color(pmin(val, 1))),
                   round(val, 3)),
              div(style = "font-size:0.75rem; color:#666; text-align:right; margin-top:3px;",
                  score_label(pmin(val, 1)))
            )
          )
      )
    })
  })

  observeEvent(input$click_indicator, {
    rv$indicator <- input$click_indicator; rv$view <- "rootcause"
  })

  # ════════════════════════════════════════════════════════════════════════════
  # VIEW 4: ROOT CAUSE
  # ════════════════════════════════════════════════════════════════════════════
  rootcause_ui <- function() {
    ind_row  <- ind_meta   %>% filter(col == rv$indicator)
    det_row  <- ind_detail %>% filter(col == rv$indicator)

    tagList(
      actionButton("back_to_indicator",
                   paste("< Back to", ind_row$pillar_label[1], "indicators"),
                   class = "btn-outline-secondary back-btn"),
      fluidRow(
        column(6,
          card(
            card_header(paste(ind_row$label[1], "—",
                              rv$province, "|", sel_year())),
            uiOutput("rootcause_summary"),
            hr(),
            div(style = "font-size:0.85rem;",
              div(style = "font-weight:600; margin-bottom:4px;", "Formula:"),
              code(det_row$formula[1]),
              div(style = "font-weight:600; margin:10px 0 4px;",
                  "What it measures:"),
              p(det_row$interpretation[1])
            )
          )
        ),
        column(6,
          card(
            card_header("All provinces — ranked"),
            plotlyOutput("rootcause_dist", height = "340px")
          )
        )
      ),
      fluidRow(
        column(12,
          card(
            card_header("Specialism detail — which specialisms drive this score?"),
            plotlyOutput("rootcause_specialism", height = "340px")
          )
        )
      )
    )
  }

  observeEvent(input$back_to_indicator, {
    rv$view <- "indicator"; rv$indicator <- NULL
  })

  output$rootcause_summary <- renderUI({
    d       <- detail_yr() %>% filter(province == rv$province)
    nat     <- nat_avg()
    ind_row <- ind_meta %>% filter(col == rv$indicator)

    val     <- mean(d[[rv$indicator]], na.rm = TRUE)
    nat_val <- nat[[rv$indicator]]
    delta   <- (val - nat_val) / max(nat_val, 0.001) * 100

    direction <- if (val > nat_val) "above" else "below"
    severity  <- if (abs(delta) > 30) "substantially"
                 else if (abs(delta) > 10) "notably"
                 else "slightly"

    worst_cat <- d %>%
      filter(!is.na(.data[[rv$indicator]])) %>%
      arrange(desc(.data[[rv$indicator]])) %>%
      slice(1)

    explanation <- paste0(
      rv$province, " scores ",
      if (val >= 0.66) "HIGH" else if (val >= 0.33) "MODERATE" else "LOW",
      " on ", ind_row$label[1], " in ", sel_year(), ". ",
      "Province average (", round(val, 3), ") is ",
      severity, " ", direction, " the national average (",
      round(nat_val, 3), ") by ", round(abs(delta), 1), "%. ",
      if (nrow(worst_cat) > 0)
        paste0("The most-pressured specialism is ",
               worst_cat$category, " (",
               round(worst_cat[[rv$indicator]], 3), ").")
      else ""
    )

    tagList(
      div(style = "text-align:center; margin:12px 0;",
        div(style = paste0("font-size:3rem; font-weight:700; color:",
                           score_color(pmin(val, 1))),
            round(val, 3)),
        span(class = "score-badge",
             style = paste0("background:", score_color(pmin(val, 1))),
             score_label(pmin(val, 1)))
      ),
      div(class = "rootcause-box", tags$b("Diagnosis: "), explanation)
    )
  })

  output$rootcause_dist <- renderPlotly({
    req(rv$view == "rootcause", !is.null(rv$indicator))
    # Province-level averages of this indicator
    prov_ind <- detail_yr() %>%
      group_by(province) %>%
      summarise(val = mean(.data[[rv$indicator]], na.rm = TRUE), .groups = "drop") %>%
      arrange(val) %>%
      mutate(highlight = province == rv$province,
             color     = score_color(pmin(val, 1)))

    nat_line <- nat_avg()[[rv$indicator]]

    p <- ggplot(prov_ind,
                aes(x = reorder(province, val), y = val,
                    fill = color, alpha = highlight,
                    text = paste0(province, ": ", round(val, 3)))) +
      geom_col(width = 0.6) +
      geom_hline(yintercept = nat_line, linetype = "dashed",
                 colour = "#333", linewidth = 0.8) +
      scale_fill_identity() +
      scale_alpha_manual(values = c("TRUE" = 1, "FALSE" = 0.6), guide = "none") +
      coord_flip() +
      scale_y_continuous(labels = number_format(accuracy = 0.01)) +
      labs(x = NULL, y = "Normalised score",
           caption = "Dashed = national average") +
      theme_minimal(base_size = 12) +
      theme(panel.grid.minor = element_blank())

    ggplotly(p, tooltip = "text") %>%
      layout(showlegend = FALSE,
             uirevision = paste(rv$indicator, sel_year())) %>%
      config(doubleClick = "reset", displayModeBar = TRUE)
  })

  # ── Cell-level heatmap (all provinces × specialisms) ────────────────────────
  output$cell_heatmap <- renderPlotly({
    req(rv$view == "map")
    d <- detail_yr() %>% select(province, category, CGI)

    # Specialisms: highest mean CGI at top
    cat_order <- d %>%
      group_by(category) %>%
      summarise(m = mean(CGI, na.rm = TRUE), .groups = "drop") %>%
      arrange(m) %>% pull(category)

    # Provinces: highest mean CGI on left
    prov_order <- d %>%
      group_by(province) %>%
      summarise(m = mean(CGI, na.rm = TRUE), .groups = "drop") %>%
      arrange(desc(m)) %>% pull(province)

    # Build z matrix (rows = specialisms, cols = provinces)
    z_mat <- matrix(NA_real_, nrow = length(cat_order), ncol = length(prov_order))
    for (i in seq_along(cat_order))
      for (j in seq_along(prov_order)) {
        v <- d$CGI[d$category == cat_order[i] & d$province == prov_order[j]]
        if (length(v)) z_mat[i, j] <- v[1]
      }

    # Hover text
    hover <- matrix("", nrow = nrow(z_mat), ncol = ncol(z_mat))
    for (i in seq_along(cat_order))
      for (j in seq_along(prov_order)) {
        v   <- z_mat[i, j]
        hot <- !is.na(v) && v >= CGI_THRESH_HIGH
        hover[i, j] <- paste0(
          prov_order[j], " \u00d7 ", cat_order[i],
          "\nCGI: ", ifelse(is.na(v), "NA", round(v, 3)),
          if (hot) "\n\u26a0 HOTSPOT" else ""
        )
      }

    plot_ly(
      x         = prov_order,
      y         = cat_order,
      z         = z_mat,
      type      = "heatmap",
      source    = "cell_heatmap_src",
      colorscale = list(
        list(0,    "#4dac26"),
        list(0.33, "#fee08b"),
        list(0.66, "#fc8d59"),
        list(1,    "#d73027")
      ),
      zmin      = 0,
      zmax      = 1,
      text      = hover,
      hoverinfo = "text",
      colorbar  = list(
        title    = "CGI",
        tickvals = c(0, CGI_THRESH_LOW, CGI_THRESH_HIGH, 1),
        ticktext = c("0\nLow",
                     paste0(round(CGI_THRESH_LOW, 2), "\nModerate"),
                     paste0(round(CGI_THRESH_HIGH, 2), "\nHigh"),
                     "1\nCritical"),
        len      = 0.6
      )
    ) %>%
      layout(
        xaxis = list(title = "", tickangle = -40,
                     tickfont = list(size = 11), side = "bottom"),
        yaxis = list(title = "", tickfont = list(size = 10)),
        margin     = list(l = 220, t = 20, b = 120, r = 110),
        uirevision = sel_year()
      ) %>%
      config(doubleClick = "reset", scrollZoom = FALSE, displayModeBar = TRUE) %>%
      event_register("plotly_click")
  })

  observeEvent(event_data("plotly_click", source = "cell_heatmap_src"), {
    click <- event_data("plotly_click", source = "cell_heatmap_src")
    if (!is.null(click$x)) {
      prov <- as.character(click$x)
      if (prov %in% cgi_rollup$province) {
        rv$province <- prov
        rv$view     <- "pillar"
      }
    }
  })

  output$rootcause_specialism <- renderPlotly({
    req(rv$view == "rootcause", !is.null(rv$province), !is.null(rv$indicator))
    d <- detail_yr() %>%
      filter(province == rv$province, !is.na(.data[[rv$indicator]]))

    nat_spec <- detail_yr() %>%
      group_by(category) %>%
      summarise(nat_val = mean(.data[[rv$indicator]], na.rm = TRUE), .groups = "drop")

    plot_df <- d %>%
      left_join(nat_spec, by = "category") %>%
      mutate(delta = .data[[rv$indicator]] - nat_val)

    y_max <- max(c(plot_df[[rv$indicator]], plot_df$nat_val), na.rm = TRUE) * 1.08

    p <- ggplot(plot_df,
                aes(x = reorder(category, .data[[rv$indicator]]),
                    y = .data[[rv$indicator]],
                    fill = pmin(.data[[rv$indicator]], 1),
                    text = paste0(category,
                                  "\nScore: ", round(.data[[rv$indicator]], 3),
                                  "\nNational avg: ", round(nat_val, 3),
                                  "\nDelta: ",
                                  ifelse(delta >= 0, "+", ""), round(delta, 3)))) +
      geom_col(width = 0.6, alpha = 0.9) +
      geom_point(aes(y = nat_val), shape = 21, size = 3,
                 fill = "white", colour = "#333", stroke = 1.2) +
      scale_fill_gradient2(low = "#4dac26", mid = "#fee08b", high = "#d73027",
                           midpoint = 0.5, limits = c(0, 1),
                           na.value = "#999", guide = "none") +
      coord_flip() +
      scale_y_continuous(limits = c(0, max(y_max, 1.05)),
                         labels = number_format(accuracy = 0.01)) +
      labs(x = NULL, y = "Normalised score",
           caption = "White dot = national average for that specialism") +
      theme_minimal(base_size = 11) +
      theme(panel.grid.minor = element_blank())

    ggplotly(p, tooltip = "text") %>%
      layout(uirevision = paste(rv$province, rv$indicator, sel_year())) %>%
      config(doubleClick = "reset", displayModeBar = TRUE)
  })
}

shinyApp(ui, server)
