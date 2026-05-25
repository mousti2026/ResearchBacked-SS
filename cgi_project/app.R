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

# ── 4. Color helpers ──────────────────────────────────────────────────────────
score_color <- function(x) {
  case_when(
    is.na(x)   ~ "#999999",
    x >= 0.66  ~ "#d73027",
    x >= 0.33  ~ "#fc8d59",
    TRUE        ~ "#4dac26"
  )
}

score_label <- function(x) {
  case_when(
    is.na(x)   ~ "No data",
    x >= 0.66  ~ "High pressure",
    x >= 0.33  ~ "Moderate",
    TRUE        ~ "Low pressure"
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
          div(style = "max-height:520px; overflow-y:auto;",
              uiOutput("prov_ranking"))
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

      cgi_range <- range(ps$CGI, na.rm = TRUE)
      pal <- colorNumeric(
        palette  = c("#4dac26", "#fee08b", "#d73027"),
        domain   = cgi_range,
        na.color = "#999999"
      )

      leaflet(map_data) %>%
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
          pal      = pal,
          values   = ~CGI,
          title    = paste("CGI", sel_year()),
          position = "bottomright",
          labFormat = labelFormat(digits = 2)
        )
    }
  })

  # Re-draw polygons when year changes (without full map reset)
  observeEvent(sel_year(), {
    req(!is.null(prov_sf), rv$view == "map")
    ps  <- prov_summary()
    map_data <- prov_sf %>%
      left_join(ps %>% select(province, CGI), by = "province")

    cgi_range <- range(ps$CGI, na.rm = TRUE)
    pal <- colorNumeric(
      palette  = c("#4dac26", "#fee08b", "#d73027"),
      domain   = cgi_range,
      na.color = "#999999"
    )

    leafletProxy("choropleth") %>%
      clearShapes() %>%
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
        pal      = pal,
        values   = map_data$CGI,
        title    = paste("CGI", sel_year()),
        position = "bottomright",
        labFormat = labelFormat(digits = 2)
      )
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
             style = paste0("background:", score_color(r$CGI)),
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

    tagList(
      div(style = "text-align:center; margin:10px 0;",
        div(style = paste0("font-size:2.5rem; font-weight:700; color:",
                           score_color(pmin(cgi_val, 1))),
            round(cgi_val, 3)),
        div(style = "font-size:0.85rem; color:#555;",
            paste("CGI Score (", sel_year(), ")")),
        span(class = "score-badge",
             style = paste0("background:", score_color(pmin(cgi_val, 1))),
             score_label(pmin(cgi_val, 1)))
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
      geom_hline(yintercept = 0.66, linetype = "dashed",
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
           caption = "White dot = national average | dashed = 0.66 threshold") +
      theme_minimal(base_size = 13) +
      theme(panel.grid.minor = element_blank())

    ggplotly(p, tooltip = "text", source = "pillar_src") %>%
      event_register("plotly_click") %>%
      layout(clickmode = "event+select")
  })

  observeEvent(event_data("plotly_click", source = "pillar_src"), {
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
    # Specialism × pillar matrix — rows ordered by CGI descending
    d_wide <- detail_yr() %>%
      filter(province == rv$province) %>%
      select(category, DP, SP, AS, CGI)

    cat_order <- d_wide %>% arrange(desc(CGI)) %>% pull(category)

    d_long <- d_wide %>%
      pivot_longer(c(DP, SP, AS, CGI), names_to = "metric", values_to = "value") %>%
      mutate(
        metric   = factor(metric, levels = c("DP", "SP", "AS", "CGI")),
        category = factor(category, levels = rev(cat_order)),  # top CGI at top
        val_capped = pmin(value, 1.5)
      )

    p <- ggplot(d_long,
                aes(x = metric, y = category, fill = val_capped,
                    text = paste0(category, " — ", metric,
                                  ": ", round(value, 3)))) +
      geom_tile(colour = "white", linewidth = 0.4) +
      scale_fill_gradient2(
        low = "#4dac26", mid = "#fee08b", high = "#d73027",
        midpoint = 0.5, limits = c(0, 1.5), na.value = "#cccccc",
        name = "Score"
      ) +
      scale_x_discrete(expand = c(0, 0)) +
      scale_y_discrete(expand = c(0, 0)) +
      labs(x = NULL, y = NULL) +
      theme_minimal(base_size = 11) +
      theme(panel.grid = element_blank(),
            axis.text.x = element_text(face = "bold"),
            legend.position = "right")

    ggplotly(p, tooltip = "text", height = 600) %>%
      layout(showlegend = TRUE, margin = list(l = 180))
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
      geom_hline(yintercept = 0.66, linetype = "dashed",
                 colour = "#d73027", alpha = 0.5) +
      coord_flip() +
      scale_y_continuous(limits = c(0, max(y_max, 1.05)),
                         labels = number_format(accuracy = 0.01)) +
      labs(x = NULL, y = "Normalised score",
           caption = "White dot = national average") +
      theme_minimal(base_size = 13) +
      theme(panel.grid.minor = element_blank())

    ggplotly(p, tooltip = "text")
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

    ggplotly(p, tooltip = "text") %>% layout(showlegend = FALSE)
  })

  output$rootcause_specialism <- renderPlotly({
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

    ggplotly(p, tooltip = "text")
  })
}

shinyApp(ui, server)
