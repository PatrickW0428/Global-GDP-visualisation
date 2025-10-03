library(shiny)
library(leaflet)
library(threejs)
library(plotly)
library(ggplot2)
library(rnaturalearth)
library(dplyr)
library(tidyr)
library(bslib)

theme <- bs_theme(
  version   = 5,
  bootswatch= "flatly",
  base_font = font_google("Inter"),
  heading_font = font_google("Inter")
)

sticky_css <- tags$style(HTML("
  .sticky-slider { position: sticky; top: 64px; z-index: 1020; background: #ffffffcc; backdrop-filter: blur(2px); }
  .subnote { font-size:12px; color:#6c757d; margin: -6px 0 10px; }
"))
page_controls_css <- tags$style(HTML("
  .page-controls{
    display:flex;
    align-items:center;
    justify-content:flex-end;
    gap:8px;
  }
  .page-controls .btn{ margin:0; }
  .page-controls .form-select{ width:auto; }
"))
data_path <- "data"
country_info <- read.csv(file.path(data_path, "WDICountry.csv"), stringsAsFactors = FALSE)
if (!"Country.Name" %in% names(country_info)) {
  cand <- c("ShortName", "Short.Name", "TableName", "Table.Name")
  hit  <- cand[cand %in% names(country_info)]
  if (length(hit)) {
    country_info$Country.Name <- country_info[[hit[1]]]
  } else {
    stop("WDICountry.csv is missing ShortName/Short.Name/TableName/Table.Name to build Country.Name.")
  }
}
aggregate_codes <- country_info$Country.Code[country_info$Region == "Aggregates"]
country_codes   <- country_info$Country.Code[country_info$Region != "Aggregates"]
wdi_data <- read.csv(file.path(data_path, "WDICSV.csv"), stringsAsFactors = FALSE, check.names = FALSE)
normalize_wdi_names <- function(df) {
  rn <- names(df)
  repl <- c(
    "Country.Code"   = "Country Code",
    "Country.Name"   = "Country Name",
    "Indicator.Name" = "Indicator Name",
    "Indicator.Code" = "Indicator Code"
  )
  for (k in names(repl)) {
    if (k %in% rn && !(repl[[k]] %in% rn)) {
      names(df)[names(df) == k] <- repl[[k]]
    }
  }
  df
}
wdi_data <- normalize_wdi_names(wdi_data)
indicators <- c("NY.GDP.MKTP.PP.KD",
                "NY.GDP.PCAP.PP.KD",
                "NY.GDP.MKTP.KD.ZG")
wdi_sub <- wdi_data[wdi_data$`Indicator Code` %in% indicators, ]
gdp_df    <- wdi_sub[wdi_sub$`Indicator Code` == "NY.GDP.MKTP.PP.KD", ]
gdppc_df  <- wdi_sub[wdi_sub$`Indicator Code` == "NY.GDP.PCAP.PP.KD", ]
growth_df <- wdi_sub[wdi_sub$`Indicator Code` == "NY.GDP.MKTP.KD.ZG", ]
gdp_df    <- gdp_df[gdp_df$`Country Code` %in% country_codes, ]
gdppc_df  <- gdppc_df[gdppc_df$`Country Code` %in% country_codes, ]
growth_df <- growth_df[growth_df$`Country Code` %in% country_codes, ]
gdppc_df <- merge(gdppc_df, country_info[, c("Country.Code", "Region")],
                  by.x = "Country Code", by.y = "Country.Code", all.x = TRUE)
year_columns <- names(gdp_df)[grepl("^[0-9]{4}$", names(gdp_df))]
year_columns <- as.numeric(year_columns)
latest_year  <- max(year_columns, na.rm = TRUE)
start_year   <- latest_year - 10
world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")
world$Country.Code <- world$iso_a3
world_centroids    <- sf::st_centroid(world)
centroid_coords    <- sf::st_coordinates(world_centroids)
coords_df <- data.frame(
  Country.Code = world$Country.Code,
  lon = centroid_coords[, "X"],
  lat = centroid_coords[, "Y"],
  stringsAsFactors = FALSE
)
have_ind <- unique(wdi_data$`Indicator Code`)
pick <- function(...) {
  xs <- c(...); ys <- xs[xs %in% have_ind]
  if (length(ys)) ys[1] else NA_character_
}
sector_codes <- list(
  ZS = c(
    Agr = pick("NV.AGR.TOTL.ZS"),
    Ind = pick("NV.IND.TOTL.ZS"),
    Srv = pick("NV.SRV.TETC.ZS", "NV.SRV.TOTL.ZS")
  ),
  CD = c(
    Agr = pick("NV.AGR.TOTL.CD"),
    Ind = pick("NV.IND.TOTL.CD"),
    Srv = pick("NV.SRV.TETC.CD", "NV.SRV.TOTL.CD")
  ),
  KD = c(
    Agr = pick("NV.AGR.TOTL.KD"),
    Ind = pick("NV.IND.TOTL.KD"),
    Srv = pick("NV.SRV.TETC.KD", "NV.SRV.TOTL.KD")
  )
)
gdp_codes <- c(CD = pick("NY.GDP.MKTP.CD"), KD = pick("NY.GDP.MKTP.KD"))
has_all  <- function(x) all(!is.na(x))
to_long <- function(df, value_name){
  df %>%
    tidyr::pivot_longer(cols = matches("^[0-9]{4}$"),
                        names_to = "Year", values_to = value_name) %>%
    dplyr::mutate(Year = as.integer(Year))
}
if (has_all(sector_codes$ZS)) {
  sec_wide <- wdi_data %>%
    dplyr::filter(`Indicator Code` %in% sector_codes$ZS,
                  `Country Code` %in% country_codes)
  sector_long <- to_long(sec_wide, "Value") %>%
    dplyr::mutate(
      Sector = dplyr::recode(`Indicator Code`,
                             !!!setNames(c("Agriculture","Industry","Services"), sector_codes$ZS)
      ),
      Method = "ZS"
    )
} else if (has_all(sector_codes$CD) && gdp_codes["CD"] %in% have_ind) {
  sec_cd <- wdi_data %>%
    dplyr::filter(`Indicator Code` %in% sector_codes$CD,
                  `Country Code` %in% country_codes) %>%
    to_long("SecUSD") %>%
    dplyr::mutate(
      Sector = dplyr::recode(`Indicator Code`,
                             !!!setNames(c("Agriculture","Industry","Services"), sector_codes$CD)
      )
    )
  gdp_cd <- wdi_data %>%
    dplyr::filter(`Indicator Code` == gdp_codes["CD"],
                  `Country Code` %in% country_codes) %>%
    to_long("GDPUSD") %>%
    dplyr::select(`Country Code`, Year, GDPUSD)
  sector_long <- sec_cd %>%
    dplyr::left_join(gdp_cd, by = c("Country Code","Year")) %>%
    dplyr::mutate(Value = ifelse(is.na(SecUSD) | is.na(GDPUSD) | GDPUSD == 0,
                                 NA_real_, 100 * SecUSD / GDPUSD),
                  Method = "CD/GDP") %>%
    dplyr::select(`Country Code`, Year, Sector, Value)
} else if (has_all(sector_codes$KD) && gdp_codes["KD"] %in% have_ind) {
  sec_kd <- wdi_data %>%
    dplyr::filter(`Indicator Code` %in% sector_codes$KD,
                  `Country Code` %in% country_codes) %>%
    to_long("SecUSD") %>%
    dplyr::mutate(
      Sector = dplyr::recode(`Indicator Code`,
                             !!!setNames(c("Agriculture","Industry","Services"), sector_codes$KD)
      )
    )
  gdp_kd <- wdi_data %>%
    dplyr::filter(`Indicator Code` == gdp_codes["KD"],
                  `Country Code` %in% country_codes) %>%
    to_long("GDPUSD") %>%
    dplyr::select(`Country Code`, Year, GDPUSD)
  sector_long <- sec_kd %>%
    dplyr::left_join(gdp_kd, by = c("Country Code","Year")) %>%
    dplyr::mutate(Value = ifelse(is.na(SecUSD) | is.na(GDPUSD) | GDPUSD == 0,
                                 NA_real_, 100 * SecUSD / GDPUSD),
                  Method = "KD/GDP") %>%
    dplyr::select(`Country Code`, Year, Sector, Value)
} else if (has_all(sector_codes$CD) || has_all(sector_codes$KD)) {
  use <- if (has_all(sector_codes$CD)) "CD" else "KD"
  code_vec <- sector_codes[[use]]
  sec_sum <- wdi_data %>%
    dplyr::filter(`Indicator Code` %in% code_vec,
                  `Country Code` %in% country_codes) %>%
    to_long("Val") %>%
    dplyr::mutate(
      Sector = dplyr::recode(`Indicator Code`,
                             !!!setNames(c("Agriculture","Industry","Services"), code_vec)
      )
    )
  total_df <- sec_sum %>%
    dplyr::group_by(`Country Code`, Year) %>%
    dplyr::summarise(Total = sum(Val, na.rm = TRUE), .groups = "drop")
  sector_long <- sec_sum %>%
    dplyr::left_join(total_df, by = c("Country Code","Year")) %>%
    dplyr::mutate(Value = ifelse(is.na(Val) | Total == 0, NA_real_, 100 * Val / Total),
                  Method = paste0(use, "_shareOf3")) %>%
    dplyr::select(`Country Code`, Year, Sector, Value)
} else {
  stop("No usable sector indicators found:\n- Prefer ZS (NV.AGR.TOTL.ZS / NV.IND.TOTL.ZS / NV.SRV.TETC.ZS)\n- Or sector CD with NY.GDP.MKTP.CD; or sector KD with NY.GDP.MKTP.KD;\n- Or at least three sector totals (CD or KD) to approximate shares.")
}
sector_long <- sector_long %>%
  dplyr::left_join(country_info[, c("Country.Code","Country.Name","Region")],
                   by = c("Country Code" = "Country.Code"))
sector_year_range <- range(sector_long$Year[!is.na(sector_long$Value)], na.rm = TRUE)
if ("Method" %in% names(sector_long)) {
  cat("Sector-share construction method(s):", unique(na.omit(sector_long$Method)), "\n")
}
ui <- navbarPage(
  title = "Global GDP Dashboard",
  id = "main_nav",
  theme = theme,
  header = tagList(sticky_css, page_controls_css),
  tabPanel(
    "GDP Data",
    fluidRow(
      column(3,
             selectInput("regionFilter", "Region:",
                         choices  = c("All", sort(unique(country_info$Region[country_info$Region != "Aggregates"]))),
                         selected = "All")
      ),
      column(5,
             selectizeInput(
               inputId = "countries_gdp",
               label   = "Countries:",
               choices = NULL,
               multiple = TRUE,
               options = list(
                 placeholder = 'Type to search country...',
                 maxOptions  = 1000,
                 create      = FALSE,
                 highlight   = TRUE
               )
             )
      ),
      column(4,
             radioButtons("viewType", "Map View:",
                          choices = c("2D", "3D"), inline = TRUE, selected = "2D")
      )
    ),
    fluidRow(
      column(12, align = "center",
             conditionalPanel("input.viewType == '2D'",
                              leafletOutput("map2d", width = "100%", height = "600px")),
             conditionalPanel("input.viewType == '3D'",
                              globeOutput("map3d",  width = "100%", height = "600px"))
      )
    ),
    fluidRow(
      column(
        12, align = "center",
        div(class = "sticky-slider",
            sliderInput("year", "Year:",
                        min = 1990, max = latest_year, value = latest_year, step = 1,
                        sep = "", animate = TRUE, width = "80%")
        ),
        div(class = "subnote", tags$em("* If data are missing, the chart will be blank."))
      )
    ),
    fluidRow(
      column(12, plotlyOutput("barPlot", height = "350px"))
    )
  ),
  tabPanel(
    "GDP Growth And Sector Share",
    fluidRow(
      column(
        6,
        selectInput("region_growth_filter", "Region for Growth Analysis:",
                    choices  = c("All", sort(unique(country_info$Region[country_info$Region != "Aggregates"]))),
                    selected = "All", width = "100%")
      ),
      column(
        6,
        div(
          class = "page-controls",
          actionButton("prevPage", "Prev", class = "btn btn-outline-secondary"),
          uiOutput("regionPageSelector"),
          actionButton("nextPage", "Next", class = "btn btn-primary")
        )
      )
    ),
    fluidRow(
      column(12, plotlyOutput("regionGrowthPlot", height = "360px"))
    ),
    hr(),
    fluidRow(
      column(
        4,
        selectInput(
          "sector_region",
          "Region (Sector):",
          choices = c("All", sort(unique(country_info$Region[country_info$Region != "Aggregates"]))),
          selected = "All"
        )
      ),
      column(
        4,
        selectizeInput(
          "sector_countries",
          "Country (Sector):",
          choices = NULL,
          multiple = FALSE,
          options = list(
            placeholder = "Type to search country...",
            maxOptions = 1000,
            create = FALSE
          )
        )
      ),
      column(
        4,
        div(class = "sticky-slider",
            sliderInput(
              "sector_years",
              "Years:",
              min   = sector_year_range[1],
              max   = sector_year_range[2],
              value = c(max(sector_year_range[2]-20, sector_year_range[1]), sector_year_range[2]),
              step  = 1, sep = ""
            )
        ),
        div(class = "subnote", tags$em("* If data are missing, the chart will be blank."))
      )
    ),
    fluidRow(
      column(
        6,
        radioButtons(
          "sector_chart",
          "Chart type:",
          choices = c("Stacked area" = "area", "Stacked bar (latest year)" = "bar"),
          inline = TRUE, selected = "area"
        )
      )
    ),
    fluidRow(
      column(12, plotlyOutput("sectorPlot", height = "480px"))
    )
  )
)
server <- function(input, output, session) {
  plt_cfg <- function(fig) {
    fig %>% plotly::config(
      displaylogo = FALSE,
      modeBarButtonsToRemove = c("lasso2d","select2d","toggleSpikelines","autoScale2d"),
      toImageButtonOptions = list(format = "png", filename = "chart_export", scale = 2)
    )
  }
  observeEvent(input$region_growth_filter, {
    updateSelectInput(session, "sector_region", selected = input$region_growth_filter)
    updateSelectInput(session, "regionFilter",   selected = input$region_growth_filter)
  })
  observeEvent(input$sector_region, {
    updateSelectInput(session, "region_growth_filter", selected = input$sector_region)
  })
  observe({
    req(country_info)
    if (is.null(input$regionFilter) || input$regionFilter == "All") {
      choices <- setNames(
        country_info$Country.Code[country_info$Region != "Aggregates"],
        country_info$Country.Name[country_info$Region != "Aggregates"]
      )
    } else {
      choices <- setNames(
        country_info$Country.Code[country_info$Region == input$regionFilter],
        country_info$Country.Name[country_info$Region == input$regionFilter]
      )
    }
    updateSelectizeInput(session, "countries_gdp", choices = choices, server = TRUE)
  })
  selected_country_codes <- reactive({
    if (!is.null(input$countries_gdp) && length(input$countries_gdp) > 0) {
      input$countries_gdp
    } else if (!is.null(input$regionFilter) && input$regionFilter != "All") {
      country_info$Country.Code[country_info$Region == input$regionFilter]
    } else {
      country_info$Country.Code[country_info$Region != "Aggregates"]
    }
  })
  year_gdp <- reactive({
    col_year <- as.character(input$year)
    codes <- selected_country_codes()
    if (!(col_year %in% colnames(gdp_df))) return(data.frame())
    gdp_df %>%
      dplyr::filter(`Country Code` %in% codes) %>%
      dplyr::transmute(
        Country.Code = `Country Code`,
        Country.Name = `Country Name`,
        GDP          = .data[[col_year]]
      )
  })
  year_gdppc <- reactive({
    col_year <- as.character(input$year)
    codes <- selected_country_codes()
    if (!(col_year %in% colnames(gdppc_df))) return(data.frame())
    gdppc_df %>%
      dplyr::filter(`Country Code` %in% codes) %>%
      dplyr::transmute(
        Country.Name = `Country Name`,
        Country.Code = `Country Code`,
        Region       = Region,
        GDPpc        = .data[[col_year]]
      )
  })
  output$map2d <- leaflet::renderLeaflet({
    req(input$viewType == "2D")
    gdp_data <- year_gdp()
    map_sf <- merge(world, gdp_data, by = "Country.Code", all.x = FALSE)
    if (nrow(map_sf) == 0) return(leaflet::leaflet())
    valid_gdp <- sort(na.omit(map_sf$GDP), decreasing = TRUE)
    if (length(valid_gdp) < 3) return(leaflet::leaflet())
    top1 <- valid_gdp[1]
    top2 <- valid_gdp[2]
    max_other <- max(valid_gdp[-c(1,2)])
    custom_breaks <- unique(c(
      0, 1e9, 1e10, 5e10, 2e11, 1e12,
      max_other, top2, top1 * 1.01
    ))
    breaks <- sort(unique(custom_breaks))
    pal <- leaflet::colorBin(
      palette = "YlGnBu",
      domain  = map_sf$GDP,
      bins    = breaks,
      na.color = "#f5f5f5"
    )
    leaflet::leaflet(map_sf, options = leaflet::leafletOptions(minZoom = 2)) %>%
      leaflet::addTiles() %>%
      leaflet::addPolygons(
        color = "#444444", weight = 0.5, smoothFactor = 0.2,
        fillOpacity = 0.85, fillColor = ~pal(GDP),
        layerId = ~Country.Code,
        label = ~paste0(
          Country.Name, ": ",
          ifelse(is.na(GDP), "No Data",
                 paste0("$", formatC(GDP/1e9, format="f", digits=1, big.mark=","), " Billion"))
        ),
        highlight = leaflet::highlightOptions(weight = 1.5, color = "#666",
                                              fillOpacity = 0.95, bringToFront = TRUE)
      ) %>%
      leaflet::addLegend(
        pal = pal, values = ~GDP,
        title = paste("GDP (Billion, 2021$) –", input$year),
        position = "bottomright",
        labFormat = function(type, cuts, p) {
          sapply(seq_along(cuts)[-length(cuts)], function(i) {
            lo <- cuts[i]; hi <- cuts[i+1]
            if (hi >= top1) {
              "Top 1"
            } else if (hi >= top2 && hi < top1) {
              "Top 2"
            } else if (hi >= 1e12) {
              paste0("$", formatC(lo/1e12, digits=1, format="f"),
                     "T ~ $", formatC(hi/1e12, digits=1, format="f"), "T")
            } else if (hi >= 1e9) {
              paste0("$", formatC(lo/1e9, digits=0, format="f"),
                     "B ~ $", formatC(hi/1e9, digits=0, format="f"), "B")
            } else {
              paste0("$", formatC(lo, digits=0, format="f"),
                     " ~ $", formatC(hi, digits=0, format="f"))
            }
          })
        }
      )
  })
  observe({
    gdp_data <- year_gdp()
    map_sf <- merge(world, gdp_data, by = "Country.Code", all.x = FALSE)
    if (nrow(map_sf) == 0) return()
    bbox <- sf::st_bbox(map_sf)
    leaflet::leafletProxy("map2d") %>% leaflet::fitBounds(bbox["xmin"], bbox["ymin"], bbox["xmax"], bbox["ymax"])
  })
  output$map3d <- threejs::renderGlobe({
    req(input$viewType == "3D")
    gdp_data <- year_gdp()
    map_points <- merge(gdp_data, coords_df, by = "Country.Code")
    map_points <- map_points[!is.na(map_points$GDP), ]
    if (nrow(map_points) == 0) return(NULL)
    max_gdp <- max(map_points$GDP, na.rm = TRUE)
    heights <- 100 * map_points$GDP / max_gdp
    pal <- leaflet::colorNumeric("Blues", domain = map_points$GDP)
    colors <- pal(map_points$GDP)
    threejs::globejs(lat = map_points$lat, long = map_points$lon,
                     value = heights, color = colors,
                     atmosphere = TRUE, bg = "black")
  })
  output$barPlot <- plotly::renderPlotly({
    data <- year_gdppc()
    if (nrow(data) == 0) return(plt_cfg(plotly::plotly_empty()))
    if (input$regionFilter == "All") {
      data <- data[order(data$GDPpc, decreasing = TRUE), ]
      data <- head(data, 10)
    } else {
      data <- data[order(data$GDPpc, decreasing = TRUE), ]
    }
    data$Country.Name <- factor(data$Country.Name, levels = data$Country.Name)
    pal <- colorRampPalette(c("#b3c6e7", "#1976d2", "#0d133b"))
    n_bar <- nrow(data)
    bar_colors <- pal(n_bar)[rank(data$GDPpc, ties.method = "first")]
    data$GDPpc_int <- round(data$GDPpc)
    data$hover <- paste0(
      "<b>", data$Country.Name, "</b><br>",
      "GDP per Capita: <b>$", formatC(data$GDPpc_int, format="d", big.mark=","), "</b>"
    )
    fig <- plotly::plot_ly(
      data, x = ~Country.Name, y = ~GDPpc_int, type = 'bar',
      marker = list(color = bar_colors), hoverinfo = "text", text = ~hover
    ) %>%
      plotly::layout(
        title  = list(text = paste("GDP per Capita -", input$year), y = 0.86, font = list(size = 22)),
        xaxis  = list(title = list(text = "Country/Region", font = list(size = 18)),
                      tickangle = -30, tickfont = list(size = 15)),
        yaxis  = list(title = list(text = "GDP per Capita (constant 2021 $)", font = list(size = 18)),
                      rangemode = "tozero", tickformat = ",",
                      gridcolor = "rgba(200,200,220,0.3)", zerolinecolor = "#888",
                      range = c(0, max(data$GDPpc_int, na.rm=TRUE) * 1.15), tickfont = list(size = 15)),
        margin = list(t = 80, b = 100),
        hoverlabel = list(bgcolor = "rgba(40,60,80,0.95)", bordercolor = "#1976d2",
                          font = list(size = 17, color = "white"), align = "left")
      )
    plt_cfg(fig)
  })
  regionPage <- reactiveVal(1)
  regionPageTotal <- reactive({
    region_sel <- input$region_growth_filter
    codes <- if (region_sel == "All")
      country_info$Country.Code[country_info$Region != "Aggregates"]
    else
      country_info$Country.Code[country_info$Region == region_sel]
    total <- nrow(growth_df[growth_df$`Country Code` %in% codes, ])
    page_size <- 5
    max(1, ceiling(total / page_size))
  })
  observe({ updateSelectInput(session, "regionPageInput", selected = regionPage()) })
  observeEvent(input$prevPage, { if (regionPage() > 1) regionPage(regionPage() - 1) })
  observeEvent(input$nextPage, { if (regionPage() < regionPageTotal()) regionPage(regionPage() + 1) })
  observeEvent(input$regionPageInput, { p <- as.numeric(input$regionPageInput); if(!is.na(p)) regionPage(p) })
  observeEvent(input$region_growth_filter, { regionPage(1) })
  output$regionPageSelector <- renderUI({
    selectInput("regionPageInput", NULL,
                choices = setNames(seq_len(regionPageTotal()), paste("Page", seq_len(regionPageTotal()))),
                selected = regionPage(), width = "110px")
  })
  region_growth_data <- reactive({
    codes <- country_codes
    if (input$region_growth_filter != "All") {
      codes <- country_info$Country.Code[country_info$Region == input$region_growth_filter]
    }
    sel_rows <- growth_df[growth_df$`Country Code` %in% codes, ]
    if (nrow(sel_rows) == 0) return(NULL)
    years <- names(sel_rows)[grepl("^[0-9]{4}$", names(sel_rows))]
    years_num <- as.numeric(years)
    recent_years <- tail(years_num, 30)
    avg_growth <- apply(sel_rows[, years %in% as.character(recent_years), drop = FALSE], 1,
                        function(x) mean(as.numeric(x), na.rm = TRUE))
    sel_rows$avg_growth <- avg_growth
    sel_rows <- sel_rows[order(-sel_rows$avg_growth), ]
    page_size <- 5
    total <- nrow(sel_rows)
    total_pages <- ceiling(total / page_size)
    page <- regionPage()
    if (page < 1) page <- 1
    if (page > total_pages) page <- total_pages
    idx <- seq(1 + (page-1)*page_size, min(page*page_size, total))
    sel_rows <- sel_rows[idx, , drop=FALSE]
    sel_rows$legend_name <- sprintf("%s (Avg: %.2f%%)", sel_rows$`Country Name`, sel_rows$avg_growth)
    long_list <- lapply(1:nrow(sel_rows), function(i) {
      country <- sel_rows$legend_name[i]
      code    <- sel_rows$`Country Code`[i]
      year_vals <- sel_rows[i, grepl("^[0-9]{4}$", names(sel_rows))]
      years     <- as.numeric(names(year_vals))
      values    <- as.numeric(year_vals[1, ])
      data.frame(Year = years, Growth = values, Country = country, Code = code, stringsAsFactors = FALSE)
    })
    plot_df <- do.call(rbind, long_list)
    plot_df$Country <- factor(plot_df$Country, levels = sel_rows$legend_name)
    plot_df
  })
  output$regionGrowthPlot <- plotly::renderPlotly({
    plot_data <- region_growth_data()
    if (is.null(plot_data) || nrow(plot_data) == 0) return(plt_cfg(plotly::plotly_empty()))
    nlines <- length(unique(plot_data$Country))
    pal <- RColorBrewer::brewer.pal(max(3, min(8, nlines)), "Set2")
    plotly::plot_ly(
      plot_data, x = ~Year, y = ~Growth, color = ~Country, colors = pal,
      type = 'scatter', mode = 'lines+markers',
      line = list(width = 3, shape = "spline"),
      marker = list(size = 7, symbol = "circle"),
      hoverinfo = "text",
      text = ~paste0(Country, "<br>Year: ", Year, "<br>Growth: ", sprintf("%.2f%%", Growth))
    ) %>%
      plotly::layout(
        title  = paste0("GDP Annual Growth Rate by Country/Region [", input$region_growth_filter, "] (Sorted by Avg Growth)"),
        xaxis  = list(title = "Year"),
        yaxis  = list(title = "Annual Growth (%)"),
        legend = list(title = list(text = "Country/Region (Avg Growth)"), orientation = "v", x = 1.03, y = 1)
      ) %>%
      plt_cfg()
  })
  observeEvent(input$sector_region, {
    if (input$sector_region == "All") {
      ch <- setNames(
        country_info$Country.Code[country_info$Region != "Aggregates"],
        country_info$Country.Name[country_info$Region != "Aggregates"]
      )
    } else {
      idx <- country_info$Region == input$sector_region
      ch <- setNames(country_info$Country.Code[idx], country_info$Country.Name[idx])
    }
    updateSelectizeInput(session, "sector_countries", choices = ch, selected = NULL, server = TRUE)
  }, ignoreInit = TRUE)
  sector_data_filtered <- reactive({
    req(sector_long)
    yr <- input$sector_years
    if (is.null(yr) || length(yr) != 2) yr <- sector_year_range
    chosen <- if (!is.null(input$sector_countries) && length(input$sector_countries) > 0) {
      input$sector_countries
    } else if (!is.null(input$sector_region) && input$sector_region != "All") {
      country_info$Country.Code[country_info$Region == input$sector_region]
    } else {
      country_info$Country.Code[country_info$Region != "Aggregates"]
    }
    sector_long %>%
      dplyr::filter(
        `Country Code` %in% chosen,
        Year >= yr[1], Year <= yr[2],
        !is.na(Value)
      )
  })
  output$sectorPlot <- plotly::renderPlotly({
    dat <- sector_data_filtered()
    if (nrow(dat) == 0) return(plt_cfg(plotly::plotly_empty()))
    if (identical(input$sector_chart, "area")) {
      cntry_code <- if (!is.null(input$sector_countries) && length(input$sector_countries) > 0) {
        input$sector_countries[1]
      } else {
        unique(dat$`Country Code`)[1]
      }
      d1 <- dat %>% dplyr::filter(`Country Code` == cntry_code) %>% dplyr::arrange(Year)
      cname <- ifelse(length(d1$`Country Name`) > 0, d1$`Country Name`[1], "")
      p <- plotly::plot_ly(
        d1, x = ~Year, y = ~Value, color = ~Sector, colors = "Set2",
        type = "scatter", mode = "lines", stackgroup = "one",
        hovertemplate = paste0("%{fullData.name}<br>%{x}: %{y:.1f}%<extra>", cname, "</extra>")
      ) %>%
        plotly::layout(
          title = paste0("Sector shares (% of GDP) — ", cname),
          xaxis = list(title = "Year"),
          yaxis = list(title = "% of GDP", rangemode = "tozero"),
          legend = list(title = list(text = "Sector"))
        )
    } else {
      latest <- max(dat$Year, na.rm = TRUE)
      d2 <- dat %>%
        dplyr::filter(Year == latest) %>%
        dplyr::group_by(`Country Name`, Sector) %>%
        dplyr::summarise(Value = mean(Value, na.rm = TRUE), .groups = "drop")
      p <- plotly::plot_ly(
        d2, x = ~`Country Name`, y = ~Value, color = ~Sector, colors = "Set2",
        type = "bar",
        hovertemplate = "%{x}<br>%{fullData.name}: %{y:.1f}%<extra></extra>"
      ) %>%
        plotly::layout(
          title = paste0("Sector shares (% of GDP) — ", latest),
          barmode = "stack",
          xaxis = list(title = ""),
          yaxis = list(title = "% of GDP", rangemode = "tozero")
        )
    }
    plt_cfg(p)
  })
  output$dl_sector_csv <- downloadHandler(
    filename = function() {
      yr <- if (is.null(input$sector_years)) "" else input$sector_years[2]
      sprintf("sector_%s_%s.csv",
              ifelse(is.null(input$sector_region), "All", input$sector_region), yr)
    },
    content  = function(file) {
      dat <- sector_data_filtered()
      utils::write.csv(dat, file, row.names = FALSE, na = "")
    }
  )
}
shinyApp(ui = ui, server = server)
