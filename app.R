# Community Capitals Index - Shiny app (Vermont counties)

library(shiny)
library(leaflet)
library(maps)

# Setup: capitals, variables (the same for every state)

capital_info <- data.frame(
  capital = c("Financial",
              "Built",
              "Political",
              "Social",
              "Natural",
              "Human",
              "Cultural"),
  method = rep("PCA", 7),
  description = c(
    "Money and financial resources available in the community for investment, savings, and local government.",
    "Physical infrastructure like housing, public services, broadband, hospitals, and businesses.",
    "Ability of a community to access power, influence decisions, and participate in civic life.",
    "Networks, trust, and connections between people and organizations in the community.",
    "Environmental resources and amenities such as land, water, farmland, and conservation areas.",
    "Skills, education, and health of the people living in the community.",
    "Traditions, arts, heritage, and the ways people understand the world."
  ),
  stringsAsFactors = FALSE
)

# methodology notes (shown on cover page)
capital_info$notes <- c(
  "First principal component of the variables (following Schmitt).",
  "Concept draft says EFA/CFA, but many states have few counties, so we use PCA for now.",
  "First principal component of the variables.",
  "First principal component of the variables.",
  "First principal component of the variables.",
  "First principal component of the variables.",
  "First principal component of the variables."
)

var_info <- data.frame(
  capital = c(rep("Financial", 8),
              rep("Built", 8),
              rep("Political", 5),
              rep("Social", 6),
              rep("Natural", 3),
              rep("Human", 7),
              rep("Cultural", 6)),
  variable = c(
    # Financial
    "Population",
    "# Owner Occupied Units",
    "Median household income",
    "# Bank Deposits",
    "Investment Interest",
    "% in Poverty",
    "% With No Vehicle Availability",
    "% Receiving SNAP",
    
    # Built
    "% Population with Access to Parks and Playgrounds",
    "# Food & Beverage Manufacturing Establishments",
    "Nonemployers Sales Value of Shipments or Revenue",
    "# Non-F&B Manufacturing Establishments",
    "% Access to Broadband Coverage",
    "Max Advertised Download Speed",
    "Max Advertised Upload Speed",
    "# Hospitals",
    
    # Political
    "Voter Turnout (%)",
    "Census Response Rate",
    "Median Value of Political Contributions",
    "Total Number of Contributions",
    "Total Value of Political Contributions",
    
    # Social
    "Homeownership rate",
    "Share born in state of residence",
    "Share of households in home since before 2000",
    "Share family households",
    "Membership associations per 1,000 people",
    "Nonprofits per 1,000 people",
    
    # Natural
    "Share of area that is water",
    "Farmland as share of land area",
    "Crop sales per farm acre",
    
    # Human
    "Share with bachelor's degree or higher",
    "Share with high school diploma or higher",
    "Labor force participation rate",
    "Share of civilian labor force employed",
    "Share of population aged 18-64",
    "Share of 16-19 year olds who dropped out",
    "Share uninsured",
    
    # Cultural
    "Racial/ethnic fractionalization",
    "Share international",
    "Share of workers in arts, design, entertainment & media",
    "Share of workers in management, business, science & arts",
    "Museums & historical sites per 10,000 people",
    "Performing arts companies & independent artists per 10,000 people"
  ),
  source = c(
    # Financial
    "Census ACS",
    "Census",
    "Census",
    "FDIC",
    "IRS",
    "SAIPE",
    "FRAC",
    "FRAC",
    
    # Built
    "County Health Rankings",
    "Census",
    "Census",
    "Census",
    "County Health Rankings",
    "Census",
    "Census",
    "Data.CMS.gov",
    
    # Political
    "outside.vermont.gov",
    "County Health Rankings",
    "Campaign Finance System of VT",
    "Campaign Finance System of VT",
    "Campaign Finance System of VT",
    
    # Social
    "Census ACS 2024",
    "Census ACS 2024",
    "Census ACS 2024",
    "Census ACS 2024",
    "Census County Business Patterns 2023",
    "IRS Exempt Organizations Business Master File; Census ZCTA-county file 2020",
    
    # Natural
    "Census county area 2024",
    "USDA NASS Census of Agriculture 2022; Census county area 2024",
    "USDA NASS Census of Agriculture 2022",
    
    # Human
    rep("Census ACS 2024", 7),
    
    # Cultural
    "Census ACS 2024",
    "Census ACS 2024",
    "Census ACS 2024",
    "Census ACS 2024",
    "Census County Business Patterns 2023",
    "Census County Business Patterns 2023"
  ),
  stringsAsFactors = FALSE
)

# How each county_capitals_2024.csv variable was built from the raw files
def_vars <- c(
  "Homeownership rate",
  "Share born in state of residence",
  "Share of households in home since before 2000",
  "Share family households",
  "Membership associations per 1,000 people",
  "Nonprofits per 1,000 people",
  "Share of area that is water",
  "Farmland as share of land area",
  "Crop sales per farm acre",
  "Share with bachelor's degree or higher",
  "Share with high school diploma or higher",
  "Labor force participation rate",
  "Share of civilian labor force employed",
  "Share of population aged 18-64",
  "Share of 16-19 year olds who dropped out",
  "Share uninsured",
  "Racial/ethnic fractionalization",
  "Share international",
  "Share of workers in arts, design, entertainment & media",
  "Share of workers in management, business, science & arts",
  "Museums & historical sites per 10,000 people",
  "Performing arts companies & independent artists per 10,000 people"
)

def_text <- c(
  "Owner-occupied / all occupied housing units",
  "Residents born in their current state / total population",
  "Households that moved in before 2000 / all households",
  "Family households / all households",
  "Establishments in NAICS 813110, 813410, 813910, 813920, 813930, 713910, 713940, 713950, 711211 / population x 1,000",
  "IRS-registered nonprofits (assigned to counties by ZIP code) / population x 1,000",
  "Water area / (land + water area)",
  "Farm acres / county land acres",
  "Crop sales / farm acres",
  "Adults 25+ with a bachelor's or higher / adults 25+",
  "Adults 25+ with a high school diploma or higher / adults 25+",
  "In labor force / population 16+",
  "Employed / civilian labor force",
  "Population aged 18-64 / total population",
  "Teens 16-19 not enrolled and without a diploma / teens 16-19",
  "People without health insurance / civilian noninstitutionalized population",
  "1 - sum of squared shares across 8 race/ethnicity groups",
  "International residents / total population",
  "Arts, design, entertainment, sports & media workers / employed civilians 16+",
  "Management, business, science & arts workers / employed civilians 16+",
  "Establishments in NAICS 712110 (museums) + 712120 (historical sites) / population x 10,000",
  "Establishments in NAICS 711110, 711130 (theater, music groups) + 711510 (independent artists) / population x 10,000"
)

# Catch name typos with a clear message instead of an NA-subscript error
bad_defs <- setdiff(def_vars, var_info$variable)
if (length(bad_defs) > 0) {
  stop("Definitions given for unknown variables: ", paste(bad_defs, collapse = ", "))
}
if (length(def_vars) != length(def_text)) {
  stop("def_vars and def_text have different lengths")
}

var_info$definition <- ""
var_info$definition[match(def_vars, var_info$variable)] <- def_text

# Setup: Vermont county geometry

# Vermont county polygons from the maps package. Names look like "vermont,addison".
vt_map <- map("county", regions = "vermont", plot = FALSE, fill = TRUE)
vt_map$county <- tools::toTitleCase(sub("^[^,]*,", "", sub(":.*$", "", vt_map$names)))
vt_counties <- sort(unique(vt_map$county))

# One row per county; every variable column is filled from the data files below
vt <- list(name     = "Vermont",
           map      = vt_map,
           counties = vt_counties,
           data     = data.frame(county = vt_counties))

# Data files for each capital
real_files <- list(
  Built     = "vt_built_6.csv",
  Political = "vt_political_2.csv",
  Financial = "vt_county_finance.csv",
  Social    = "county_capitals_2024.csv",
  Natural   = "county_capitals_2024.csv",
  Human     = "county_capitals_2024.csv",
  Cultural  = "county_capitals_2024.csv"
)

# Map app variable names -> column names in your CSVs
col_map <- list(
  Financial = c(
    "Population"                     = "pop",
    "# Owner Occupied Units"         = "owner_occupied_units",
    "Median household income"        = "median_hh_income",
    "# Bank Deposits"                = "deposits_per_county",
    "Investment Interest"            = "Investment Interest Paid",
    "% in Poverty"                   = "Percent in Poverty",
    "% With No Vehicle Availability" = "Percent of households with no vehicle available",
    "% Receiving SNAP"               = "Percent with SNAP"
  ),
  Built = c(
    "% Population with Access to Parks and Playgrounds" = "pct_with_access_to_parks",
    "# Food & Beverage Manufacturing Establishments"    = "number_of_nonemployer_establishments_nestab",
    "Nonemployers Sales Value of Shipments or Revenue"  = "nonemployer_sales_value_of_shipments_or_revenue_1_000_nrcptot",
    "# Non-F&B Manufacturing Establishments"            = "total_est",
    "% Access to Broadband Coverage"                    = "pct_with_broadband_access",
    "Max Advertised Download Speed"                     = "max_advertised_download_speed",
    "Max Advertised Upload Speed"                       = "max_advertised_upload_speed",
    "# Hospitals"                                       = "number_hospitals"
  ),
  Political = c(
    "Voter Turnout (%)"                          = "pct_voter_turnout",
    "Census Response Rate"                       = "pct_participation",
    "Median Value of Political Contributions"    = "median_value_of_contribution",
    "Total Number of Contributions"              = "num_contributions",
    "Total Value of Political Contributions"     = "total_contribution"
  ),
  Social = c(
    "Homeownership rate"                            = "homeown",
    "Share born in state of residence"              = "native_state",
    "Share of households in home since before 2000" = "longterm_res",
    "Share family households"                       = "family_hh",
    "Membership associations per 1,000 people"      = "associations_per1k",
    "Nonprofits per 1,000 people"                   = "nonprofits_per1k"
  ),
  Natural = c(
    "Share of area that is water"    = "water_share",
    "Farmland as share of land area" = "farm_land_share",
    "Crop sales per farm acre"       = "crop_sales_per_acre"
  ),
  Human = c(
    "Share with bachelor's degree or higher"   = "bach_plus",
    "Share with high school diploma or higher" = "hs_or_higher",
    "Labor force participation rate"           = "lfp",
    "Share of civilian labor force employed"   = "employed",
    "Share of population aged 18-64"           = "pop_18_64",
    "Share of 16-19 year olds who dropped out" = "dropout_16_19",
    "Share uninsured"                          = "uninsured"
  ),
  Cultural = c(
    "Racial/ethnic fractionalization"                                   = "ethnic_frac",
    "Share international"                                               = "foreign_born",
    "Share of workers in arts, design, entertainment & media"           = "arts_workers",
    "Share of workers in management, business, science & arts"          = "creative_occ",
    "Museums & historical sites per 10,000 people"                      = "museums_per10k",
    "Performing arts companies & independent artists per 10,000 people" = "perf_arts_per10k"
  )
)

# Read each capital's variables from its data file
add_real_data <- function(st, files) {
  for (cap in names(files)) {
    d <- read.csv(files[[cap]], check.names = FALSE, stringsAsFactors = FALSE)
    
    # Accept "County" or "county"
    names(d)[tolower(names(d)) == "county"] <- "county"
    d$county <- trimws(d$county)
    
    # county_capitals_2024.csv covers every state: keep only Vermont
    if ("state" %in% names(d)) d <- d[d$state == "Vermont", , drop = FALSE]
    
    # Align rows to the order of the Vermont counties
    d <- d[match(st$counties, d$county), , drop = FALSE]
    if (anyNA(d$county)) {
      stop("Missing counties in ", files[[cap]], ": ",
           paste(st$counties[is.na(d$county)], collapse = ", "))
    }
    
    vars <- var_info$variable[var_info$capital == cap]
    src  <- col_map[[cap]]
    
    missing_map <- setdiff(vars, names(src))
    if (length(missing_map) > 0) {
      stop("No column mapping for: ", paste(missing_map, collapse = ", "))
    }
    missing_cols <- setdiff(unname(src[vars]), names(d))
    if (length(missing_cols) > 0) {
      stop("Missing columns in ", files[[cap]], ": ",
           paste(missing_cols, collapse = ", "))
    }
    
    for (v in vars) {
      st$data[[v]] <- as.numeric(d[[ src[[v]] ]])
    }
  }
  st
}

vt <- add_real_data(vt, real_files)

# County information for the About page (from county_capitals_2024.csv)
cc <- read.csv("county_capitals_2024.csv", check.names = FALSE, stringsAsFactors = FALSE)
cc <- cc[cc$state == "Vermont", ]
cc <- cc[match(vt$counties, cc$county), ]
vt$county_info <- data.frame(
  County                     = vt$counties,
  Population                 = format(cc$pop, big.mark = ","),
  `Homeownership (%)`        = round(100 * cc$homeown, 1),
  `Bachelor's or higher (%)` = round(100 * cc$bach_plus, 1),
  `Uninsured (%)`            = round(100 * cc$uninsured, 1),
  check.names = FALSE
)

# Calculate capital scores

rescale <- function(x) 100 * (x - min(x)) / (max(x) - min(x))

# UI

ui <- navbarPage(
  "Community Capitals",
  
  # Index explorer
  tabPanel("Index Explorer",
           div(style = "background-color:#2e7d6b; color:white; padding:15px; margin-bottom:15px; border-radius:5px;",
               h3(textOutput("header_title", inline = TRUE)),
               p("Adjust the weights for each capital on the left to see how the overall index changes across counties.
        Hover over a county to see its index value and ranking. See the Cover Page tab for more about the project."),
           ),
           
           fluidRow(
             column(4,
                    wellPanel(
                      h4("Capital Weights"),
                      lapply(capital_info$capital, function(cap) {
                        sliderInput(paste0("w_", cap), cap, min = 0, max = 1,
                                    value = round(1 / 7, 2), step = 0.01)
                      }),
                      actionButton("reset", "Reset to equal weights"),
                      br(), br(),
                      h5("Weights used (rescaled to add to 1):"),
                      tableOutput("weight_table")
                    )
             ),
             column(8,
                    tags$style(".leaflet-container { background: white; }"),
                    leafletOutput("map", height = 650)
             )
           )
  ),
  
  # Capital deep dive
  tabPanel("Capital Deep Dive",
           div(style = "background-color:#2e7d6b; color:white; padding:15px; margin-bottom:15px; border-radius:5px;",
               fluidRow(
                 column(8, h3(textOutput("cap_title", inline = TRUE)),
                        p(textOutput("cap_description")),
                        p(strong("Method: "), textOutput("cap_method", inline = TRUE))),
                 column(4, selectInput("deep_cap", "Choose a capital",
                                       choices = capital_info$capital))
               )
           ),
           sidebarLayout(
             sidebarPanel(width = 4,
                          h4(textOutput("mini_map_title", inline = TRUE)),
                          leafletOutput("mini_map", height = 300),
                          br(),
                          h4("Variables"),
                          p("Uncheck a variable to drop it. The capital score, contributions, and index will update."),
                          p(em(textOutput("cap_status", inline = TRUE))),
                          lapply(capital_info$capital, function(cap) {
                            vars <- var_info$variable[var_info$capital == cap]
                            conditionalPanel(
                              condition = sprintf("input.deep_cap == '%s'", cap),
                              checkboxGroupInput(paste0("inc_", cap), NULL,
                                                 choices = vars, selected = vars)
                            )
                          }),
                          actionButton("include_all", "Select all variables")
             ),
             mainPanel(width = 8,
                       h4("Variable Contribution"),
                       plotOutput("contrib_plot", height = 300),
                       tableOutput("var_table"),
                       h4(textOutput("county_plot_title", inline = TRUE)),
                       plotOutput("county_plot", height = 300)
             )
           ),
           br(), br()
  ),
  # Cover page
  tabPanel("About",
           fluidRow(
             column(10, offset = 1,
                    h2("About the Project"),
                    p("The Community Capitals Framework describes communities through seven types of capital:
          natural, cultural, human, social, political, financial, and built. This project reviews
          quantitative approaches to measuring the framework (Schmitt, Ringwood & Watson, Mueller,
          Cruz, Taylor) and tests whether a standardized community capitals index is feasible for
          planning and evaluation, using 2020-2024 county data. The app covers Vermont's
          14 counties."),
                    p("Users can choose how much each capital matters for their purpose. For example, someone
          evaluating a mining project might care more about natural and political capital than human capital."),
                    
                    h3("Descriptions of Capitals"),
                    tableOutput("cap_desc_table"),
                    
                    h3("Methodology for Each Capital"),
                    tableOutput("method_table"),
                    p("Each capital score is rescaled from 0 to 100. The overall index is the weighted
          average of the seven capital scores using the weights chosen on the Index Explorer tab."),
                    
                    h3("Sources"),
                    tableOutput("source_table"),
                    
                    h3(textOutput("county_heading", inline = TRUE)),
                    tableOutput("county_table"),
                    br()
             )
           )
  )
)

# Server

server <- function(input, output, session) {
  
  # Vermont's counties, map, and data
  sel_state <- reactive({ vt })
  
  output$header_title <- renderText({
    paste("Community Capitals Index for", sel_state()$name, "Counties")
  })
  
  # weights + index
  weights <- reactive({
    w <- sapply(capital_info$capital, function(cap) input[[paste0("w_", cap)]])
    if (sum(w) == 0) w <- rep(1, length(w))   # avoid dividing by zero
    w / sum(w)
  })
  
  observeEvent(input$reset, {
    for (cap in capital_info$capital) {
      updateSliderInput(session, paste0("w_", cap), value = round(1 / 7, 2))
    }
  })
  
  output$weight_table <- renderTable({
    data.frame(Capital = capital_info$capital, Weight = round(weights(), 3))
  })
  
  # which variables are currently checked for a capital
  included_vars <- function(cap) {
    v <- input[[paste0("inc_", cap)]]
    if (is.null(v)) character(0) else v
  }
  
  # recompute every capital score from the selected variables
  capital_calc <- reactive({
    st     <- sel_state()
    scores <- data.frame(county = st$counties)
    n      <- length(st$counties)
    contribs <- list()
    
    for (cap in capital_info$capital) {
      vars   <- included_vars(cap)
      method <- capital_info$method[capital_info$capital == cap]
      min_n  <- if (method == "PCA") 2 else 1
      
      if (length(vars) < min_n) {
        scores[[cap]] <- rep(NA_real_, n)
        contribs[[cap]] <- data.frame(Variable = character(0),
                                      Source = character(0),
                                      Contribution = numeric(0))
        next
      }
      
      dat <- st$data[, vars, drop = FALSE]
      
      if (method == "PCA") {
        pca <- prcomp(dat, scale. = TRUE)
        load <- pca$rotation[, 1]
        if (sum(load) < 0) load <- -load   # flip so higher = more capital
        score <- as.matrix(scale(dat)) %*% load
        contrib <- abs(load) / sum(abs(load))
      } else {
        score <- rowMeans(scale(dat))
        contrib <- rep(1 / length(vars), length(vars))
      }
      
      scores[[cap]] <- rescale(as.numeric(score))
      contribs[[cap]] <- data.frame(
        Variable = vars,
        Source = var_info$source[match(vars, var_info$variable)],
        Contribution = round(contrib, 3)
      )
    }
    list(scores = scores, contributions = contribs)
  })
  
  # overall index, leaving out capitals with no score and renormalizing weights
  index_data <- reactive({
    cs   <- capital_calc()$scores
    caps <- capital_info$capital
    w    <- weights()
    
    ok <- caps[sapply(caps, function(cp) !all(is.na(cs[[cp]])))]
    
    df <- cs
    if (length(ok) == 0) {
      df$index <- NA_real_
    } else {
      w_ok <- w[caps %in% ok]
      if (sum(w_ok) == 0) w_ok <- rep(1, length(ok))
      w_ok <- w_ok / sum(w_ok)
      df$index <- as.numeric(as.matrix(df[, ok, drop = FALSE]) %*% w_ok)
    }
    df$rank <- rank(-df$index, ties.method = "min")
    df
  })
  
  # map: base layer, zoomed to whichever state is selected
  output$map <- renderLeaflet({
    # No tile layer: the county polygons are drawn from the maps package,
    # so no basemap (or API key) is needed.
    # Map is fixed in place: no dragging or zooming (it still zooms to the
    # selected state automatically, and hover labels still work)
    leaflet(options = leafletOptions(attributionControl = FALSE,
                                     zoomControl = FALSE,
                                     dragging = FALSE,
                                     scrollWheelZoom = FALSE,
                                     doubleClickZoom = FALSE,
                                     touchZoom = FALSE,
                                     boxZoom = FALSE,
                                     keyboard = FALSE)) %>%
      setView(lng = -96, lat = 38, zoom = 4)
  })
  
  observe({
    rng <- sel_state()$map$range
    leafletProxy("map") %>% fitBounds(rng[1], rng[3], rng[2], rng[4])
  })
  
  # map: county colors and labels, redrawn when the state or index changes
  observe({
    st  <- sel_state()
    df  <- index_data()
    matched <- match(st$map$county, df$county)
    vals  <- df$index[matched]
    ranks <- df$rank[matched]
    
    pal <- colorNumeric("YlGn", domain = df$index)
    
    labels <- paste0("<b>", st$map$county, " County</b><br>",
                     "Index value: ", round(vals, 1), "<br>",
                     "Ranking: ", ranks, " of ", nrow(df))
    
    leafletProxy("map") %>%
      clearShapes() %>%
      clearControls() %>%
      addPolygons(data = st$map,
                  fillColor = pal(vals), fillOpacity = 0.8,
                  color = "white", weight = 1,
                  label = lapply(labels, HTML),
                  highlightOptions = highlightOptions(weight = 3, color = "#333",
                                                      bringToFront = TRUE)) %>%
      addLegend(pal = pal, values = df$index, title = "Index", position = "bottomright")
  })
  
  # deep dive: mini map of the selected capital's score, redrawn whenever
  # the capital or its checked variables change. Locked in place like the main map.
  output$mini_map_title <- renderText({ paste(current_cap(), "Capital Score by County") })
  
  output$mini_map <- renderLeaflet({
    st     <- sel_state()
    cs     <- capital_calc()$scores
    scores <- cs[[current_cap()]][match(st$map$county, cs$county)]
    rng    <- st$map$range
    
    m <- leaflet(options = leafletOptions(attributionControl = FALSE,
                                          zoomControl = FALSE,
                                          dragging = FALSE,
                                          scrollWheelZoom = FALSE,
                                          doubleClickZoom = FALSE,
                                          touchZoom = FALSE,
                                          boxZoom = FALSE,
                                          keyboard = FALSE)) %>%
      fitBounds(rng[1], rng[3], rng[2], rng[4])
    
    if (all(is.na(scores))) {
      # not enough variables checked: grey counties
      return(m %>% addPolygons(data = st$map, fillColor = "#cccccc", fillOpacity = 0.8,
                               color = "white", weight = 1,
                               label = paste(st$map$county, "County: not enough variables selected")))
    }
    
    pal <- colorNumeric("YlGn", domain = c(0, 100))
    labels <- paste0("<b>", st$map$county, " County</b><br>",
                     current_cap(), " score: ", round(scores, 1))
    m %>%
      addPolygons(data = st$map,
                  fillColor = pal(scores), fillOpacity = 0.8,
                  color = "white", weight = 1,
                  label = lapply(labels, HTML),
                  highlightOptions = highlightOptions(weight = 3, color = "#333",
                                                      bringToFront = TRUE)) %>%
      addLegend(pal = pal, values = c(0, 100), title = "Score", position = "bottomright")
  })
  
  # deep dive: capital chosen from the dropdown
  current_cap <- reactive({ req(input$deep_cap) })
  
  output$cap_title <- renderText({ paste(current_cap(), "Capital:", sel_state()$name) })
  output$cap_description <- renderText({
    capital_info$description[capital_info$capital == current_cap()]
  })
  output$cap_method <- renderText({
    capital_info$method[capital_info$capital == current_cap()]
  })
  output$county_plot_title <- renderText({
    paste(current_cap(), "Capital Score by County (0-100)")
  })
  
  output$cap_status <- renderText({
    cap    <- current_cap()
    n      <- length(included_vars(cap))
    total  <- sum(var_info$capital == cap)
    method <- capital_info$method[capital_info$capital == cap]
    min_n  <- if (method == "PCA") 2 else 1
    if (n < min_n) {
      paste0("Only ", n, " of ", total, " variables selected. At least ", min_n,
             " needed, so this capital is left out of the index.")
    } else {
      paste0(n, " of ", total, " variables included.")
    }
  })
  
  observeEvent(input$include_all, {
    cap <- current_cap()
    updateCheckboxGroupInput(session, paste0("inc_", cap),
                             selected = var_info$variable[var_info$capital == cap])
  })
  
  output$var_table <- renderTable({
    capital_calc()$contributions[[current_cap()]]
  }, digits = 3)
  
  output$contrib_plot <- renderPlot({
    ct <- capital_calc()$contributions[[current_cap()]]
    validate(need(nrow(ct) > 0, "Not enough variables selected for this capital."))
    par(mar = c(4, 24, 1, 1))   # room for long variable names
    barplot(rev(ct$Contribution), names.arg = rev(ct$Variable), horiz = TRUE,
            las = 1, col = "#2e7d6b", xlab = "Contribution", cex.names = 0.8)
  })
  
  output$county_plot <- renderPlot({
    cs     <- capital_calc()$scores
    scores <- cs[[current_cap()]]
    validate(need(!all(is.na(scores)), "Not enough variables selected for this capital."))
    ord <- order(scores, decreasing = TRUE)
    par(mar = c(7, 4, 1, 1))
    barplot(scores[ord], names.arg = cs$county[ord], las = 2,
            col = "#8bc4a8", ylab = "Score", ylim = c(0, 100))
  })
  
  
  # cover page tables
  output$cap_desc_table <- renderTable({
    data.frame(Capital = capital_info$capital, Description = capital_info$description)
  })
  
  output$method_table <- renderTable({
    data.frame(Capital = capital_info$capital, Method = capital_info$method,
               Notes = capital_info$notes)
  })
  
  output$source_table <- renderTable({
    data.frame(Capital = var_info$capital, Variable = var_info$variable,
               Source = var_info$source, Definition = var_info$definition)
  })
  
  output$county_heading <- renderText({
    paste("County Information:", sel_state()$name)
  })
  
  output$county_table <- renderTable({ sel_state()$county_info })
}

shinyApp(ui, server)