# Community Capitals Index - Shiny app (draft, multi-state)

library(shiny)
library(leaflet)
library(maps)

set.seed(67)

# Setup: capitals, variables (the same for every state)

capital_info <- data.frame(
  capital = c("Financial",
              "Built",
              "Political",
              "Social",
              "Natural",
              "Human",
              "Cultural"),
  method = c("PCA",
             "PCA",
             "PCA",
             "Z-score average",
             "Z-score average",
             "Z-score average",
             "Z-score average"),
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
  "Variables are z-scored and averaged with equal weights (following Taylor).",
  "Variables are z-scored and averaged with equal weights.",
  "Variables are z-scored and averaged with equal weights (variables from Schmitt).",
  "Variables are z-scored and averaged with equal weights.",
  "Variables are z-scored and averaged with equal weights."
)

var_info <- data.frame(
  capital = c(rep("Financial", 8),
              rep("Built", 8),
              rep("Political", 5),
              rep("Social", 4),
              rep("Natural", 4),
              rep("Human", 4),
              rep("Cultural", 3)),
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
    "Associations per capita",
    "Nonprofits per 1,000 people",
    "% living in same county 5+ years",
    "Homeownership rate",

    # Natural
    "USDA natural amenities scale",
    "% acres prime farmland",
    "% acres in conservation",
    "% acres in National Forest",

    # Human
    "% with high school diploma",
    "% with bachelor's degree",
    "% with health insurance",
    "Primary care physicians per 10,000",

    # Cultural
    "Arts organizations per 1,000",
    "Public libraries per 10,000",
    "Historic sites (NRHP)"
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

    # Social, Natural, Human, Cultural (placeholders)
    rep("Fake", 15)
  ),
  stringsAsFactors = FALSE
)

# Setup: state and county geometry

# All county polygons in the maps package. Names look like "vermont,addison",
# and multi-part counties (islands, etc.) have suffixes like "florida,okaloosa:main".
all_county_names <- map("county", plot = FALSE, fill = TRUE)$names

# Lowercase state keys. DC is not a state, and Alaska and Hawaii are not
# in this database, so they are not offered.
state_keys    <- setdiff(unique(sub(",.*$", "", all_county_names)), "district of columbia")
state_choices <- setNames(state_keys, tools::toTitleCase(state_keys))

# Build one state: county map, fake variables, fake county info
build_state <- function(key) {
  region_names <- unique(all_county_names[startsWith(all_county_names, paste0(key, ","))])
  m <- map("county", regions = region_names, exact = TRUE, plot = FALSE, fill = TRUE)
  m$county <- tools::toTitleCase(sub("^[^,]*,", "", sub(":.*$", "", m$names)))
  cty <- sort(unique(m$county))

  # Fake data: one column per variable, one row per county
  dat <- data.frame(county = cty)
  for (v in var_info$variable) {
    dat[[v]] <- round(rnorm(length(cty), mean = 50, sd = 10), 1)
  }

  # fake county info for the cover page
  info <- data.frame(
    County = cty,
    Population = sample(6000:170000, length(cty)),
    `Median Age` = round(runif(length(cty), 38, 50), 1),
    `% Rural` = round(runif(length(cty), 30, 100), 0),
    check.names = FALSE
  )

  list(name        = tools::toTitleCase(key),
       map         = m,
       counties    = cty,
       data        = dat,
       county_info = info)
}

state_data <- lapply(state_keys, build_state)
names(state_data) <- state_keys

# Real data overlays, keyed by state. Add more states here as CSVs become available.
real_files <- list(
  vermont = list(
    Built     = "~/Desktop/STAT 711/vt_built_6.csv",
    Political = "~/Desktop/STAT 711/vt_political_2.csv"
  )
)

# Map app variable names -> column names in your CSVs
col_map <- list(
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
  )
)

# Replace fake columns with real ones for one state's capitals
add_real_data <- function(st, files) {
  for (cap in names(files)) {
    d <- read.csv(files[[cap]], check.names = FALSE, stringsAsFactors = FALSE)

    # Accept "County" or "county"
    names(d)[tolower(names(d)) == "county"] <- "county"
    d$county <- trimws(d$county)

    # Align rows to the order of this state's counties
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

for (key in names(real_files)) {
  state_data[[key]] <- add_real_data(state_data[[key]], real_files[[key]])
}

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
               p(em(textOutput("data_note", inline = TRUE)))
           ),

           fluidRow(
             column(4,
                    selectInput("state", "State", choices = state_choices, selected = "vermont"),
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
                    leafletOutput("map", height = 650)
             )
           ),

           hr(),

           # ---- Capital deep dive ----
           h3("Capital Deep Dive"),
           fluidRow(
             column(2, actionButton("prev_cap", "< Previous", width = "100%")),
             column(8, h3(textOutput("cap_title"), style = "text-align:center; margin-top:0;")),
             column(2, actionButton("next_cap", "Next >", width = "100%"))
           ),
           div(style = "display:none", textOutput("cap_now")),  # hidden helper for the checkbox panels
           p(textOutput("cap_description")),
           p(strong("Method: "), textOutput("cap_method", inline = TRUE)),
           p(textOutput("cap_status", inline = TRUE)),
           fluidRow(
             column(6,
                    h4("Variables"),
                    p("Uncheck a variable to drop it. The capital score, contributions, and index will update."),
                    actionButton("include_all", "Select all variables"),
                    br(), br(),
                    lapply(capital_info$capital, function(cap) {
                      vars <- var_info$variable[var_info$capital == cap]
                      conditionalPanel(
                        condition = sprintf("output.cap_now == '%s'", cap),
                        checkboxGroupInput(paste0("inc_", cap), NULL,
                                           choices = vars, selected = vars)
                      )
                    }),
                    tableOutput("var_table")
             ),
             column(6,
                    h4("Variable Contribution"),
                    plotOutput("contrib_plot", height = 300)
             )
           ),
           fluidRow(
             column(12,
                    h4("Capital Score by County (0-100)"),
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
          planning and evaluation, using 2020-2024 county data. Real data is currently available
          for Vermont only; other states use placeholder data."),
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
                    p(em("Placeholder values for now.")),
                    tableOutput("county_table"),
                    br()
             )
           )
  )
)

# Server

server <- function(input, output, session) {

  # the selected state's counties, map, and data
  sel_state <- reactive({ state_data[[req(input$state)]] })

  output$header_title <- renderText({
    paste("Community Capitals Index for", sel_state()$name, "Counties")
  })

  output$data_note <- renderText({
    if (input$state %in% names(real_files)) {
      "Built and Political data are real for this state; other variables use fake data."
    } else {
      "Note: this state uses fake data for all variables."
    }
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
    leaflet() %>%
      addProviderTiles("CartoDB.Positron") %>%
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

  # deep dive: cycle through capitals
  current <- reactiveVal(1)
  n_caps <- nrow(capital_info)

  observeEvent(input$next_cap, {
    if (current() == n_caps) current(1) else current(current() + 1)
  })
  observeEvent(input$prev_cap, {
    if (current() == 1) current(n_caps) else current(current() - 1)
  })

  current_cap <- reactive({ capital_info$capital[current()] })

  output$cap_title <- renderText({ paste(current_cap(), "Capital") })
  output$cap_description <- renderText({ capital_info$description[current()] })
  output$cap_method <- renderText({ capital_info$method[current()] })

  # hidden output that tells the checkbox panels which capital is showing
  output$cap_now <- renderText({ current_cap() })
  outputOptions(output, "cap_now", suspendWhenHidden = FALSE)

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
    par(mar = c(4, 16, 1, 1))
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
               Source = var_info$source)
  })

  output$county_heading <- renderText({
    paste("County Information:", sel_state()$name)
  })

  output$county_table <- renderTable({ sel_state()$county_info })
}

shinyApp(ui, server)
