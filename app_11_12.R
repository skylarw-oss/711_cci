# Community Capitals Index - Shiny app (multi-state)

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
  capital = c(rep("Financial", 5),
              rep("Built", 5),
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
    "# With No Vehicle Availability",
    "% Receiving SNAP",
    
    # Built
    "% Population with Access to Parks and Playgrounds",
    "# Food & Beverage Manufacturing Establishments",
    "# Non-F&B Manufacturing Establishments",
    "% Access to Broadband Coverage",
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
    "Share foreign born",
    "Share of workers in arts, design, entertainment & media",
    "Share of workers in management, business, science & arts",
    "Museums & historical sites per 10,000 people",
    "Performing arts companies & independent artists per 10,000 people"
  ),
  source = c(
    # Financial
    "Census ACS",
    "Census ACS",
    "Census ACS",
    "Census ACS",
    "Census ACS",
    
    # Built
    "CDC",
    "Census CBP Tables",
    "Census CBP Tables",
    "NIH",
    "CMS (Center for Medicare and Medicaid Services)",
    
    # Political (TODO: these three sources are still the Vermont ones; update for the national data)
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
var_info$definition <- ""
var_info$definition[match(c(
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
  "Share foreign born",
  "Share of workers in arts, design, entertainment & media",
  "Share of workers in management, business, science & arts",
  "Museums & historical sites per 10,000 people",
  "Performing arts companies & independent artists per 10,000 people"
), var_info$variable)] <- c(
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
  "Foreign-born residents / total population",
  "Arts, design, entertainment, sports & media workers / employed civilians 16+",
  "Management, business, science & arts workers / employed civilians 16+",
  "Establishments in NAICS 712110 (museums) + 712120 (historical sites) / population x 10,000",
  "Establishments in NAICS 711110, 711130 (theater, music groups) + 711510 (independent artists) / population x 10,000"
)

# Data files

# The app looks for each CSV in these places, in order. Add your own folder if needed.
find_file <- function(name) {
  Filter(file.exists, c(name,                                  # working directory / app folder
                        file.path("~/Desktop/STAT 711", name),
                        file.path("~/Desktop", name),
                        file.path("~/Downloads", name)))[1]
}

# 1) One CSV with every county: Financial, Built, and Political capitals
data_file <- find_file("v1_big_data.csv")
if (is.na(data_file)) {
  stop("Can't find v1_big_data.csv. Working directory is: ", getwd(),
       "\nPut the CSV there, or add its folder to find_file().")
}

# quote = "\"" matters: names like O'Brien and Prince George's contain apostrophes,
# which read.csv would otherwise treat as the start of a quoted string.
read_big <- function(f) {
  read.csv(f, check.names = FALSE, stringsAsFactors = FALSE,
           quote = "\"", encoding = "UTF-8", na.strings = c("NA", "", "-"))
}
big <- read_big(data_file)

# App variable name -> column name in v1_big_data.csv
col_map <- c(
  # Financial
  "Population"                                        = "population_est_2025",
  "# Owner Occupied Units"                            = "owner_occupied",
  "Median household income"                           = "median_hh_income",
  "# With No Vehicle Availability"                    = "num_no_vehicle",
  "% Receiving SNAP"                                  = "snap_pct",
  # Built
  "% Population with Access to Parks and Playgrounds" = "pct_access_to_parks",
  "# Food & Beverage Manufacturing Establishments"    = "num_fb_est",
  "# Non-F&B Manufacturing Establishments"            = "num_non_fb_est",
  "% Access to Broadband Coverage"                    = "pct_broadband_access",
  "# Hospitals"                                       = "num_hospitals",
  # Political
  "Voter Turnout (%)"                                 = "pres_voter_turnout",
  "Census Response Rate"                              = "CRRALL",
  "Median Value of Political Contributions"           = "median_contribution_value",
  "Total Number of Contributions"                     = "total_num_contributions",
  "Total Value of Political Contributions"            = "total_contribution_value"
)

# 2) county_capitals_2024.csv (every state, with "state" and "county" columns):
#    Social, Natural, Human, and Cultural capitals. If the file isn't found, those
#    four capitals fall back to fake data so the app still runs.
cc_file <- find_file("county_capitals_2024.csv")
have_cc <- !is.na(cc_file)

# App variable name -> column name in county_capitals_2024.csv
cc_col_map <- list(
  Social = c(
    "Homeownership rate"               = "homeown",
    "Share born in state of residence" = "native_state",
    "Share of households in home since before 2000"        = "longterm_res",
    "Share family households"          = "family_hh",
    "Membership associations per 1,000 people"    = "associations_per1k",
    "Nonprofits per 1,000 people"      = "nonprofits_per1k"
  ),
  Natural = c(
    "Share of area that is water" = "water_share",
    "Farmland as share of land area"      = "farm_land_share",
    "Crop sales per farm acre"         = "crop_sales_per_acre"
  ),
  Human = c(
    "Share with bachelor's degree or higher"   = "bach_plus",
    "Share with high school diploma or higher" = "hs_or_higher",
    "Labor force participation rate"           = "lfp",
    "Share of civilian labor force employed"            = "employed",
    "Share of population aged 18-64"           = "pop_18_64",
    "Share of 16-19 year olds who dropped out" = "dropout_16_19",
    "Share uninsured"                          = "uninsured"
  ),
  Cultural = c(
    "Racial/ethnic fractionalization"                        = "ethnic_frac",
    "Share foreign born"                              = "foreign_born",
    "Share of workers in arts, design, entertainment & media"                        = "arts_workers",
    "Share of workers in management, business, science & arts"                   = "creative_occ",
    "Museums & historical sites per 10,000 people"                       = "museums_per10k",
    "Performing arts companies & independent artists per 10,000 people" = "perf_arts_per10k"
  )
)

cc_map_flat  <- setNames(unlist(cc_col_map, use.names = FALSE),
                        unlist(lapply(cc_col_map, names), use.names = FALSE))
real_vars_big <- names(col_map)
real_vars_cc  <- unlist(lapply(cc_col_map, names), use.names = FALSE)
real_vars     <- if (have_cc) c(real_vars_big, real_vars_cc) else real_vars_big

# every variable in var_info needs a column mapping
stopifnot(setequal(names(col_map), var_info$variable[var_info$capital %in% c("Financial", "Built", "Political")]),
          setequal(real_vars_cc,  var_info$variable[!var_info$capital %in% c("Financial", "Built", "Political")]),
          all(c("County", "State", col_map) %in% names(big)))

for (col in col_map) big[[col]] <- suppressWarnings(as.numeric(big[[col]]))

# Count variables: within a state that reports them, a blank means none, so it becomes 0
# (done per state in build_state; a state that is blank for ALL its counties stays missing)
count_vars <- c("# Food & Beverage Manufacturing Establishments",
                "# Non-F&B Manufacturing Establishments",
                "# Hospitals")

# Turnout can't pass 100% (a few tiny counties do, because votes > CVAP estimate)
big[[ col_map[["Voter Turnout (%)"]] ]] <- pmin(big[[ col_map[["Voter Turnout (%)"]] ]], 1)

if (have_cc) {
  cc <- read_big(cc_file)
  names(cc)[tolower(names(cc)) == "county"] <- "county"
  names(cc)[tolower(names(cc)) == "state"]  <- "state"
  cc_cols <- unlist(cc_col_map, use.names = FALSE)
  if (!all(c("state", "county") %in% names(cc)) || !all(cc_cols %in% names(cc))) {
    stop("county_capitals_2024.csv is missing: ",
         paste(setdiff(c("state", "county", cc_cols), names(cc)), collapse = ", "))
  }
  for (col in cc_cols) cc[[col]] <- suppressWarnings(as.numeric(cc[[col]]))
}

# Matching counties across the CSVs and the map
# County names are only unique within a state (there are 30+ "Washington" counties),
# so everything is matched on state AND county. Names are normalized first so that
# "St. Clair" = "St Clair", "DeKalb" = "De Kalb", "O'Brien" = "Obrien", and so on.
norm_name <- function(x) {
  x <- tolower(x)
  x <- iconv(x, "UTF-8", "ASCII", sub = "?")   # the CSV has a broken n-tilde in Dona Ana
  x <- gsub("\\?+", "n", x)
  x <- sub(",.*$", "", x)                      # "Dona Ana, New Mexico" -> "Dona Ana"
  x <- sub(" (county|parish|municipio|planning region|borough|census area)$", "", x)
  gsub("[^a-z]", "", x)
}

# Name shown in the app (CSV spelling, without the ", State" suffix)
clean_county <- function(x) {
  x <- sub(",.*$", "", x)
  sub("^Do[^a-zA-Z ]+a Ana$", "Do\u00f1a Ana", x)
}

big$state_key  <- tolower(trimws(big$State))
big$county_key <- norm_name(big$County)
if (have_cc) {
  cc$state_key  <- tolower(trimws(cc$state))
  cc$county_key <- norm_name(cc$county)
}

# Virginia independent cities that the maps package draws as their own polygons.
# The CSV calls them "Norfolk city", etc.
va_city_polys <- c("suffolk", "hampton", "newport news", "norfolk", "virginia beach")

# Setup: state and county geometry

# All county polygons in the maps package. Names look like "vermont,addison",
# and multi-part counties (islands, etc.) have suffixes like "florida,okaloosa:main".
all_county_names <- map("county", plot = FALSE, fill = TRUE)$names

# Lowercase state keys. DC is not a state, and Alaska and Hawaii are not
# in this database, so they are not offered.
state_keys <- setdiff(unique(sub(",.*$", "", all_county_names)), "district of columbia")

# Build one state: county map, data, county info
build_state <- function(key) {
  region_names <- unique(all_county_names[startsWith(all_county_names, paste0(key, ","))])
  m <- map("county", regions = region_names, exact = TRUE, plot = FALSE, fill = TRUE)
  raw <- sub("^[^,]*,", "", sub(":.*$", "", m$names))
  
  # Match each polygon to this state's rows in the big CSV (state first, then county)
  rows     <- big[big$state_key == key, , drop = FALSE]
  poly_key <- norm_name(ifelse(key == "virginia" & raw %in% va_city_polys,
                               paste(raw, "city"), raw))
  hit <- match(poly_key, rows$county_key)      # NA = polygon with no data row
  
  # Polygons with data get the CSV name; polygons without get the map's name
  m$county <- ifelse(is.na(hit), tools::toTitleCase(raw), clean_county(rows$County[hit]))
  
  # One row per county that has data
  has    <- !is.na(hit)
  keep   <- !duplicated(poly_key[has])
  r_use  <- rows[hit[has][keep], , drop = FALSE]
  cty    <- clean_county(r_use$County)
  o      <- order(cty)
  cty    <- cty[o]
  r_use  <- r_use[o, , drop = FALSE]
  
  # Real data sources for these counties, rows lined up with cty
  srcs <- list(big = list(rows = r_use, map = col_map, counts = count_vars))
  if (have_cc) {
    cc_rows <- cc[cc$state_key == key, , drop = FALSE]
    srcs$cc <- list(rows   = cc_rows[match(r_use$county_key, cc_rows$county_key), , drop = FALSE],
                    map    = cc_map_flat,
                    counts = character(0))
  }
  
  # Fake data for every variable first (only the Social/Natural/Human/Cultural
  # variables stay fake, and only if county_capitals_2024.csv wasn't found)
  dat <- data.frame(county = cty)
  for (v in var_info$variable) {
    dat[[v]] <- round(rnorm(length(cty), mean = 50, sd = 10), 1)
  }
  
  # Real data over the top.
  # Blank counts become 0; other blanks get the state median so PCA can run.
  # A variable that is blank for the whole state (or has no spread) can't be
  # scored, so it is marked unusable.
  unusable  <- character(0)
  n_imputed <- 0
  if (length(cty) > 0) {
    for (src in srcs) {
      for (v in names(src$map)) {
        x    <- src$rows[[ src$map[[v]] ]]
        miss <- is.na(x)
        if (all(miss)) {
          unusable <- c(unusable, v)
          dat[[v]] <- x
          next
        }
        if (v %in% src$counts) {
          x[miss] <- 0
        } else {
          x[miss]   <- median(x, na.rm = TRUE)
          n_imputed <- n_imputed + sum(miss)
        }
        dat[[v]] <- x
        if (!isTRUE(sd(x) > 0)) unusable <- c(unusable, v)
      }
    }
  }
  
  # County information for the About page (raw values, before any filling)
  fmt_n <- function(x) format(x, big.mark = ",", trim = TRUE)
  if (have_cc) {
    cr <- srcs$cc$rows
    info <- data.frame(
      County                     = cty,
      Population                 = fmt_n(r_use[[ col_map[["Population"]] ]]),
      `Homeownership (%)`        = round(100 * cr[[ cc_col_map$Social[["Homeownership rate"]] ]], 1),
      `Bachelor's or higher (%)` = round(100 * cr[[ cc_col_map$Human[["Share with bachelor's degree or higher"]] ]], 1),
      `Uninsured (%)`            = round(100 * cr[[ cc_col_map$Human[["Share uninsured"]] ]], 1),
      check.names = FALSE
    )
  } else {
    info <- data.frame(
      County                     = cty,
      Population                 = fmt_n(r_use[[ col_map[["Population"]] ]]),
      `Median household income`  = fmt_n(r_use[[ col_map[["Median household income"]] ]]),
      `Receiving SNAP (%)`       = round(r_use[[ col_map[["% Receiving SNAP"]] ]], 1),
      `Broadband access (%)`     = round(r_use[[ col_map[["% Access to Broadband Coverage"]] ]], 1),
      check.names = FALSE
    )
  }
  
  list(name        = tools::toTitleCase(key),
       map         = m,
       counties    = cty,
       data        = dat,
       county_info = info,
       unusable    = unusable,
       n_imputed   = n_imputed)
}

state_data <- lapply(state_keys, build_state)
names(state_data) <- state_keys

# Only offer states that matched at least one county in the CSV
# (Connecticut drops out: the CSV uses planning regions, the map uses old counties)
state_data    <- Filter(function(st) length(st$counties) > 0, state_data)
state_choices <- setNames(names(state_data), tools::toTitleCase(names(state_data)))

# Calculate capital scores

rescale <- function(x) {
  if (max(x) == min(x)) return(rep(50, length(x)))
  100 * (x - min(x)) / (max(x) - min(x))
}

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
          planning and evaluation, using 2020-2024 county data. Financial, Built, and Political capitals
          use v1_big_data.csv; Social, Natural, Human, and Cultural capitals use county_capitals_2024.csv.
          Counties are matched to the map by state and county name."),
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
  
  # "Addison" -> "Addison County"; parishes and independent cities keep their own name
  county_label <- function(x) {
    ifelse(grepl("(Parish|city|City)$", x), x, paste(x, "County"))
  }
  
  # the selected state's counties, map, and data
  sel_state <- reactive({ state_data[[req(input$state)]] })
  
  output$header_title <- renderText({
    paste("Community Capitals Index for", sel_state()$name, "Counties")
  })
  
  output$data_note <- renderText({
    st  <- sel_state()
    msg <- if (have_cc) {
      "All seven capitals use real county data."
    } else {
      paste("Financial, Built, and Political data are real; Social, Natural, Human, and",
            "Cultural use fake data (county_capitals_2024.csv was not found).")
    }
    if (st$n_imputed > 0) {
      msg <- paste0(msg, " ", st$n_imputed, " missing values were filled with the state median.")
    }
    if (length(st$unusable) > 0) {
      msg <- paste0(msg, " Not available for this state: ",
                    paste(st$unusable, collapse = "; "), ".")
    }
    msg
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
  
  # checked variables that this state actually has data for
  usable_vars <- function(cap) {
    v <- included_vars(cap)
    v[!v %in% sel_state()$unusable]
  }
  
  # recompute every capital score from the selected variables
  capital_calc <- reactive({
    st     <- sel_state()
    scores <- data.frame(county = st$counties)
    n      <- length(st$counties)
    contribs <- list()
    
    for (cap in capital_info$capital) {
      vars   <- usable_vars(cap)
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
    
    pal <- colorNumeric("YlGn", domain = df$index, na.color = "#d9d9d9")
    
    nm <- county_label(st$map$county)
    labels <- ifelse(is.na(matched),
                     paste0("<b>", nm, "</b><br>No data"),
                     paste0("<b>", nm, "</b><br>",
                            "Index value: ", round(vals, 1), "<br>",
                            "Ranking: ", ranks, " of ", nrow(df)))
    
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
    
    nm <- county_label(st$map$county)
    if (all(is.na(scores))) {
      # not enough variables checked: grey counties
      return(m %>% addPolygons(data = st$map, fillColor = "#cccccc", fillOpacity = 0.8,
                               color = "white", weight = 1,
                               label = paste0(nm, ": not enough variables selected")))
    }
    
    pal <- colorNumeric("YlGn", domain = c(0, 100), na.color = "#d9d9d9")
    labels <- ifelse(is.na(scores),
                     paste0("<b>", nm, "</b><br>No data"),
                     paste0("<b>", nm, "</b><br>",
                            current_cap(), " score: ", round(scores, 1)))
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
    n      <- length(usable_vars(cap))
    all_v  <- var_info$variable[var_info$capital == cap]
    total  <- sum(!all_v %in% sel_state()$unusable)
    method <- capital_info$method[capital_info$capital == cap]
    min_n  <- if (method == "PCA") 2 else 1
    extra  <- if (total < length(all_v)) {
      paste0(" (", length(all_v) - total, " variable(s) have no usable data for this state.)")
    } else ""
    if (n < min_n) {
      paste0("Only ", n, " of ", total, " variables selected. At least ", min_n,
             " needed, so this capital is left out of the index.", extra)
    } else {
      paste0(n, " of ", total, " variables included.", extra)
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
    shiny::validate(shiny::need(nrow(ct) > 0, "Not enough variables selected for this capital."))
    par(mar = c(4, 24, 1, 1))   # room for long variable names
    barplot(rev(ct$Contribution), names.arg = rev(ct$Variable), horiz = TRUE,
            las = 1, col = "#2e7d6b", xlab = "Contribution", cex.names = 0.8)
  })
  
  output$county_plot <- renderPlot({
    cs     <- capital_calc()$scores
    scores <- cs[[current_cap()]]
    shiny::validate(shiny::need(!all(is.na(scores)), "Not enough variables selected for this capital."))
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