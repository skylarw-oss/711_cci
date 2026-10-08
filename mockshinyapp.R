# Community Capitals Index for Vermont - Shiny app (draft)
# NOTE: all data in here is FAKE placeholder data for now!

library(shiny)
library(leaflet)
library(maps)

set.seed(67)

# Setup: counties, capitals, variables

counties <- c("Addison", "Bennington", "Caledonia", "Chittenden", "Essex",
              "Franklin", "Grand Isle", "Lamoille", "Orange", "Orleans",
              "Rutland", "Washington", "Windham", "Windsor")

capital_info <- data.frame(
  capital = c("Financial", "Built", "Political", "Social",
              "Natural", "Human", "Cultural"),
  method = c("PCA", "PCA", "Z-score average", "Z-score average",
             "Z-score average", "Z-score average", "Z-score average"),
  description = c(
    "Money and financial resources available in the community for investment, savings, and local government.",
    "Physical infrastructure like housing, roads, broadband, hospitals, and businesses.",
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
  "Concept draft says EFA/CFA, but with only 14 counties we use PCA for now.",
  "Variables are z-scored and averaged with equal weights (following Taylor).",
  "Variables are z-scored and averaged with equal weights.",
  "Variables are z-scored and averaged with equal weights (variables from Schmitt).",
  "Variables are z-scored and averaged with equal weights.",
  "Variables are z-scored and averaged with equal weights."
)

var_info <- data.frame(
  capital = c(rep("Financial", 5), rep("Built", 6), rep("Political", 4),
              rep("Social", 4), rep("Natural", 4), rep("Human", 4),
              rep("Cultural", 3)),
  variable = c(
    # Financial
    "Investment interest", "Bank deposits", "Median household income",
    "Owner occupied units", "Population", "Percentage in poverty", "Percentage of households with no vehicle",
    "Percentage of households receiving SNAP"
    # Built
    "Food & beverage manufacturing establishments", "Other manufacturing establishments",
    "% with fixed advanced telecom access", "Proximity to interstate ramp",
    "Number of housing units", "Total hospitals",
    # Political
    "Voter turnout (2024)", "Census response rate",
    "Political contributions per capita", "Political organizations per capita",
    # Social
    "Associations per capita", "Nonprofits per 1,000 people",
    "% living in same county 5+ years", "Homeownership rate",
    # Natural
    "USDA natural amenities scale", "% acres prime farmland",
    "% acres in conservation", "% acres in National Forest",
    # Human
    "% with high school diploma", "% with bachelor's degree",
    "% with health insurance", "Primary care physicians per 10,000",
    # Cultural
    "Arts organizations per 1,000", "Public libraries per 10,000",
    "Historic sites (NRHP)"
  ),
  source = c(
    "IRS", "FDIC", "Census", "Census", "Census", "SAIPE", "FRAC", "FRAC", 
    "Census", "Census", "FCC", "Dicken et al. (2011)", "Data.CMS.gov",
    "VT Secretary of State", "Census", "FEC", "FEC",
    "County Business Patterns", "NCCS", "Census (ACS)", "Census (ACS)",
    "USDA ERS", "USDA", "USDA", "USFS",
    "Census (ACS)", "Census (ACS)", "Census (ACS)", "County Health Rankings",
    "County Business Patterns", "IMLS", "National Park Service"
  ),
  stringsAsFactors = FALSE
)

# Fake data: one column per variable, one row per county

fake_data <- data.frame(county = counties)
for (v in var_info$variable) {
  fake_data[[v]] <- round(rnorm(length(counties), mean = 50, sd = 10), 1)
}

# fake county info for the cover page
county_info <- data.frame(
  County = counties,
  Population = sample(6000:170000, length(counties)),
  `Median Age` = round(runif(length(counties), 38, 50), 1),
  `% Rural` = round(runif(length(counties), 30, 100), 0),
  check.names = FALSE
)

# Calculate capital scores

rescale <- function(x) 100 * (x - min(x)) / (max(x) - min(x))

capital_scores <- data.frame(county = counties)
contributions <- list()

for (cap in capital_info$capital) {
  vars <- var_info$variable[var_info$capital == cap]
  dat <- fake_data[, vars]
  method <- capital_info$method[capital_info$capital == cap]
  
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
  
  capital_scores[[cap]] <- rescale(as.numeric(score))
  contributions[[cap]] <- data.frame(
    Variable = vars,
    Source = var_info$source[var_info$capital == cap],
    Contribution = round(contrib, 3)
  )
}

# Vermont county map

vt_map <- map("county", "vermont", fill = TRUE, plot = FALSE)
vt_map$county <- tools::toTitleCase(sub("vermont,", "", sub(":.*", "", vt_map$names)))

# UI

ui <- navbarPage(
  "Vermont Community Capitals",
  
  # Index explorer
  tabPanel("Index Explorer",
           div(style = "background-color:#2e7d6b; color:white; padding:15px; margin-bottom:15px; border-radius:5px;",
               h3("Community Capitals Index for Vermont Counties"),
               p("Adjust the weights for each capital on the left to see how the overall index changes across counties.
        Hover over a county to see its index value and ranking. See the Cover Page tab for more about the project."),
               p(em("Note: this version uses fake data."))
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
           p(textOutput("cap_description")),
           p(strong("Method: "), textOutput("cap_method", inline = TRUE)),
           fluidRow(
             column(6,
                    h4("Variables"),
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
  tabPanel("Cover Page",
           fluidRow(
             column(10, offset = 1,
                    h2("About the Project"),
                    p("The Community Capitals Framework describes communities through seven types of capital:
          natural, cultural, human, social, political, financial, and built. This project reviews
          quantitative approaches to measuring the framework (Schmitt, Ringwood & Watson, Mueller,
          Cruz, Taylor) and tests whether a standardized community capitals index is feasible for
          planning and evaluation, using 2020-2024 data for Vermont counties."),
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
                    
                    h3("County Information"),
                    p(em("Placeholder values for now.")),
                    tableOutput("county_table"),
                    br()
             )
           )
  )
)

# Server

server <- function(input, output, session) {
  
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
  
  index_data <- reactive({
    df <- capital_scores
    df$index <- as.numeric(as.matrix(df[, capital_info$capital]) %*% weights())
    df$rank <- rank(-df$index, ties.method = "min")
    df
  })
  
  # map 
  output$map <- renderLeaflet({
    df <- index_data()
    matched <- match(vt_map$county, df$county)
    vals <- df$index[matched]
    ranks <- df$rank[matched]
    
    pal <- colorNumeric("YlGn", domain = df$index)
    
    labels <- paste0("<b>", vt_map$county, " County</b><br>",
                     "Index value: ", round(vals, 1), "<br>",
                     "Ranking: ", ranks, " of ", nrow(df))
    
    leaflet(vt_map) %>%
      addProviderTiles("CartoDB.Positron") %>%
      addPolygons(fillColor = pal(vals), fillOpacity = 0.8,
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
  
  output$var_table <- renderTable({ contributions[[current_cap()]] }, digits = 3)
  
  output$contrib_plot <- renderPlot({
    ct <- contributions[[current_cap()]]
    par(mar = c(4, 16, 1, 1))
    barplot(rev(ct$Contribution), names.arg = rev(ct$Variable), horiz = TRUE,
            las = 1, col = "#2e7d6b", xlab = "Contribution", cex.names = 0.8)
  })
  
  output$county_plot <- renderPlot({
    scores <- capital_scores[[current_cap()]]
    ord <- order(scores, decreasing = TRUE)
    par(mar = c(7, 4, 1, 1))
    barplot(scores[ord], names.arg = capital_scores$county[ord], las = 2,
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
  
  output$county_table <- renderTable({ county_info })
}

shinyApp(ui, server)

