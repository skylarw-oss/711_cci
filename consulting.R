library(tidycensus)
library(lavaan)
census_api_key("c4a6b5d32aba953b9d81168239d8905b7af2fbab", install = TRUE, overwrite = TRUE)
Sys.setenv(CENSUS_API_KEY = "c4a6b5d32aba953b9d81168239d8905b7af2fbab")


d1 <- get_acs("county", state = "VT", variables = "B01001_001E")

# VT financial capital score test

# get VT county data from 2023 (5 year ACS)
vt_counties <- get_acs(
  geography = "county",
  state = "VT",
  variables = c(
    investment_income = "B19064_001",
    population = "B01003_001",
    median_income = "B19013_001"
  ),
  year = 2023,
  survey = "acs5",
  output = "wide"
)
names(vt_counties)
# construct indicators
vt_data <- vt_counties %>%
  mutate(
    investment_pc = investment_incomeE / populationE,
    log_investment_pc = log(investment_pc),
    log_median_income = log(median_incomeE)
  )

# make sure one row per county
vt_data %>%
  select(
    NAME,
    investment_pc,
    log_investment_pc,
    log_median_income
  )

# test with NY county data (VT might be too small to test)
ny_counties <- get_acs(
  geography = "county",
  state = "NY",
  variables = c(
    investment_income = "B19064_001",
    population = "B01003_001"
  ),
  year = 2023,
  survey = "acs5",
  output = "wide"
)

ny_data <- ny_counties %>%
  mutate(
    investment_pc = investment_incomeE / populationE,
    log_investment_pc = log(investment_pc),
  )

# test SEM (as per Ringwood & Watson) - only includes 2 variables
financial_model <- 'Financial =~ log_investment_pc + log_median_income'

fit <- cfa(
  financial_model,
  data = vt_data
)

summary(
  fit,
  standardized = TRUE,
  fit.measures = TRUE
)

# score
vt$financial_score <- as.numeric(
  lavPredict(fit)
)

# try with FDIC and IRS data
# 2023 deposits per capita (06/23/2023)
deposits_per_county_2023 <- read_csv("Summary_Tables_By_State_County_06_30_2023.csv")
# 2023 IRS data
vt_irs_data <- read_csv("23incyvt(Sheet2).csv")

vt_data_join_old <- left_join(deposits_per_county_2023, vt_irs_data, by = "County") |>
  select("County", "All Deposits (Millions USD)", "Total Income", "Investment Interest Paid")

vt_data_join <- vt_data_join_old[-15, ] |>
  mutate(log_deposits = log(`All Deposits (Millions USD)`),
         log_investment_interest = log(`Investment Interest Paid`))

# test SEM with FDIC and IRS data (as per Ringwood & Watson) - only includes 2 variables
financial_model_new <- 'Financial =~ log_investment_interest + log_deposits'

fit_1 <- cfa(
  financial_model_new,
  data = vt_data_join
)

summary(
  fit,
  standardized = TRUE,
  fit.measures = TRUE
)

# score
vt$financial_score <- as.numeric(
  lavPredict(fit)
)
