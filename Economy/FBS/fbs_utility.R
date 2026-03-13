# parameters - update each year

fbs_current_year <- "2024-25"
fbs_prev_year <- "2023-24"
fbs_gdp_year <- "2024"
fbs_pub_date <- "26 March 2026"
gdp_pub_date <- "22 December 2025" 
fbs_gdp_url <-   "https://www.gov.uk/government/statistics/gdp-deflators-at-market-prices-and-money-gdp-december-2025-quarterly-national-accounts"

#Adm parameters
server <- "S0196A\\ADM"
database <-  "RuralAndEnvironmentalScienceFarmingStatistics"
schema <- "farmbusinesssurvey2025"



farm_types <- c("All farms",
                "Cereals",
                "General cropping", 
                "Dairy",
                "LFA sheep", 
                "LFA cattle",
                "LFA cattle and sheep", 
                "Lowland cattle and sheep",
                "Mixed")





fbs_years <- c("2012-13" = 2012,
                "2013-14" = 2013,
                "2014-15" = 2014,
                "2015-16" = 2015,
                "2016-17" = 2016,
                "2017-18" = 2017,
                "2018-19" = 2018,
                "2019-20" = 2019,
                "2020-21" = 2020,
                "2021-22" = 2021,
                "2022-23" = 2022,
                "2023-24" = 2023,
               "2024-25" = 2024)

# function to get numeric format for years  
year_levels <- c("2012-13", "2013-14", "2014-15", "2015-16", "2016-17", 
                 "2017-18", "2018-19", "2019-20", "2020-21", 
                 "2021-22", "2022-23", "2023-24", "2024-25")










  # categories


#totals
out_totals <- c("Output from agriculture", # outputs
                "Output from agri-environment activities and other payments",
                "Output from contracting",
                "Output from diversification out of agriculture",
                "Payment schemes in total",
                "Other payment schemes in total")

cost_totals <- c(
  "Agricultural costs",# costs
  "Costs of contracting",
  "Costs of agri-environment activities and other payments",
  "Costs of diversification out of agriculture",
  "Costs of payment schemes")

out_sub_totals <- c("Crop output",
                    "Livestock output",
                    "Support  payments to agriculture (severe weather payments)",
                    "Miscellaneous output (including agricultural work done on other farms)",
                    "LFASS",
                    "Project based schemes",
                    "Other grants and support payments",
                    "Output from contracting", # INCLUDE AS LOWEST LEVEL - NO SUB CATEGORIES
                    "Food processing and retailing",
                    "Tourism",
                    "Recreation",
                    "Rental income",
                    "Other diversified output",
                    "Basic Payment Scheme",
                    "Scottish Suckler Beef Support Scheme",
                    "Scottish Upland Sheep Scheme",
                    "PILLAR 2 Payments")

out_items <- c("Wheat",
               "Barley",
               "Oats",
               "Other cereals",
               "Oilseed rape",
               "Peas and beans",
               "Potatoes",
               "Other crops",
               "By-products, forage and cultivations (excluding set-aside)",
               "Disposal of previous crops",
               "Milk and milk products",
               "Cattle",
               "Sheep and wool",
               "Pigs",
               "Eggs",
               "Other livestock (including horses)",
               "Support  payments to agriculture (severe weather payments)",
               "Miscellaneous output (including agricultural work done on other farms)",
               "Ownership income",
               "LFASS",
               "Project based schemes",
               "ESA grants",
               "Other grants and support payments",
               "Support Advisory",
               "Other miscellaneous grants",
               "Output from contracting",
               "Food processing and retailing",
               "Tourism",
               "Recreation",
               "Rental income",
               "Other diversified output",
               "Basic Payment Scheme",
               "Scottish Suckler Beef Support Scheme",
               "Scottish Upland Sheep Scheme",
               "PILLAR 2 Payments"
               
)

cost_sub_totals <- c(
  "Variable agricultural costs", #costs
  "Fixed agricultural costs",
  "Fixed contract costs",
  "Variable contract costs",
  "Variable agri-environment costs",
  "Fixed agri-environment costs",
  "Crop specific costs",
  "Livestock specific costs",
  "All feed and fodder",
  "Casual labour",
  "Miscellaneous variable costs (including for work done on other farms)",
  "Regular labour",
  "Machinery costs",
  "General farming costs",
  "Land and property costs",
  "Variable diversification costs",
  "Fixed diversification costs",
  "Variable payment schemes costs",
  "Fixed payment schemes costs"
  
)

cost_items <- c("Casual labour",
                "Miscellaneous variable costs (including for work done on other farms)",
                "Seed",
                "Fertilisers",
                "Crop protection",
                "Other crop costs",
                "Purchased feed & fodder",
                "Home grown feed & fodder",
                "Veterinary fees & medicines",
                "Other livestock costs",
                "Machinery running costs",
                "Machinery depreciation",
                "Regular labour",
                "Bank charges & professional fees",
                "Electricity",
                "Water",
                "Farm taxes",
                "Share of net interest payments",
                "Other general farm costs",
                "Agri-environment labour costs",
                "Agri-environment machinery costs",
                "Agri-environment general farming costs (including share of interest)",
                "Agri-environment land and property costs",
                "Diversification labour costs",
                "Diversification machinery costs",
                "Diversification general farming costs (including share of interest)",
                "Diversification land and property costs",
                "Payment schemes labour costs",
                "Payment schemes machinery costs",
                "Payment schemes general farming costs (including share of interest)",
                "Payment schemes land and property costs",
                "Variable payment schemes costs",
                "Variable diversification costs",
                "Variable agri-environment costs"
)

# inflation function 
  # df: has columns `year`, `value`
  # gdp: has columns `year`, `inflation_rate` (or any name you pass)
  inflate_with_lookup <- function(df,
                                  gdp,
                                  value_col = "value",
                                  year_col = "year",
                                  inflation_col_lookup = "inflation") {
    value_col <- rlang::ensym(value_col)
    year_col <- rlang::ensym(year_col)
    inflation_col_lookup <- rlang::ensym(inflation_col_lookup)
    
    df %>%
      left_join(gdp %>% select(!!year_col, !!inflation_col_lookup), by = rlang::as_name(year_col)) %>%
      mutate(
        !!value_col := (!!value_col / !!inflation_col_lookup)*100
      ) %>%
      select(-!!inflation_col_lookup)  # drop lookup column if you don't need it
  }
