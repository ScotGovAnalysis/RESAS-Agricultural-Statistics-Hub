#file: fbs_data_process

################################################################################

###############################
####
#### FBS 2023-24 data
####
#### This data comes from the 2023-24 FBS publication (COST CENTRES)
####
#### https://www.gov.scot/publications/scottish-farm-business-income-annual-estimates-2023-2024/
##### need to run cost centre code in FBS 2023-24 project to save data to ADM source (script = costs_outputs_changes.R)



#### 
####
#################################
# Load necessary libraries
library(readxl)
library(stringr)
library(dplyr)
source(here("Economy/FBS", "fbs_utility.R"))
#data pre-load ------
#LOAD FBS data - run this once to save processed fbs data to data folder (uncomment and edit parameters for new year )
#### parameters ----
current_year <- "2023-24"
prev_year <- "2022-23"
fbs_data_path <- "//s0196a/ADM-Rural and Environmental Science-Farming Statistics/Agriculture/Source/FBS/FBS 2023-24/"

fbs_data <- load(paste0(fbs_data_path, "FBS2023-24_CC.rda"))

# ###Data pre-process ------
# #remove unwanted columns and flatten into one dataframe
# #
fbs_data <- map(Outputs_Costs, ~ .x %>%
                  select(Measure,
                         !!sym(current_year),
                         !!sym(prev_year),
                         farm_type,
                         input_output_type,
                         main_category, sub_category_1)) %>% bind_rows(.)
# ####save data  ------
save(fbs_data, file="Data/FBS_data.Rda" )
#
# # load data -----
load("Data/FBS_data.Rda")

# change farm types to sentence case
snake_to_sentence <- function(snake_str) {
  sentence <- gsub("_", " ", snake_str)                # Replace underscores with spaces
  sentence <- tolower(sentence)                        # Make entire string lowercase
  sentence <- paste0(toupper(substr(sentence, 1, 1)),  # Capitalize first letter
                     substr(sentence, 2, nchar(sentence)))
  return(sentence)
}

fbs_data$farm_type <- snake_to_sentence(fbs_data$farm_type)
# edit Lfa to LFA
fbs_data$farm_type <- gsub("Lfa", "LFA", fbs_data$farm_type)

# edit LFA sheep cattle to LFA cattle and sheep
fbs_data$farm_type <- gsub("sheep cattle", "cattle and sheep", fbs_data$farm_type)

# add cattle and sheep to lowland
fbs_data$farm_type <- gsub("Lowland", "Lowland cattle and sheep", fbs_data$farm_type)


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
                "Agri-environmental schemes",
                "Project based schemes",
                "Other grants and support payments",
                "Output from contracting",
                "Food processing and retailing",
                "Tourism",
                "Recreation",
                "Rental income",
                "Other diversified output",
                "Basic Payment Scheme",
                "Scottish Suckler Beef Support Scheme",
                "Scottish Upland Sheep Scheme",
                "PILLAR 2 Payments",
                "ESA Grants",
                "Support Advisory",
                "Other miscellaneous grants")

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
               "Agri-environmental schemes",
               "Project based schemes",
               "Other grants and support payments",
               "Output from contracting",
               "Food processing and retailing",
               "Tourism",
               "Recreation",
               "Rental income",
               "Other diversified output",
               "Basic Payment Scheme",
               "Scottish Suckler Beef Support Scheme",
               "Scottish Upland Sheep Scheme",
               "PILLAR 2 Payments",
               "ESA Grants",
               "Support Advisory",
               "Other miscellaneous grants"

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
   # add identifiers to data for charts
 fbs_data <- fbs_data %>% mutate(#total_value = case_when(Measure %in% totals~value,
                                                    #.default = NA_integer_),
                                 tot_item = case_when(Measure %in% out_totals ~ "output_totals",
                                                      Measure %in% out_sub_totals ~ "output_sub-cat",
                                                      Measure %in% out_items ~ "output_itemised",
                                                      Measure %in% cost_totals ~ "costs_totals",
                                                      Measure %in% cost_sub_totals ~ "costs_sub-cat",
                                                      Measure %in% cost_items ~ "costs_itemised",
                                                      .default =NA_character_))
                                 #itemised_value = case_when(Measure %in% sub_totals~value,
                                                     # .default = NA_integer_))



  #farm types
farm_types <- c("All farms",
             "Cereals",
             "General cropping",
             "Dairy",
             "LFA sheep",
             "LFA cattle",
             "LFA cattle and sheep",
             "Lowland cattle and sheep",
             "Mixed")

# reformat for charts
fbs_cost_centre <- fbs_data %>%
  pivot_longer(cols = c(!!sym(current_year), !!sym(prev_year) ), names_to = "year", values_to = "value") |> 
# add numeric year col
 mutate(sampyear = recode(as.character(year), !!!fbs_years))



# add income timeseries data: filter for real prices and reformat
load(paste0(fbs_data_path, "income_timeseries_2023-24.Rda"))



fbs_income <- ag_hub$income |> filter (Prices == "Real") |> 
  select (-Prices) |> 
  pivot_longer(-(Measure), names_to = "year", values_to = "value") |> 
  mutate(`Farm type` = "All farms")

# same for net farm income
nfi <-ag_hub$NFI |> filter(Prices == "Real") |> 
  select (-Prices) |> 
  mutate(Measure = "Net farm income") |> # add measure col before binding
  pivot_longer(-c(Measure, `Farm type`), names_to = "year", values_to = "value")

# same for farm business income
fbi <-ag_hub$FBI |> filter(Prices == "Real") |> 
  select (-Prices) |> 
  mutate(Measure = "Farm business income") |> # add measure col before binding
  pivot_longer(-c(Measure, `Farm type`), names_to = "year", values_to = "value")


# bind
fbs_income <- rbind(fbs_income, nfi, fbi) |> 
  rename(farm_type = `Farm type`)

# edit farm types to match app
fbs_income$farm_type <- gsub("\\(LFA\\)", "", fbs_income$farm_type)
fbs_income$farm_type <- gsub("Specialist", "LFA", fbs_income$farm_type)
fbs_income$farm_type <- gsub("LFA Cattle", "LFA cattle", fbs_income$farm_type)
fbs_income$farm_type <- gsub("Cattle", "LFA cattle", fbs_income$farm_type)
fbs_income$farm_type <- gsub("Cereal", "Cereals", fbs_income$farm_type)
fbs_income$farm_type <- sub("\\s+$", "", fbs_income$farm_type) # remove trailing space





# add in_out_type column

fbs_income <- fbs_income |> mutate(input_output_type = case_when(Measure == "Farm business income" ~ "fbi",
                                                           Measure == "Net farm income" ~ "nfi",
                                                           grepl("FBI without support payments", Measure) ~ "fbi_nosupp",
                                                           grepl("Support payments", Measure) ~ "supp",
                                                           grepl("Diversified income", Measure) ~ "div_inc",
                                                           grepl("Off farm income", Measure) ~ "ofi",
                                                           .default = NA_character_
                                                           ))


# add numeric year col
fbs_income <- fbs_income |>  mutate(sampyear = recode(as.character(year), !!!fbs_years))
# save all

save(fbs_cost_centre, fbs_income, file="Data/FBS_data.Rda" )
load("Data/FBS_data.Rda")
