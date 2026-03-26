#file: fbs_data_process

################################################################################

###############################
####
#### FBS 2024-25 data
####
#### This data comes from the 2024-25 FBS publication (COST CENTRES)
####
#### https://www.gov.scot/publications/scottish-farm-business-income-annual-estimates-2024-2025/
##### need to run:
######scripts in latest FBS project to save data to ADM source folder (fbs_data path)  
######(script = costs_outputs_changes - ~Publication/Publication cost centre/costs_outputs_changes.R
########## script = income timeseries fbi_nfi summary ~/R/FBS_2024_25/Publication/Publication farm income workbook
####script = GDP process - ~/R/FBS_2024_25/Publication/Publication GDP/GDP process.R



#### 
####
#################################
# Load necessary libraries
library(here)
library(readxl)
library(stringr)
library(tidyverse)
source(here("Economy/FBS", "fbs_utility.R"))
library(RtoSQLServer)




#data pre-load ------
#LOAD FBS data - run this once to save processed fbs data to data folder (uncomment and edit parameters for new year )
#### parameters - find and update in fbs_utility.R ----

fbs_data_path <- paste("//s0196a/ADM-Rural and Environmental Science-Farming Statistics/Agriculture/Source/FBS/FBS", fbs_current_year)
# load cost centre data
fbs_data <- load(paste0(fbs_data_path, "/FBS ", fbs_current_year, "_CC.rda"))

#load income timeseries data 
load(paste0(fbs_data_path, "/income_timeseries_", fbs_current_year, ".rda"))

# load FBI no supp - taken 
fbi_no_supp <-  read_table_from_db(server=server, 
                                   database=database, 
                                   schema=schema, 
                                   table_name="tox1timeseries")

# load inflation data
load(paste0(fbs_data_path,"/inflation rates for data year 20", (substr(fbs_current_year, 6,7)), ".RData")) 


# ###Data pre-process ------
#remove unwanted columns and flatten into one dataframe
#
fbs_data <- map(Outputs_Costs, ~ .x %>%
                  select(Measure,
                         !!sym(fbs_current_year),
                         !!sym(fbs_prev_year),
                         farm_type,
                         input_output_type,
                         main_category, sub_category_1)) %>% bind_rows(.)
# ####save data  ------
# save(fbs_data, file="Data/FBS_data.Rda" )
# 
# # # load data -----
# load("Data/FBS_data.Rda")

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
  pivot_longer(cols = c(!!sym(fbs_current_year), !!sym(fbs_prev_year) ), names_to = "year", values_to = "value") |> 
# add numeric year col
 mutate(sampyear = recode(as.character(year), !!!fbs_years))



  

# load fbi no support payment data  - reformat and add cols before binding 
# calculate weighted fbi without supp payments and add 
fbi_no_supp <- fbi_no_supp |> group_by(type, year) |>  reframe(no_supp = weighted.mean(fbinosubs, fbswt))
fbi_no_supp <- fbi_no_supp |> mutate(
  `Farm type` = case_when(type == 1 ~ farm_types[[2]],
                          type == 2 ~ farm_types[[3]],
                          type == 3 ~ farm_types [[4]],
                          type == 4 ~ farm_types [[5]],
                          type == 5 ~ farm_types [[6]],
                          type == 6 ~ farm_types [[7]],
                          type == 7 ~ farm_types[[8]],
                          type == 8 ~ farm_types [[9]],
                          type == 9 ~ farm_types[[1]],
                          .default = NA_character_
                            )
) |>
  select(`Farm type`, value = no_supp, year)



fbi_no_supp <- fbi_no_supp |> 
  mutate(Measure = "FBI without support payments",
         value = round(value, 0))  # round to nearest pound
# inflate

#inflation function
# change gdp year to match datayear
gdp$year <- gdp$year+1
  
fbi_no_supp <- inflate_with_lookup(fbi_no_supp, gdp)

# change year to time_periods
fbi_no_supp <- fbi_no_supp |> mutate(fbs_year = paste0("20", (substr(year-1,3, 4)), "-",(substr(year,3, 4) ))) |> 
                                       select(Measure, `Farm type`, year = fbs_year, value)

# filter income for real prices and reformat
fbs_income <- ag_hub$income |> filter (Prices == "Real") |> 
  select (-Prices) |> 
  pivot_longer(-(Measure), names_to = "year", values_to = "value") |> 
  mutate(`Farm type` = "All farms") |>
  filter (!grepl("FTE", Measure)) # remove fte measures

fbs_income$Measure <- gsub("\\(£\\)", "", fbs_income$Measure) # remove £
#fbs_income$Measure <- gsub("FBI", "Farm business income", fbs_income$Measure) # spell out fbi


# join fbi no supp with income timeseries
fbs_income <- rbind(fbs_income, fbi_no_supp)

# same for net farm income
nfi <-ag_hub$NFI |> filter(Prices == "Real") |> 
  select (-Prices) |> 
  mutate(Measure = "Net farm income") |> # add measure col before binding
  pivot_longer(-c(Measure, `Farm type`), names_to = "year", values_to = "value")

nfi$`Farm type` <- gsub("Cereal", "Cereals", nfi$`Farm type`) # change cereal to cereals

# same for farm business income
fbi <-ag_hub$FBI |> filter(Prices == "Real") |> 
  select (-Prices) |> 
  mutate(Measure = "Farm business income") |> # add measure col before binding
  pivot_longer(-c(Measure, `Farm type`), names_to = "year", values_to = "value")

fbi$`Farm type` <- gsub("Cereal", "Cereals", fbi$`Farm type`) # change cereal to cereals

# bind
fbs_income <- rbind(fbs_income, nfi, fbi) |> 
  rename(farm_type = `Farm type`)

# edit farm types to match app
fbs_income$farm_type <- gsub("\\(LFA\\)", "", fbs_income$farm_type)
fbs_income$farm_type <- gsub("Specialist", "LFA", fbs_income$farm_type)
fbs_income$farm_type <- gsub("LFA Cattle", "LFA cattle", fbs_income$farm_type)
fbs_income$farm_type <- gsub("Cattle", "LFA cattle", fbs_income$farm_type)

fbs_income$farm_type <- sub("\\s+$", "", fbs_income$farm_type) # remove trailing space





# add in_out_type column

fbs_income <- fbs_income |> mutate(input_output_type = case_when(grepl("Farm business income", Measure) ~ "fbi",
                                                           Measure == "Net farm income" ~ "nfi",
                                                          grepl("FBI", Measure) ~ "fbi", # for both to appear on chart
                                                           grepl("Support payments", Measure) ~ "supp",
                                                           grepl("Diversified income", Measure) ~ "div_inc",
                                                           grepl("Off farm income", Measure) ~ "ofi",
                                                           .default = NA_character_
                                                           ))


# add numeric year col
fbs_income <- fbs_income |>  mutate(sampyear = recode(as.character(year), !!!fbs_years))
fbs_income <- fbs_income |>  mutate(Measure = paste("Average", tolower(Measure)))
fbs_income$Measure <- gsub("fbi", "FBI",fbs_income$Measure) # capitalise FBI

# CHANGE ORDER TO MAKE FBI APPEAR BEFORE FBI WITH SUPPS
fbs_income <- fbs_income %>%
  arrange(desc(grepl("farm business", Measure)))

# save all

save(fbs_cost_centre, fbs_income, file="Data/FBS_data.Rda" )
load("Data/FBS_data.Rda")
