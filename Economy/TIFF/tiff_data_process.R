#file: tiff_data_process

################################################################################

###############################
####
#### tiff 2024 data
####
#### This data comes from the 2024 tiff publication 
####
#### https://www.gov.scot/publications/total-income-from-farming-estimates-2018-2024/

#################################
# Load necessary libraries
library(readxl)
library(dplyr)
library(tidyr)
library(shiny)
library(here)

#data pre-load ------
#LOAD TIFF data - run this once to save processed tiff data to tiff folder (uncomment and edit parameters for new year )
#### parameters ----

tiff_data_path <- "//s0196a/ADM-Rural and Environmental Science-Farming Statistics/Agriculture/Source/TIFF/TIFF 2024/"
file_path <- paste0(tiff_data_path, "Downloaded TIFF Table 2024.xlsx")

# Get sheet names
sheets <- excel_sheets(file_path)


# Filter sheets that start with "Table"
table_sheets <- sheets[grepl("^Table", sheets)]

# Function to extract table number
get_table_number <- function(sheet_name) {
  as.numeric(gsub("Table ([0-9]+).*", "\\1", sheet_name))
}

# Loop over each relevant sheet
for (sheet in table_sheets) {
  
  # Read data starting from row 6
  df <- read_excel(file_path, sheet = sheet, skip = 5)
  
  # Remove column 2 and last 4 columns
  df <- df[, -2] # Remove column 2
  if (ncol(df) > 4) {
    df <- df[, 1:(ncol(df) - 4)]
  }
  
  # Add "Price" column at position 2
  table_num <- get_table_number(sheet)
  
  price_value <- if (table_num %in% c(1, 2)) {
    "Current (nominal)"
  } else if (table_num %in% c(4, 5)) {
    "Real terms (Constant 2024)"
  } else {
    NA
  }
  
  measure_category <- if(table_num %in% c(1,4)){
    "Outputs"
  } else if (table_num %in% c(2, 5)) {
    "Inputs"
  } else if (table_num %in% 5){
    "Total"
  } else {
    "Total"
  }
  
  df <- df %>%
    mutate(Price = price_value, .before = 2) %>% 
    mutate(Category = measure_category, .before = 3)
  
  # Assign to variable with a unique name (e.g., Table1, Table2, etc.)
  assign(paste0("Table", table_num), df)
}

########################################
### Process tables 1,2,4 and 5 first ###
########################################

# Define the tables you want to process
table_names <- c("Table1", "Table2", "Table4", "Table5")

# Define year columns
year_cols <- as.character(1989:2024)

# Function to process one table
process_table <- function(tbl_name) {
  tbl <- get(tbl_name)                     # retrieve by name
  tbl[year_cols] <- lapply(tbl[year_cols], as.character)  # fix types
  
  tbl_long <- tbl %>%
    pivot_longer(
      cols = all_of(year_cols),
      names_to = "Year",
      values_to = "Value"
    ) %>%
    mutate(
      Year = as.integer(Year),
      Value = as.numeric(Value)
    ) %>%
    rename(Measure = `Measure (£ million)`) %>%
    mutate(Table = tbl_name)
  
  return(tbl_long)
}

combined_data <- table_names %>%
  lapply(process_table) %>%
  bind_rows() %>% 
  select(-Table)%>% #remove table row
  mutate(Measure = gsub(" ", "_", Measure)) %>%
  mutate(
    Measure = gsub(" ", "_", Measure), # spaces → underscores
    Measure = gsub("^\\d+\\._", "", Measure),# remove leading number + dot + underscore
    Measure = gsub("\\(.+\\)", "", Measure),# remove parentheses and their contents
    Measure = gsub("_+$", "", Measure),# remove trailing underscores
    Measure = gsub("__+", "_", Measure)# replace multiple underscores with single
  )

########################################
## Now process Table 3 ##
########################################
year_cols <- as.character(1989:2024)


# Standardize column types
Table3[year_cols] <- lapply(Table3[year_cols], as.character)


Table3_clean <- Table3 %>%
  select(`Measure (£ million) [Note 2]`, Price, Category, all_of(year_cols)) %>%
  rename(Measure = `Measure (£ million) [Note 2]`) %>%
  pivot_longer(
    cols = all_of(year_cols),
    names_to = "Year",
    values_to = "Value"
  ) %>%
  mutate(
    Price = case_when(
      grepl("current \\(nominal\\)", Measure, ignore.case = TRUE) ~ "Current (nominal)",
      grepl("Real terms \\(Constant 2024 price\\)", Measure, ignore.case = TRUE) ~ "Real terms (Constant 2024)",
      TRUE ~ NA_character_
    ),
    Measure = case_when(
      Measure == "33a. Total income from farming, in current (nominal) prices (26-27-31)" ~ "Total income from farming",
      Measure == "33b. Total income from farming, in real terms (constant 2024 price) (26-27-31)" ~ "Total income from farming",
      Measure == "34a. Total income from farming, without support payments, in current (nominal) prices (33a-25)" ~ "Total income from farming, without support payments",
      Measure == "34b. Total income from farming, without support payments, in real terms (constant 2024 price) (33b-25)" ~ "Total income from farming, without support payments",
      TRUE ~ Measure
    ),
    Value = as.numeric(Value),
    Year = as.integer(Year)
  )


########################################
## Join all 5 tables together ##
########################################


full_clean_data <- bind_rows(combined_data, Table3_clean)

#subset to main titles

main_categories <- c(
  "Total_output_from_crops", 
  "Total_output_from_livestock",
  "Total_output_from_other_agricultural_activities", 
  "Total_output_from_non-agricultural_activities",
  "Gross_output", 
  "Total_input_from_feedstuffs",
  "Total_input_from_seeds", 
  "Total_input_from_fertilisers_and_lime", 
  "Total_input_from_farm_maintenance",
  "Total_input_from_miscellaneous_expenses", 
  "FISIM",
  "Total_input_from_non-agricultural_activities", 
  "Gross_input", 
  "Gross_value_added",
  "Total_consumption_of_fixed_capital",
  "Net_value_added",
  "Total_other_support",
  "Total_of_all_support_payments", 
  "Net_value_added_at_factor_cost",
  "Hired_labour", 
  "Interest,_rent_and_taxes",
  "Total_Costs", 
  "Total income from farming", 
  "Total income from farming, without support payments"
)

main_tiff_data_long <- full_clean_data %>%
  filter(Measure %in% main_categories)

#save to data
save(main_tiff_data_long, file="Data/TIFF_data.Rda" )

table(main_tiff_data_long$Measure)
range(main_tiff_data_long$Year, na.rm = TRUE)

tiff_year <- max(main_tiff_data_long$Year) #Current TIFF year
tiff_year_min <- min(main_tiff_data_long$Year)

