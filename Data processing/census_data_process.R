################################################################################
# Script name: census_data_process.R
# Last updated: 21/10/2026
#
# Description: processes census data for the ag stats hub. Data manipulation
# within this section:
# >> Census data - main data, agricultural region data and local authority data
#    > Structure
#    > Livestock
#    > Crops
# >> 2023 module (fertiliser/manure)
# >> 2024 module (machinery)
# >> 2025 module (irrigation)
#
# --------------------------------
#
# How to update data:
# - Individual instructions are available for each section. 
# - Inputted will need to be in the same format as previous iterations unless
#   code is edited to adapt this. 
# - Current data within the section primarily comes from the publicly available
#   publication tables.
# - Download the most recent tables from the publication, add them to the project
#   directory, and modify the file path.
# - Data processing is designed to be robust, but small changes within
#   publications will likely lead to bugs needing fixed
# - Ensure when updating census information that the all subsections are updated below.
################################################################################

# 1. Set up- -------------------------------------------------------------------

# Load libraries
library(readxl)
library(openxlsx)
library(dplyr)
library(tidyr)
library(stringr)
library(highcharter)
library(ggthemes)
library(purrr)

# Source functions
source("charts_tables_functions/function_data_cleaning.R")


# 2. Census Data ---------------------------------------------------------------

# Xlsx file originates from the published data tables in supporting documents
# 2026 version: https://www.gov.scot/publications/results-scottish-agricultural-census-june-2026/documents/

# To update, download the excel document and insert file into compendium working directory / file
# Insert file path in the file_path below.
# Check table_names for changes vs. the previous year's names and order## If the order is incorrect, change the table number accordingly

# Try to avoid renaming tables below unless necessary as these are referenced throughout the app
# If changing any names, ensure all sourcing of the data matches changing (print_code.R can help to QA this)

# The data processing was designed for the 2023 census tables, and changes to tables may cause bugs
# Processing aims to standardise all tables to make them compatible with the different visualisation modules
# This includes removing the metadata above the tables, renaming headers, removing problematic trailing spaces, etc.

# Additional processing can be included if necessary
# Some data processing could be moved from within the server pages in the app to speed up loading & improve efficiency

# To update, run the code below.
# QA results before saving as RData
# RData is then loaded within options.RData

# Ensure the rest of the census code is ran subsequently after updating the base tables.


## 2a. Read in main data and agricultural region data --------------------------

# Define file path
file_path <- "Data/June Agricultural Census 2026 - Main data tables.xlsx"

# Define the simplified table names and corresponding sheet names
table_names <- c(
  "agricultural_area_hectares" = "Table_1",
  "vegetables_bulbs_fruit_area" = "Table_2",
  "number_of_cattle" = "Table_3",
  "number_of_sheep" = "Table_4",
  "number_of_pigs" = "Table_5",
  "number_of_poultry" = "Table_6",
  "number_of_other_livestock" = "Table_7",
  "occupiers_employees" = "Table_8",
  
  "occupiers_sex" = "Table_10",
  "occupiers_age_gender" = "Table_11",
  "legal_responsibility" = "Table_12",
  "owned_rented_land" = "Table_13",
  "farm_type" = "Table_14",
  "agricultural_area_lfa" = "Table_15",
  "holdings_crops_grass_subregion" = "Table_16",
  "crops_grass_area_subregion" = "Table_17",
  "holdings_livestock_region_subregion" = "Table_18",
  "livestock_subregion" = "Table_19",
  "holdings_occupiers_employees_subregion" = "Table_20",
  "occupiers_employees_subregion" = "Table_21",
  "holdings_region_subregion_farm_type" = "Table_22",
  "agricultural_area_region_subregion_farm_type" = "Table_23",
  "holdings_area_size_band_farm_type" = "Table_24",
  "agricultural_area_area_size_band_farm_type" = "Table_25"
)


# Read each table, clean it, and assign to a variable
for (table in names(table_names)) {
  sheet_name <- table_names[table]
  data <- read_excel(file_path, sheet = sheet_name, col_names = FALSE)  # Read data without headers
  
  # Remove rows until the first occurrence of "Source:" in the first column
  data <- remove_until_source(data)
  
  # Clean headers manually since the first row was treated as data
  headers <- as.character(data[1, ])  # Take the first row as headers
  cleaned_headers <- clean_headers(headers)
  
  # Remove the first row that is now the headers
  data <- data[-1, ]
  
  # Assign cleaned headers to the data
  names(data) <- cleaned_headers
  
  # Remove columns with '5 year' in the header (case insensitive)
  columns_to_remove <- grep("5 year", cleaned_headers, ignore.case = TRUE)
  if (length(columns_to_remove) > 0) {
    data <- data[, -columns_to_remove]
    cleaned_headers <- cleaned_headers[-columns_to_remove]
  }
  
  # Convert all columns except the first to numeric, setting non-numeric values to NA
  data <- data %>% mutate(across(-1, ~ as.numeric(as.character(.))))
  
  # Remove '\r\n' from all character columns and clean multiple spaces in the first column
  data[[1]] <- str_replace_all(data[[1]], "\r\n", " ")
  data <- clean_cells(data)
  
  # Clean data
  cleaned_data <- clean_data(data)
  
  assign(table, cleaned_data)
}


## 2b. Read in Local Authority data --------------------------------------------

# Define LA file path
xlsx_path <- "Data/unitary_authority_data_2026.xlsx"

# Get sheet names
sheets <- excel_sheets(xlsx_path)

# Read each sheet into a named list of tibbles
wb_list <- map(sheets, ~ read_excel(xlsx_path, sheet = .x, na = "c"))
names(wb_list) <- sheets

safe_names <- make.names(names(wb_list))              # base R safe names
safe_names <- str_replace_all(safe_names, "\\.", "_") # turn dots into underscores

# Assign to the current environment (or specify .GlobalEnv)
list2env(setNames(wb_list, safe_names), envir = .GlobalEnv)

# Remove units from column names
names(unitary_crops_area) <- names(unitary_crops_area) %>%
  str_replace_all(" \\(Hectares\\)", "")
names(unitary_workforce_numbers) <- names(unitary_workforce_numbers) %>%
  str_replace_all(" \\(Number\\)", "")
names(unitary_livestock_numbers) <- names(unitary_livestock_numbers) %>%
  str_replace_all(" \\(Number\\)", "")


## 2c. Structure ---------------------------------------------------------------

### Owned and rented land ----

# Time series table data
owned_rented_land <- owned_rented_land %>%
  select(-contains("% Change"))

save(owned_rented_land, file = "Data/owned_rented_data.RData")


### Land use ----

# Time series table data 
land_use_timeseries_table <- agricultural_area_hectares %>% 
  mutate(across(where(is.numeric), ~ round(.x, 0))) %>% 
  select(-contains("% Change")) %>% 
  filter(`Crop/land use` %in% c("Total crops, fallow, and set-aside", 
                                "Rough grazing",
                                "Other land (including woodland)",
                                "Total grass", 
                                "Total sole right agricultural area", 
                                "Common grazings"))

# Time series chart data
land_use_timeseries_chart <- land_use_timeseries_table %>% 
  pivot_longer(-`Crop/land use`, names_to = "year", values_to = "value")

# Agricultural region table data
land_use_region_table <- crops_grass_area_subregion %>% 
  filter(`Land use by category` %in% c(
    "Total crops, fallow, and set-aside",
    "Rough grazing",
    "Other land (including woodland)",
    "Total sole right agricultural area",
    "Common grazings",
    "Total agricultural area")) %>% 
  mutate(across(where(is.numeric), ~ round(.x, 0))) %>% 
  select(-`Scotland total`)

# Agricultural region map data
land_use_region_map <- land_use_region_table %>% 
  pivot_longer(-`Land use by category`, 
               names_to = "sub_region", values_to = "value")

# Local authority map data
land_use_unitauth_map <- unitary_crops_area %>%
  mutate(across(where(is.numeric), ~ round(.x, 0))) %>% 
  select(unitauth,
         `Total crops, fallow, and set-aside`,
         `Rough grazing`,
         `Other land (including woodland)`,
         `Total sole right agricultural area`,
         `Common grazings`,
         `Total agricultural area`) %>% 
  pivot_longer(-unitauth, names_to = "land use", values_to = "value") %>% 
  select(`land use`, unitauth, value)

# Local authority table data
land_use_unitauth_table <- land_use_unitauth_map %>%
  pivot_wider(names_from = "unitauth", values_from = "value") %>% 
  rename(`Land use by category` = `land use`)

# Save data
save(
  land_use_timeseries_chart,
  land_use_timeseries_table,
  land_use_region_map,
  land_use_region_table,
  land_use_unitauth_map,
  land_use_unitauth_table,
  file = "Data/land_use_data.RData"
)


### Occupiers -----

# Remove % change column
occupiers_employees <- occupiers_employees %>%
  select(-contains("% Change"))

# Time series table data 
occupiers_timeseries_table <- occupiers_employees %>%
  filter(grepl("occupiers", `Occupiers and employees by category`, 
                ignore.case = TRUE))

# Time series chart data
occupiers_timeseries_chart <- occupiers_timeseries_table %>%
  pivot_longer(
    cols = -`Occupiers and employees by category`,
    names_to = "Year", values_to = "Value")

# Agricultural region table data
occupiers_region_table <- occupiers_employees_subregion %>%
  select(-`Scotland total`) %>%
  filter(
    `Occupiers and employees by category` %in% c(
      "Total working occupiers",
      "Occupiers not working on the holding"))

# Agricultural region map data
occupiers_region_map <- occupiers_region_table %>%
  pivot_longer(
    cols = -`Occupiers and employees by category`,
    names_to = "sub_region", values_to = "value") %>% 
  rename(occupier = `Occupiers and employees by category`)

# Local authority map data
occupiers_unitauth_map <- unitary_workforce_numbers %>%
  select(unitauth, `Total working occupiers`, `Occupiers not working on the holding`) %>% 
  pivot_longer(cols = -unitauth, names_to = "occupier", values_to = "value") %>% 
  select(occupier, unitauth, value)

# Local authority table data
occupiers_unitauth_table <- occupiers_unitauth_map %>%
  pivot_wider(names_from = "unitauth", values_from = "value") %>% 
  rename(`Occupiers and employees by category` = occupier)

# Population pyramid chart data
occupiers_pyramid_chart <- occupiers_age_gender %>%
  filter(`Occupier working time` == "Total working occupiers") %>%
  select(-`Male Total`, -`Female Total`) %>%
  pivot_longer(cols = -`Occupier working time`, 
               names_to = "Gender_Age", values_to = "Count") %>%
  separate(Gender_Age, into = c("Gender", "Age"), sep = " ", extra = "merge")

# Population pyramid table data
occupiers_pyramid_table <- occupiers_pyramid_chart %>%
  pivot_wider(names_from = Age, values_from = Count)

# Save data
save(
  occupiers_timeseries_chart,
  occupiers_timeseries_table,
  occupiers_region_map,
  occupiers_region_table,
  occupiers_unitauth_map,
  occupiers_unitauth_table,
  occupiers_pyramid_chart,
  occupiers_pyramid_table,
  file = "Data/occupiers_data.RData"
)


### Employees ----

# Categories used in maps
employee_categories <- c(
  "Regular full-time staff total",
  "Regular part-time staff total",
  "Total casual and seasonal staff",
  "Total workforce (including occupiers)")

# Remove % change column
occupiers_employees <- occupiers_employees %>%
  select(-contains("% Change"))

# Time series table data (remove occupier rows)
employees_timeseries_table <- occupiers_employees %>%
  filter(!grepl("occupiers", `Occupiers and employees by category`, 
                ignore.case = TRUE))

# Time series chart data
employees_timeseries_chart <- employees_timeseries_table %>%
  pivot_longer(
    cols = -`Occupiers and employees by category`,
    names_to = "Year", values_to = "Value") %>%
  mutate(Year = as.numeric(Year))

# Agricultural region map data
employees_region_map <- occupiers_employees_subregion %>%
  select(-`Scotland total`) %>%
  pivot_longer(
    cols = -`Occupiers and employees by category`,
    names_to = "sub_region", values_to = "value") %>%
  filter(`Occupiers and employees by category` %in% employee_categories)

# Agricultural region table data
employees_region_table <- employees_region_map %>%
  pivot_wider(names_from = sub_region, values_from = value)

# Local authority map data
employees_unitauth_map <- unitary_workforce_numbers %>%
  select(unitauth, all_of(employee_categories)) %>%
  pivot_longer(cols = -unitauth, names_to = "workforce", values_to = "value") %>% 
  select(workforce, unitauth, value)

# Local authority table data
employees_unitauth_table <- employees_unitauth_map %>%
  pivot_wider(names_from = "unitauth", values_from = "value") %>% 
  rename(`Occupiers and employees by category` = workforce)

# Save data
save(
  occupiers_employees,
  employees_timeseries_chart,
  employees_timeseries_table,
  employees_region_map,
  employees_region_table,
  employees_unitauth_map,
  employees_unitauth_table,
  file = "Data/employees_data.RData"
)


## 2d. Livestock ---------------------------------------------------------------

### Cattle -----

# Remove % change column
number_of_cattle <- number_of_cattle %>%
  select(-contains("% Change"))

# Time series table data
cattle_timeseries_table <- number_of_cattle

# Time series chart data
cattle_timeseries_chart <- number_of_cattle %>%
  pivot_longer(cols = -`Cattle by category`, names_to = "year", values_to = "value") %>%
  mutate(year = as.numeric(year))

# Total cattle (long)
total_cattle_long <- cattle_timeseries_chart %>%
  filter(`Cattle by category` == "Total cattle") %>%
  select(Year = year, `Total cattle` = value)

# Agricultural region map data
cattle_region_map <- livestock_subregion %>%
  filter(`Livestock by category` %in%   
           c("Total cattle", "Total female dairy cattle",
             "Total female beef cattle", "Total male cattle",
             "Total calves")) %>%
  select(-`Scotland total`) %>%
  pivot_longer(cols = -`Livestock by category`, names_to = "sub_region", values_to = "value") %>%
  mutate(value = as.numeric(value))

# Agricultural region table data
cattle_region_table <- cattle_region_map %>%
  pivot_wider(names_from = sub_region, values_from = value)

# Local authority table data
cattle_unitauth_table <- unitary_livestock_numbers %>%
  select(unitauth, `Total cattle`, `Total female dairy cattle`,
         `Total female beef cattle`, `Total male cattle`,
         `Total calves`) %>%
  pivot_longer(cols = -unitauth, names_to = "livestock", values_to = "value") %>%
  mutate(value = as.numeric(value)) %>%
  pivot_wider(names_from  = unitauth, values_from = value) %>% 
  rename(`Cattle by category` = `livestock`)

# Local authority map data
cattle_unitauth_map <- unitary_livestock_numbers %>%
  select(unitauth, `Total cattle`, `Total female dairy cattle`,
         `Total female beef cattle`, `Total male cattle`,
         `Total calves`) %>% 
  pivot_longer(cols = -unitauth, names_to = "livestock", values_to = "value") %>%
  mutate(value = if_else(is.na(value), NA_integer_, as.integer(value))) %>% 
  select(livestock, unitauth, value)

# Save data
save(
  cattle_timeseries_table,
  cattle_timeseries_chart,
  cattle_region_table,
  cattle_region_map,
  cattle_unitauth_table,
  cattle_unitauth_map,
  file = "Data/cattle_data.RData"
)


### Sheep -----

# Remove % change column
number_of_sheep <- number_of_sheep %>%
  select(-contains("% Change"))

# Time series table data
sheep_timeseries_table <- number_of_sheep

# Time series chart data
sheep_timeseries_chart <- number_of_sheep %>%
  pivot_longer(cols = -`Sheep by category`, names_to = "year", values_to = "value") %>%
  mutate(year = as.numeric(year))  

# Total sheep (long)
total_sheep_long <- sheep_timeseries_chart %>% 
  filter(`Sheep by category` == "Total sheep") %>%
  select(Year = year, `Total sheep` = value)

# Agricultural region map data
sheep_region_map <- livestock_subregion %>%
  filter(`Livestock by category` %in%   
           c("Ewes used for breeding in previous season",
             "Other sheep for breeding aged 1 year and over",
             "Rams to be used for service",
             "Lambs",
             "Total sheep"))%>%
  select(-`Scotland total`) %>%
  pivot_longer(cols = -`Livestock by category`, names_to = "sub_region", values_to = "value") %>%
  mutate(value = as.numeric(value))

# Agricultural region table data
sheep_region_table <- sheep_region_map %>%
  pivot_wider(names_from = sub_region, values_from = value)

# Local authority table data
sheep_unitauth_table <- unitary_livestock_numbers %>%
  select(unitauth, `Total sheep`, `Ewes used for breeding in previous season`,
         `Other sheep for breeding aged 1 year and over`, `Rams to be used for service`,
         `Lambs`) %>%
  pivot_longer(cols = -unitauth, names_to = "livestock", values_to = "value") %>%
  mutate(value = as.numeric(value)) %>%
  pivot_wider(names_from  = unitauth, values_from = value) %>% 
  rename(`Sheep by category` = `livestock`)

# Local authority map data
sheep_unitauth_map <- unitary_livestock_numbers %>%
  select(unitauth, `Total sheep`, `Ewes used for breeding in previous season`,
         `Other sheep for breeding aged 1 year and over`, `Rams to be used for service`,
         `Lambs`) %>% 
  pivot_longer(cols = -unitauth, names_to = "livestock", values_to = "value") %>%
  mutate(value = if_else(is.na(value), NA_integer_, as.integer(value))) %>% 
  select(livestock, unitauth, value)

# Save data
save(
  sheep_timeseries_table,
  sheep_timeseries_chart,
  sheep_region_table,
  sheep_region_map,
  sheep_unitauth_table,
  sheep_unitauth_map,
  file = "Data/sheep_data.RData"
)


### Pigs -----

# Remove % change column
number_of_pigs <- number_of_pigs %>%
  select(-contains("% Change"))

# Time series table data
pigs_timeseries_table <- number_of_pigs

# Time series chart data
pigs_timeseries_chart <- number_of_pigs %>%
  pivot_longer(cols = -`Pigs by category`, names_to = "year", values_to = "value") %>%
  mutate(year = as.numeric(year))  

# Total pigs (long)
total_pigs_long <- pigs_timeseries_chart %>% 
  filter(`Pigs by category` == "Total pigs") %>%
  select(Year = year, `Total pigs` = value)

# Agricultural region map data
pigs_region_map <- livestock_subregion %>%
  filter(`Livestock by category` %in%   
           c("Female pigs breeding herd",
             "All other non-breeding pigs",
             "Total pigs"))%>%
  select(-`Scotland total`) %>%
  pivot_longer(cols = -`Livestock by category`, names_to = "sub_region", values_to = "value") %>%
  mutate(value = as.numeric(value))

# Agricultural region table data
pigs_region_table <- pigs_region_map %>%
  pivot_wider(names_from = sub_region, values_from = value)

# Local authority table data
pigs_unitauth_table <- unitary_livestock_numbers %>%
  select(unitauth, `Total pigs`, `Female pigs breeding herd`,
         `All other non-breeding pigs`) %>%
  pivot_longer(cols = -unitauth, names_to = "livestock", values_to = "value") %>%
  mutate(value = as.numeric(value)) %>%
  pivot_wider(names_from  = unitauth, values_from = value) %>% 
  rename(`pigs by category` = `livestock`)

# Local authority map data
pigs_unitauth_map <- unitary_livestock_numbers %>%
  select(unitauth, `Total pigs`, `Female pigs breeding herd`,
         `All other non-breeding pigs`) %>% 
  pivot_longer(cols = -unitauth, names_to = "livestock", values_to = "value") %>%
  mutate(value = if_else(is.na(value), NA_integer_, as.integer(value))) %>% 
  select(livestock, unitauth, value)

# Save data
save(
  pigs_timeseries_table,
  pigs_timeseries_chart,
  pigs_region_table,
  pigs_region_map,
  pigs_unitauth_table,
  pigs_unitauth_map,
  file = "Data/pigs_data.RData"
)


### Poultry -----

# Remove % change column
number_of_poultry <- number_of_poultry %>% 
  select(-contains("% Change"))

# Time series table data
poultry_timeseries_table <- number_of_poultry

# Time series chart data
poultry_timeseries_chart <- number_of_poultry %>%
  pivot_longer(cols = -`Poultry by category`, names_to = "year", values_to = "value") %>%
  mutate(year = as.numeric(year)) 

# Total poultry (long)
total_poultry_long <- poultry_timeseries_chart %>% 
  filter(`Poultry by category` == "Total poultry") %>%
  select(Year = year, `Total poultry` = value)

# Agricultural region map data
poultry_region_map <- livestock_subregion %>%
  filter(`Livestock by category` %in%   
           c("Fowls for producing eggs",
             "Fowls for breeding",
             "Broilers, other table fowls and other poultry",
             "Total poultry"))%>%
  select(-`Scotland total`) %>%
  pivot_longer(cols = -`Livestock by category`, names_to = "sub_region", values_to = "value") %>%
  mutate(value = as.numeric(value))

# Agricultural region table data
poultry_region_table <- poultry_region_map %>%
  pivot_wider(names_from = sub_region, values_from = value)

# Local authority table data
poultry_unitauth_table <- unitary_livestock_numbers %>%
  select(unitauth, `Total poultry`, `Fowls for producing eggs`,
         `Fowls for breeding`, `Broilers, other table fowls and other poultry`) %>%
  pivot_longer(cols = -unitauth, names_to = "livestock", values_to = "value") %>%
  mutate(value = as.numeric(value)) %>%
  pivot_wider(names_from  = unitauth, values_from = value) %>% 
  rename(`poultry by category` = `livestock`)

# Local authority map data
poultry_unitauth_map <- unitary_livestock_numbers %>%
  select(unitauth, `Total poultry`, `Fowls for producing eggs`,
         `Fowls for breeding`, `Broilers, other table fowls and other poultry`) %>% 
  pivot_longer(cols = -unitauth, names_to = "livestock", values_to = "value") %>%
  mutate(value = if_else(is.na(value), NA_integer_, as.integer(value))) %>% 
  select(livestock, unitauth, value)

# Save data
save(
  poultry_timeseries_table,
  poultry_timeseries_chart,
  poultry_region_table,
  poultry_region_map,
  poultry_unitauth_table,
  poultry_unitauth_map,
  file = "Data/poultry_data.RData"
)


### Other livestock -----

# Remove % change column
number_of_other_livestock <- number_of_other_livestock %>%
  select(-contains("% Change"))

# Time series table data
other_livestock_timeseries_table <- number_of_other_livestock 

# Time series chart data
other_livestock_timeseries_chart <- number_of_other_livestock %>%
  pivot_longer(cols = -`Livestock by category`, names_to = "year", values_to = "value") %>%
  mutate(year = as.numeric(year)) 

# Agricultural region map data
other_livestock_region_map <- livestock_subregion %>%
  filter(`Livestock by category` %in%   
           c("Goats and kids", "Deer", "Horses", "Donkeys", "Camelids", "Beehives")) %>%
  select(-`Scotland total`) %>%
  pivot_longer(cols = -`Livestock by category`, names_to = "sub_region", values_to = "value") %>%
  mutate(value = as.numeric(value))

# Agricultural region table data
other_livestock_region_table <- other_livestock_region_map %>%
  pivot_wider(names_from = sub_region, values_from = value)

# Local authority table data
other_livestock_unitauth_table <- unitary_livestock_numbers %>%
  select(unitauth, `Goats and kids`, `Deer`, `Horses`,
         `Donkeys`, `Camelids`, `Beehives`) %>%
  pivot_longer(cols = -unitauth, names_to = "livestock", values_to = "value") %>%
  mutate(value = as.numeric(value)) %>%
  pivot_wider(names_from  = unitauth, values_from = value) %>% 
  rename(`Livestock by category` = `livestock`)

# Local authority map data
other_livestock_unitauth_map <- unitary_livestock_numbers %>%
  select(unitauth, `Goats and kids`, `Deer`, `Horses`,
         `Donkeys`, `Camelids`, `Beehives`) %>% 
  pivot_longer(cols = -unitauth, names_to = "livestock", values_to = "value") %>%
  mutate(value = if_else(is.na(value), NA_integer_, as.integer(value))) %>% 
  select(livestock, unitauth, value)

# Save data
save(
  other_livestock_timeseries_table,
  other_livestock_timeseries_chart,
  other_livestock_region_table,
  other_livestock_region_map,
  other_livestock_unitauth_table,
  other_livestock_unitauth_map,
  file = "Data/other_livestock_data.RData"
)



### Total livestock -----

# Merge the dataframes on the 'Year' column
total_animals <- total_pigs_long %>%
  inner_join(total_poultry_long, by = "Year") %>%
  inner_join(total_sheep_long, by = "Year") %>%
  inner_join(total_cattle_long, by = "Year")

# Convert Year column to numeric
total_animals$Year <- as.numeric(total_animals$Year)

# Save data
save(total_animals, file = "Data/total_animals.RData")



## 2e. Crops -------------------------------------------------------------------

# Clean column names
names(agricultural_area_hectares) <- names(agricultural_area_hectares) %>%
  str_replace_all("Area", "") %>%
  str_trim
names(vegetables_bulbs_fruit_area) <- names(vegetables_bulbs_fruit_area) %>%
  str_replace_all("Area", "") %>%
  str_trim

# Preprocess and round the summary crops data
crops_summary_data <- agricultural_area_hectares %>%
  select(-contains("% Change")) %>% 
  filter(`Crop/land use` %in% c(
    "Total combine harvested crops", "Total crops for stockfeeding",
    "Vegetables for human consumption", "Soft fruit")) %>%
  pivot_longer(cols = -`Crop/land use`, names_to = "Year", values_to = "Value") %>%
  mutate(
    Year = as.numeric(Year),  # Ensure Year is numeric
    Value = if_else(
      abs(as.numeric(Value)) >= 1000,  # Round up for numbers with more than 3 significant figures
      round(as.numeric(Value)),
      round(as.numeric(Value), digits = 2)))

# Save data
save(crops_summary_data, file = "Data/crops_data.RData")

### Cereals ------

# Subset for cereals data
cereals_data <- agricultural_area_hectares %>%
  select(-`% Change 2026 to 2025`) %>%
  filter(`Crop/land use` %in% c(
    "Wheat", "Triticale", "Winter barley", "Spring barley", "Barley total", 
    "Winter Oats", "Spring Oats", "Oats total", "Rye", "Mixed grain", "Total Cereals"))

# Time series chart data (Area)
cereals_area_chart <- cereals_data %>%
  pivot_longer(cols = -`Crop/land use`, names_to = "Year", values_to = "value") %>%
  mutate(Year = as.numeric(Year),
         value = round(value, 0),
         Measure = "Area")

# Agricultural region map data
cereals_region_map <- crops_grass_area_subregion %>%
  select(-`Scotland total`) %>%
  pivot_longer(
    cols = -`Land use by category`,
    names_to = "sub_region", values_to = "value") %>%
  mutate(value = round(value, 0)) %>% 
  filter(`Land use by category` %in% c(
    "Wheat", "Winter barley", "Spring barley", "Oats and mixed grain"))

# Agricultural region table data
cereals_region_table <- cereals_region_map %>%
  pivot_wider(names_from = sub_region, values_from = value)

# Local authority map data
cereals_unitauth_map <- unitary_crops_area %>%
  select(unitauth, Wheat, `Winter barley`, `Spring barley`, `Oats and mixed grain`) %>% 
  pivot_longer(cols = -unitauth, names_to = "crop", values_to = "value") %>%
  mutate(value = round(value, 0)) %>% 
  select(crop, unitauth, value)

# Local authority table data
cereals_unitauth_table <- cereals_unitauth_map %>%
  pivot_wider(names_from = "unitauth", values_from = "value") %>% 
  rename(`Crop/land use` = crop)

# Save data
save(
  cereals_data,
  cereals_area_chart,
  cereals_region_map,
  cereals_region_table,
  cereals_unitauth_map,
  cereals_unitauth_table,
  file = "Data/cereals_data.RData"
)


### Beans -----

# Subset for beans data
beans_data <- agricultural_area_hectares %>%
  select(-contains("% Change")) %>% 
  filter(`Crop/land use` %in% c("Protein peas", "Field beans")) %>%
  mutate(across(where(is.numeric), ~ round(.x, 0)))

# Subset for beans_subregion
beans_subregion <- crops_grass_area_subregion %>%
  filter(`Land use by category` %in% c(
    "Peas and beans for combining")) %>%
  mutate(across(where(is.numeric), ~ round(.x, 0)))

# Time series table data
beans_timeseries_table <- beans_data

# Time series chart data
beans_timeseries_chart <- beans_data %>%
  pivot_longer(cols = -`Crop/land use`, names_to = "year", values_to = "value") %>%
  mutate(year = as.numeric(year))

# Agricultural region map data
beans_region_map <- beans_subregion %>%
  select(-`Scotland total`) %>%
  pivot_longer(cols = -`Land use by category`, 
               names_to = "sub_region", values_to = "value") %>%
  mutate(value = as.numeric(value))

# Agricultural region table data
beans_region_table <- beans_region_map %>%
  pivot_wider(names_from = sub_region, values_from = value)

# Local authority map data
beans_unitauth_map <- unitary_crops_area %>%
  select(unitauth, `Peas and beans for combining`) %>% 
  pivot_longer(cols = -unitauth, names_to = "crop", values_to = "value") %>%
  mutate(value = round(value, 0)) %>% 
  select(crop, unitauth, value)

# Local authority table data
beans_unitauth_table <- beans_unitauth_map %>%
  rename(`Crop/land use` = crop) %>%
  pivot_wider(names_from = "unitauth", values_from = "value")

# Save data
save(
  beans_data,
  beans_subregion,
  beans_timeseries_chart,
  beans_timeseries_table,
  beans_region_map,
  beans_region_table,
  beans_unitauth_map,
  beans_unitauth_table,
  file = "Data/beans_data.RData"
)


### Fruit -----

# Subset for soft fruit data
fruit_data <- vegetables_bulbs_fruit_area %>%
  select(-contains("% Change")) %>% 
  filter(`Vegetables and fruits for human consumption` %in% c(
    "Strawberries grown in the open",
    "Raspberries grown in the open",
    "Blueberries grown in the open",
    "Blackcurrants and other fruit grown in the open",
    "Total soft fruit grown in the open",
    "Tomatoes grown under cover",
    "Strawberries grown under cover",
    "Raspberries grown under cover",
    "Blueberries grown under cover",
    "Other fruit grown under cover",
    "Vegetables grown under cover",
    "Strawberries grown in open/under cover",
    "Raspberries grown in open/under cover",
    "Blackcurrants grown in open/under cover",
    "Blueberries grown in open/under cover",
    "Tomatoes grown in open/under cover",
    "Other fruit grown in open/under cover",
    "Total soft fruit")) %>% 
  mutate(across(where(is.numeric), ~ round(.x, 0)))

# Subset for fruit subregion
fruit_subregion <- crops_grass_area_subregion %>%
  filter(`Land use by category` %in% c(
    "Orchard and soft fruit")) %>%
  mutate(across(where(is.numeric), ~ round(.x, 0)))

# Time series table data
fruit_timeseries_table <- fruit_data

# Time series chart data
fruit_timeseries_chart <- fruit_data %>%
  pivot_longer(
    cols = -`Vegetables and fruits for human consumption`,
    names_to = "year", values_to = "value") %>%
  mutate(year = as.numeric(year))

# Agricultural region map data
fruit_region_map <- fruit_subregion %>%
  select(-`Scotland total`) %>%
  pivot_longer(
    cols = -`Land use by category`,
    names_to = "sub_region", values_to = "value") %>%
  mutate(value = as.numeric(value))

# Agricultural region table data
fruit_region_table <- fruit_region_map %>%
  pivot_wider(names_from = sub_region, values_from = value)

# Local authority map data
fruit_unitauth_map <- unitary_crops_area %>%
  select(unitauth, `Orchard and soft fruit`) %>% 
  pivot_longer(cols = -unitauth, names_to = "crop", values_to = "value") %>%
  mutate(value = round(value, 0)) %>% 
  select(crop, unitauth, value)

# Local authority table data
fruit_unitauth_table <- fruit_unitauth_map %>%
  rename(`Land use by category` = crop) %>%
  pivot_wider(names_from = "unitauth", values_from = "value")

# Save data
save(
  fruit_data,
  fruit_subregion,
  fruit_timeseries_chart,
  fruit_timeseries_table,
  fruit_region_map,
  fruit_region_table,
  fruit_unitauth_map,
  fruit_unitauth_table,
  file = "Data/fruit_data.RData"
)


### Vegetables for human consumption ----

# Subset for human vegetables data
human_vegetables_data <- vegetables_bulbs_fruit_area %>%
  select(-contains("% Change")) %>% 
  filter(`Vegetables and fruits for human consumption` %in% c(
    "Peas For Canning, Freezing Or Drying",
    "Beans For Canning, Freezing Or Drying",
    "Turnips/swedes",
    "Calabrese",
    "Cauliflower",
    "Carrots",
    "Other Vegetables",
    "Total Vegetables")) %>% 
  mutate(across(where(is.numeric), ~ round(.x, 0)))

# Subset for human_veg_subregion
human_vegetables_subregion <- crops_grass_area_subregion %>%
  filter(`Land use by category` %in% c(
    "Vegetables for human consumption")) %>%
  mutate(across(where(is.numeric), ~ round(.x, 0)))

# Time series table data
vegetables_timeseries_table <- human_vegetables_data

# Time series chart data
vegetables_timeseries_chart <- human_vegetables_data %>%
  pivot_longer(
    cols = -`Vegetables and fruits for human consumption`,
    names_to = "year", values_to = "value") %>%
  mutate(year = as.numeric(year),
         value = round(value, 0))

# Agricultural region map data
vegetables_region_map <- human_vegetables_subregion %>%
  select(-`Scotland total`) %>%
  pivot_longer(
    cols = -`Land use by category`,
    names_to = "sub_region", values_to = "value") %>%
  mutate(value = as.numeric(value))

# Agricultural region table data
vegetables_region_table <- vegetables_region_map %>%
  pivot_wider(names_from = sub_region, values_from = value)

# Local authority map data
vegetables_unitauth_map <- unitary_crops_area %>%
  select(unitauth, `Vegetables for human consumption`) %>% 
  pivot_longer(cols = -unitauth, names_to = "crop", values_to = "value") %>%
  mutate(value = round(value, 0)) %>% 
  select(crop, unitauth, value)

# Local authority table data
vegetables_unitauth_table <- vegetables_unitauth_map %>%
  rename(`Land use by category` = crop) %>%
  pivot_wider(names_from = "unitauth", values_from = "value")

# Save data
save(
  human_vegetables_data,
  human_vegetables_subregion,
  vegetables_timeseries_chart,
  vegetables_timeseries_table,
  vegetables_region_map,
  vegetables_region_table,
  vegetables_unitauth_map,
  vegetables_unitauth_table,
  file = "Data/human_vegetables_data.RData"
)


### Oilseeds -----

# Time series chart data (Area)
oilseed_area_chart <- agricultural_area_hectares %>%
  select(-contains("% Change")) %>% 
  pivot_longer(cols = -`Crop/land use`, names_to = "Year", values_to = "value") %>%
  filter(`Crop/land use` %in% c(
    "Winter oilseed rape", "Spring oilseed rape", "Linseed", "Total oilseeds")) %>% 
  mutate(Year = as.numeric(Year),
         Measure = "Area",
         value = round(value, 0))

# Agricultural region table data
oilseed_region_table <- crops_grass_area_subregion %>%
  select(-`Scotland total`) %>% 
  filter(`Land use by category` %in% c(
    "Oilseeds (including linseed)")) %>%
  mutate(across(where(is.numeric), ~ round(.x, 0)))

# Agricultural region map data
oilseed_region_map <- oilseed_region_table %>%
  pivot_longer(
    cols = -`Land use by category`,
    names_to = "sub_region", values_to = "value") %>%
  mutate(value = as.numeric(value))

# Local authority map data
oilseed_unitauth_map <- unitary_crops_area %>%
  select(unitauth, `Oilseeds (including linseed)`) %>% 
  pivot_longer(cols = -unitauth, names_to = "crop", values_to = "value") %>%
  mutate(value = round(value, 0)) %>% 
  select(crop, unitauth, value)
  
# Local authority table data
oilseed_unitauth_table <- oilseed_unitauth_map %>%
  rename(`Crop/land use` = crop) %>% 
  pivot_wider(names_from = "unitauth", values_from = "value")

# Save data
save(
  oilseed_area_chart,
  oilseed_region_map,
  oilseed_region_table,
  oilseed_unitauth_map,
  oilseed_unitauth_table,
  file = "Data/oilseed_data.RData"
)


### Potatoes ----

# Subset for potatoes data
potatoes_data <- agricultural_area_hectares %>%
  select(-contains("% Change")) %>% 
  filter(`Crop/land use` %in% c("Seed potatoes", "Ware potatoes", "Total potatoes")) %>% 
  mutate(across(where(is.numeric), ~ round(.x, 0)))

# Subset for potato_subregion
potatoes_subregion <- crops_grass_area_subregion %>%
  filter(`Land use by category` %in% c(
    "Potatoes")) %>%
  mutate(across(where(is.numeric), ~ round(.x, 0)))

# Time series table data
potatoes_timeseries_table <- potatoes_data

# Time series chart data
potatoes_timeseries_chart <- potatoes_data %>%
  pivot_longer(
    cols = -`Crop/land use`,
    names_to = "year", values_to = "value"
  ) %>%
  mutate(year = as.numeric(year))

# Agricultural region map data
potatoes_region_map <- potatoes_subregion %>%
  select(-`Scotland total`) %>%
  pivot_longer(
    cols = -`Land use by category`,
    names_to = "sub_region", values_to = "value") %>%
  mutate(value = as.numeric(value))

# Agricultural region table data
potatoes_region_table <- potatoes_region_map %>%
  pivot_wider(names_from = sub_region, values_from = value)

# Local authority map data
potatoes_unitauth_map <- unitary_crops_area %>%
  select(unitauth, Potatoes) %>% 
  pivot_longer(cols = -unitauth, names_to = "crop", values_to = "value") %>%
  mutate(value = round(value, 0)) %>% 
  select(crop, unitauth, value)

# Local authority table data
potatoes_unitauth_table <- potatoes_unitauth_map %>%
  rename(`Land use by category` = crop) %>%
  pivot_wider(names_from = "unitauth", values_from = "value")

# Save data
save(
  potatoes_data,
  potatoes_subregion,
  potatoes_timeseries_chart,
  potatoes_timeseries_table,
  potatoes_region_map,
  potatoes_region_table,
  potatoes_unitauth_map,
  potatoes_unitauth_table,
  file = "Data/potatoes_data.RData"
)


### Stockfeeding ----

# Subset for animal feed data
stockfeeding_data <- agricultural_area_hectares %>%
  select(-contains("% Change")) %>% 
  filter(`Crop/land use` %in% c(
    "Turnips/swedes", "Kale/cabbage", "Maize", "Rape", "Fodder beet", 
    "Lupins", "Other crops for stockfeeding", "Total crops for stockfeeding")) %>% 
  mutate(across(where(is.numeric), ~ round(.x, 0)))

# Subset for stockfeeding_subregion
stockfeeding_subregion <- crops_grass_area_subregion %>%
  filter(`Land use by category` %in% c("Stockfeeding crops")) %>%
  mutate(across(where(is.numeric), ~ round(.x, 0)))

# Time series table data
stockfeeding_timeseries_table <- stockfeeding_data

# Time series chart data
stockfeeding_timeseries_chart <- stockfeeding_data %>%
  pivot_longer(
    cols = -`Crop/land use`,
    names_to = "year", values_to = "value") %>%
  mutate(year = as.numeric(year))

# Agricultural region map data
stockfeeding_region_map <- stockfeeding_subregion %>%
  select(-`Scotland total`) %>%
  pivot_longer(
    cols = -`Land use by category`,
    names_to = "sub_region", values_to = "value") %>%
  mutate(value = as.numeric(value))

# Agricultural region table data
stockfeeding_region_table <- stockfeeding_region_map %>%
  pivot_wider(names_from = sub_region, values_from = value)

# Local authority map data
stockfeeding_unitauth_map <- unitary_crops_area %>%
  select(unitauth, `Stockfeeding crops`) %>% 
  pivot_longer(cols = -unitauth, names_to = "crop", values_to = "value") %>%
  mutate(value = round(value, 0)) %>% 
  select(crop, unitauth, value)

# Local authority table data
stockfeeding_unitauth_table <- stockfeeding_unitauth_map %>%
  rename(`Land use by category` = crop) %>%
  pivot_wider(names_from = "unitauth", values_from = "value")

# Save data
save(
  stockfeeding_data,
  stockfeeding_subregion,
  stockfeeding_timeseries_chart,
  stockfeeding_timeseries_table,
  stockfeeding_region_map,
  stockfeeding_region_table,
  stockfeeding_unitauth_map,
  stockfeeding_unitauth_table,
  file = "Data/stockfeeding_data.RData"
)


# 3. 2023 module (fertiliser/manure) -------------------------------------------

# This data comes from the 2023 Agricultural Census Module Results:
# https://www.gov.scot/publications/results-from-the-scottish-agricultural-census-module-june-2023/

# The script extracts tables looking at soil management, fertiliser usage, manure, nitrogen
# and formats the data into formatting to be used in the modules.

# This section should not need re-run as the module data will not be updated, 
# though can be used as a baseline to include future year's module data.

# Save all tables to an RData file
save(list = names(table_names), file = "Data/census_data.RData")

# load fertiliser data from june 2023
file_path <- "Data/June+Agricultural+Census+2023+Tables.xlsx"
manure_fertiliser <- read_excel(file_path, sheet = "Table_21")
save(manure_fertiliser, file = "manure_fertiliser.RData")

# Define the file path
file_path <- "Data/June+Agricultural+Census+2023+-+Module+Report+-+Production+methods+and+nutrient+application+-+Tables.xlsx"

# Define the sheet names to read
sheets_to_read <- c("Table_4", "Table_5", "Table_7", "Table_8", "Table_9", "Table_12")

# Define shortened names for the dataframes
short_names <- c("soil_nutrient_mgmt", "grass_crop_nutrient_mgmt", "nitrogen_250", 
                 "nitrogen_400", "manure_qty", "fertiliser_use")



# Read and process the specified sheets into a list of dataframes
data_frames <- lapply(sheets_to_read, read_and_process_sheet)

# Name the list elements with shortened names
names(data_frames) <- short_names

# Remove the 'Area' column from 'grass_crop_nutrient_mgmt' if it exists
if("Area" %in% colnames(data_frames$grass_crop_nutrient_mgmt)) {
  data_frames$grass_crop_nutrient_mgmt <- select(data_frames$grass_crop_nutrient_mgmt, -Area)
}

# Ensure 'Percentage of holdings' and 'Average holding area' are numeric in both dataframes
data_frames$soil_nutrient_mgmt$`Percentage of holdings` <- as.numeric(as.character(data_frames$soil_nutrient_mgmt$`Percentage of holdings`))
data_frames$grass_crop_nutrient_mgmt$`Percentage of holdings` <- as.numeric(as.character(data_frames$grass_crop_nutrient_mgmt$`Percentage of holdings`))

data_frames$soil_nutrient_mgmt$`Average holding area` <- as.numeric(as.character(data_frames$soil_nutrient_mgmt$`Average holding area`))
data_frames$grass_crop_nutrient_mgmt$`Average holding area` <- as.numeric(as.character(data_frames$grass_crop_nutrient_mgmt$`Average holding area`))

# Round all numeric columns to 2 decimal places in each dataframe
data_frames <- lapply(data_frames, round_df)

# Join 'soil_nutrient_mgmt' and 'grass_crop_nutrient_mgmt' data frames
combined_nutrient_mgmt <- bind_rows(data_frames$soil_nutrient_mgmt, data_frames$grass_crop_nutrient_mgmt)

# Remove specified rows from the 'Soil nutrient management' column in the combined data frame
remove_entries <- c("Holdings with grassland", "Cropland holdings", "Holdings with grass or crops", 
                    "Soil testing resulted in change of crop nutrient application (those that performed soil testing)", 
                    "Uses protected urea")

combined_nutrient_mgmt <- combined_nutrient_mgmt %>% 
  filter(!`Soil nutrient management` %in% remove_entries)

# Assign the combined data frame back to the list
data_frames$combined_nutrient_mgmt <- combined_nutrient_mgmt

# Remove the original separate data frames
data_frames$soil_nutrient_mgmt <- NULL
data_frames$grass_crop_nutrient_mgmt <- NULL

# Save the data frames as a .RData file
save(list = names(data_frames), file = "module_2023.RData", envir = list2env(data_frames))


# 4. 2024 module (machinery) ---------------------------------------------------

# This data comes from the June Agricultural Census 2024:
# https://www.gov.scot/publications/results-from-the-scottish-agricultural-census-june-2024/

## This script looks at the modular vehicle data

# # subset for total vehicle numbers
# total_number_vehicles_data <- number_of_ag_mach_fuel_type %>%
#   select(`Agricultural machinery`, `All fuel types`) %>%
#   filter(`Agricultural machinery` %in% c("All-terrain vehicle (ATV)/Quads", "Combine harvesters",
#                                 "Other lifting equipment (such as wheeled loaders, diggers and fork-lifts)",
#                                 "Side-by-side utility vehicles", "Self-propelled sprayers", "Telescopic material handlers (such as telehandlers)",
#                                 "All tractors", "All agricultural machinery"))
# 
# # subset by farm type
# ag_mach_farm_type_data <- number_of_ag_mach_farm_type %>%
#   select(`Main farm type`, `All tractors`, `Combine harvesters`,
#          `Self-propelled sprayers`, `Telescopic material handlers`,
#          `All-terrain vehicle/Quads`, `Side-by-side utility vehicles`,
#          `Other lifting equipment`) %>%
#   filter(`Main farm type` %in% c("General cropping", "General cropping; forage", "LFA cattle and sheep",
#                                  "Mixed holdings", "Non-LFA cattle and sheep", "Specialist cereals",
#                                  "Specialist dairy", "Specialist horticulture & permanent crops",
#                                  "Specialist pigs", "Specialist poultry", "Unclassified", "Unknown"))
# # subset by ownership
# ag_mach_ownership_data <- number_of_ag_mach_ownership %>%
#   select(-`All ownership status`) %>%
#   filter(`Agricultural machinery` %in% c("All-terrain vehicle (ATV)/Quads", "Combine harvesters",
#                                          "Side-by-side utility vehicles", "Self-propelled sprayers", "Telescopic material handlers (such as telehandlers)",
#                                          "All tractors"))
# ag_mach_ownership_data <- ag_mach_ownership_data %>%
#   pivot_longer(cols = -`Agricultural machinery`, names_to = "Status", values_to = "Value") # Pivot wider to turn category rows into columns
# ag_mach_ownership_data <- ag_mach_ownership_data %>%
#   pivot_wider(names_from = `Agricultural machinery`, values_from = Value)
# 
# 
# # subset by fuel
# ag_mach_fuel_data <- number_of_ag_mach_fuel_type %>%
#   select(-`All fuel types`) %>%
#   filter(`Agricultural machinery` %in% c("All-terrain vehicle (ATV)/Quads", "Combine harvesters", "Other lifting equipment (such as wheeled loaders, diggers and fork-lifts)",
#                                          "Side-by-side utility vehicles", "Self-propelled sprayers", "Telescopic material handlers (such as telehandlers)",
#                                          "All tractors"))
# ag_mach_fuel_data <- ag_mach_fuel_data %>%
#   pivot_longer(cols = -`Agricultural machinery`, names_to = "Fuel type", values_to = "Value") # Pivot wider to turn category rows into columns
# ag_mach_fuel_data <- ag_mach_fuel_data %>%
#   pivot_wider(names_from = `Agricultural machinery`, values_from = Value)
# 
# # Saving all the subsets to an RData file
# save(
#   total_number_vehicles_data,
#   ag_mach_farm_type_data,
#   ag_mach_ownership_data,
#   ag_mach_fuel_data,
#   file = "vehicle_data.RData"
# )


# 5. 2025 module (irrigation) --------------------------------------------------

# # Wrangle drought and flood protection measures data
# irrigation_drought_flood_protection <- irrigation_drought_flood_protection %>%
#   filter(if_any(everything(), ~ grepl("[A-Za-z]", .))) %>% 
#   filter(!(`Irrigation, drought or flood protection measure` %in% c(
#     "Number of holdings that responded", "Total responses")))
# 
# # Wrangle irrigation methods data
# irrigation_methods <- irrigation_methods %>% 
#   filter(!(`Irrigation practice` %in% c(
#     "Number of holdings that responded", "Total responses")))
# 
# # Save data to environment
# save(
#   irrigation_methods,
#   irrigation_drought_flood_protection,
#   file = "Data/irrigation_2025.RData"
# )


# Constituency ----
# 
# xlsx_path <- "Data/constituency_data.xlsx"
# 
# # Get sheet names
# sheets <- excel_sheets(xlsx_path)
# 
# # Read each sheet into a named list of tibbles
# wb_list <- map(sheets, ~ read_excel(xlsx_path, sheet = .x, na = "c"))
# names(wb_list) <- sheets
# 
# safe_names <- make.names(names(wb_list))              # base R safe names
# safe_names <- str_replace_all(safe_names, "\\.", "_") # turn dots into underscores
# 
# # Assign to the current environment (or specify .GlobalEnv)
# list2env(setNames(wb_list, safe_names), envir = .GlobalEnv)

# Remove units from column names
# names(constituency_crops_area) <- names(constituency_crops_area) %>% 
#   str_replace_all(" \\(Hectares\\)", "")
# names(constituency_workforce_numbers) <- names(constituency_workforce_numbers) %>% 
#   str_replace_all(" \\(Number\\)", "")
# names(constituency_livestock_numbers) <- names(constituency_livestock_numbers) %>% 
#   str_replace_all(" \\(Number\\)", "")

# subsetting
# land_use_constituency <- constituency_crops_area %>%
#   rename(`Common grazings` = `Sole Right Grazing`) %>% 
#   select(Constituency, 
#          `Total crops, fallow, and set-aside`, 
#          `Total grass and Rough grazing`,
#          `Other land (including woodland)`,
#          `Total sole right agricultural area`,
#          `Common grazings`) %>%
#   pivot_longer(cols = -Constituency, names_to = "land use", values_to = "value") %>%
#   mutate(value = as.numeric(value)) %>% 
#   pivot_wider(names_from  = Constituency, values_from = value)
# 
# 
# workforce_constituency <- constituency_workforce_numbers %>%
#   select(Constituency, `Regular Full-Time Staff Total`,
#          `Regular Part-Time Staff Total`, `Total Casual And Seasonal Staff`,
#          `Total Workforce (including occupiers)`) %>%
#   pivot_longer(cols = -Constituency, names_to = "workforce", values_to = "value") %>%
#   mutate(value = as.numeric(value)) %>% 
#   pivot_wider(names_from  = Constituency, values_from = value)
# 
# occupiers_constituency <- constituency_workforce_numbers %>%
#   select(Constituency, `Total Working Occupiers`, `Occupiers Not Working On The Holding`) %>%
#   pivot_longer(cols = -Constituency, names_to = "occupier", values_to = "value") %>%
#   mutate(value = as.numeric(value)) %>% 
#   pivot_wider(names_from  = Constituency, values_from = value)
# 
# cattle_constituency <- constituency_livestock_numbers %>%
#   select(Constituency, `Total cattle`, `Total Female Dairy Cattle`,
#          `Total Female Beef Cattle`, `Total Male Cattle`,
#          `Total Calves`) %>%
#   pivot_longer(cols = -Constituency, names_to = "livestock", values_to = "value") %>%
#   mutate(value = as.numeric(value)) %>% 
#   pivot_wider(names_from  = Constituency, values_from = value)
# 
# sheep_constituency <- constituency_livestock_numbers %>%
#   select(Constituency, `Total sheep`, `Ewes for breeding`,
#          `Other sheep 1 year and over for breeding`, `Rams for service`,
#          `Lambs`) %>%
#   pivot_longer(cols = -Constituency, names_to = "livestock", values_to = "value") %>%
#   mutate(value = as.numeric(value)) %>% 
#   pivot_wider(names_from  = Constituency, values_from = value)
# 
# pigs_constituency <- constituency_livestock_numbers %>%
#   select(Constituency, `Total pigs`, `Female pigs breeding herd`,
#          `All other non-breeding pigs`) %>%
#   pivot_longer(cols = -Constituency, names_to = "livestock", values_to = "value") %>%
#   mutate(value = as.numeric(value)) %>% 
#   pivot_wider(names_from  = Constituency, values_from = value)
# 
# poultry_constituency <- constituency_livestock_numbers %>%
#   select(Constituency, `Total poultry`, `Fowls for producing eggs`,
#          `Fowls for breeding`, `Broilers and other table fowls and other poultry`) %>%
#   pivot_longer(cols = -Constituency, names_to = "livestock", values_to = "value") %>%
#   mutate(value = as.numeric(value)) %>% 
#   pivot_wider(names_from  = Constituency, values_from = value)
# 
# other_animals_constituency <- constituency_livestock_numbers %>%
#   select(Constituency, `Goats and kids`, `Deer`, `Horses`,
#          `Donkeys`, `Camelids`, `Beehives`) %>%
#   pivot_longer(cols = -Constituency, names_to = "livestock", values_to = "value") %>%
#   mutate(value = as.numeric(value)) %>% 
#   pivot_wider(names_from  = Constituency, values_from = value)
# 
# cereals_constituency <- constituency_crops_area %>%
#   select(Constituency, `Wheat`, `Winter barley`, 
#          `Spring barley`, `Oats and mixed grain`) %>%
#   pivot_longer(cols = -Constituency, names_to = "crop", values_to = "value") %>%
#   mutate(value = as.numeric(value)) %>% 
#   pivot_wider(names_from  = Constituency, values_from = value)
# 
# oilseeds_constituency <- constituency_crops_area %>%
#   select(Constituency, `Oilseeds (including linseed)`) %>%
#   pivot_longer(cols = -Constituency, names_to = "crop", values_to = "value") %>%
#   mutate(value = as.numeric(value)) %>% 
#   pivot_wider(names_from  = Constituency, values_from = value)
# 
# potatoes_constituency <- constituency_crops_area %>%
#   select(Constituency, `Potatoes`) %>%
#   pivot_longer(cols = -Constituency, names_to = "crop", values_to = "value") %>%
#   mutate(value = as.numeric(value)) %>% 
#   pivot_wider(names_from  = Constituency, values_from = value)
# 
# peas_beans_constituency <- constituency_crops_area %>%
#   select(Constituency, `Peas and beans for combining`) %>%
#   pivot_longer(cols = -Constituency, names_to = "crop", values_to = "value") %>%
#   mutate(value = as.numeric(value)) %>% 
#   pivot_wider(names_from  = Constituency, values_from = value)
# 
# stockfeeding_constituency <- constituency_crops_area %>%
#   select(Constituency, `Stockfeeding crops`) %>%
#   pivot_longer(cols = -Constituency, names_to = "crop", values_to = "value") %>%
#   mutate(value = as.numeric(value)) %>% 
#   pivot_wider(names_from  = Constituency, values_from = value)
# 
# vegetables_constituency <- constituency_crops_area %>%
#   select(Constituency, `Vegetables for human consumptioncrops`) %>%
#   pivot_longer(cols = -Constituency, names_to = "crop", values_to = "value") %>%
#   mutate(value = as.numeric(value)) %>% 
#   pivot_wider(names_from  = Constituency, values_from = value)
# 
# fruit_constituency <- constituency_crops_area %>%
#   select(Constituency, `Orchard and soft fruit`) %>%
#   pivot_longer(cols = -Constituency, names_to = "crop", values_to = "value") %>%
#   mutate(value = as.numeric(value)) %>% 
#   pivot_wider(names_from  = Constituency, values_from = value)
# 
# save(
#   land_use_constituency,
#   workforce_constituency,
#   occupiers_constituency,
#   cattle_constituency,
#   sheep_constituency,
#   pigs_constituency,
#   poultry_constituency,
#   other_animals_constituency,
#   cereals_constituency,
#   oilseeds_constituency,
#   potatoes_constituency,
#   peas_beans_constituency,
#   stockfeeding_constituency,
#   vegetables_constituency,
#   fruit_constituency,
#   
#   file = "Data/constituency_data.RData"
# )
