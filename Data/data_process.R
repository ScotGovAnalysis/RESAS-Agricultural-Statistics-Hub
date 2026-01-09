## data.R

#########################################
###
### Data manipulation within this section:
### 
### >>> Census data
###     > Further crops processing
###     > Animal summary processing
### >>> Emissions data
### >>> 
### >>> 2023 census module data
###
### --------------------------------
###
### How to update data:
###
### Individual instructions are available for each section. 
###
### Inputted will need to be in the same format as previous iterations unless
### code is edited to adapt this. 
###
### Current data within the section primarily comes from the publicly available publication tables.
###
### Download the most recent tables from the publication, add them to the project directory, and modify the file path.
###
### Data processing is designed to be robust, but small changes within publications will likely lead to bugs needing fixed
###
### Ensure when updating census information that the all subsections are updated below.
###
#########################################

# Libraries:


library(readxl)
library(openxlsx)
library(dplyr)
library(tidyr)
library(stringr)
library(highcharter)
library(ggthemes)

#########################################
###
###
### Census Data: 
###
### Xlsx file originates from the published data tables in supporting documents
### 2023 version: https://www.gov.scot/publications/results-scottish-agricultural-census-june-2024/documents/
###
### To update, download the excel document and insert file into compendium working directory / file
### Insert file path in the file_path below.
### Check table_names for changes vs. the previous year's names and order
### If the order is incorrect, change the table number accordingly
###
### Try to avoid renaming tables below unless necessary as these are referenced throughout the app
### If changing any names, ensure all sourcing of the data matches changing (print_code.R can help to QA this)
###
### The data processing was designed for the 2023 census tables, and changes to tables may cause bugs
### Processing aims to standardise all tables to make them compatible with the different visualisation modules
### This includes removing the metadata above the tables, renaming headers, removing problematic trailing spaces, etc.
###
### Additional processing can be included if necessary
### Some data processing could be moved from within the server pages in the app to speed up loading & improve efficiency
###
### To update, run the code below.
### QA results before saving as RData
### RData is then loaded within options.RData
###
### Ensure the rest of the census code is ran subsequently after updating the base tables.
###
#########################################

# Define file path
file_path <- "Data/June Agricultural Census 2025 Data tables.xlsx"




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
  "occupiers_sex" = "Table_9",
  "occupiers_age_gender" = "Table_10",
  "legal_responsibility" = "Table_11",
  "owned_rented_land" = "Table_12",
  "farm_type" = "Table_13",
  "agricultural_area_lfa" = "Table_14",
  "holdings_crops_grass_subregion" = "Table_15",
  "crops_grass_area_subregion" = "Table_16",
  "holdings_livestock_region_subregion" = "Table_17",
  "livestock_subregion" = "Table_18",
  "holdings_occupiers_employees_subregion" = "Table_19",
  "occupiers_employees_subregion" = "Table_20",
  "holdings_region_subregion_farm_type" = "Table_21",
  "agricultural_area_region_subregion_farm_type" = "Table_22",
  "holdings_area_size_band_farm_type" = "Table_23",
  "agricultural_area_area_size_band_farm_type" = "Table_24",
  "irrigation_methods" = "Module_2025_Table_1",
  "irrigation_drought_flood_protection" = "Module_2025_Table_2"
)
# Function to remove rows until the first occurrence of "Source:" in the first column
remove_until_source <- function(data) {
  # Remove the first row (which is now the original header row)
  data <- data[-1, ]
  
  # Now remove rows until the first occurrence of "Source:"
  source_row <- which(str_detect(data[[1]], "^Source:"))
  if (length(source_row) > 0) {
    data <- data[(source_row + 1):nrow(data), ]
  }
  
  return(data)
}

# Function to remove columns and rows that are all NAs
clean_data <- function(data) {
  data <- data[, colSums(is.na(data)) < nrow(data)]  # Remove columns with all NAs
  data <- data[rowSums(is.na(data)) < ncol(data), ]  # Remove rows with all NAs
  return(data)
}

# Function to clean header names by removing text within brackets, any '\r\n', extra spaces, and specific region names
clean_headers <- function(headers) {
  headers <- str_replace_all(headers, "\\s*\\([^\\)]+\\)", "")  # Remove text within brackets
  headers <- str_replace_all(headers, "\r\n", " ")  # Remove \r\n
  headers <- str_replace_all(headers, "\\s+", " ")  # Replace multiple spaces with a single space
  headers <- str_replace_all(headers, "\\b(North West|North East|South East|South West)\\b", "")  # Remove specific region names
  headers <- str_trim(headers)  # Trim again to remove any resulting leading or trailing spaces
  
  # Make headers unique
  headers <- make.unique(headers, sep = "_")
  
  return(headers)
}

# Function to clean cell values by replacing multiple spaces with a single space
clean_cells <- function(data) {
  data <- data %>% mutate(across(where(is.character), ~str_replace_all(.x, "\\s+", " ")))
  return(data)
}

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

# Change Sole Right Grazing to Rough Grazing
crops_grass_area_subregion <- crops_grass_area_subregion %>%
  mutate(`Land use by category` = if_else(`Land use by category` == "Sole Right Grazing", "Rough Grazing", `Land use by category`))

holdings_crops_grass_subregion <- holdings_crops_grass_subregion %>%
  mutate(`Land use by category` = if_else(`Land use by category` == "Sole Right Grazing", "Rough Grazing", `Land use by category`))

# Remove % change column
owned_rented_land <- owned_rented_land %>%
  select(-`% Change 2025 to 2024`)
occupiers_employees <- occupiers_employees %>%
  select(-`% Change 2025 to 2024`)

# Save all tables to an RData file
save(list = names(table_names), file = "Data/census_data.RData")

# load fertiliser data from june 2023
file_path <- "Data/June+Agricultural+Census+2023+Tables.xlsx"
manure_fertiliser <- read_excel(file_path, sheet = "Table_21")
save(manure_fertiliser, file = "manure_fertiliser.RData")

# load to test
#load("census_data.RData")

#################################
###
### Processing for Crops / Fruit / Veg
###
### Run the code below to update
###
### Run whenever census data is updated
###
#################################

# Print unique values 

unique(agricultural_area_hectares$`Crop/Land use`)

unique(vegetables_bulbs_fruit_area$`Vegetables and fruits for human consumption`)

unique(crops_grass_area_subregion$`Land use by category`)


names(agricultural_area_hectares) <- names(agricultural_area_hectares) %>%
  str_replace_all("Area", "") %>%
  str_trim

names(vegetables_bulbs_fruit_area) <- names(vegetables_bulbs_fruit_area) %>%
  str_replace_all("Area", "") %>%
  str_trim

# Preprocess and round the summary crops data
crops_summary_data <- agricultural_area_hectares %>%
  select(-`% Change 2025 to 2024`) %>%
  filter(`Crop/Land use` %in% c("Total Combine Harvested Crops", "Total Crops For Stockfeeding",
                                "Vegetables For Human Consumption", "Soft Fruit")) %>%
  pivot_longer(
    cols = -`Crop/Land use`,
    names_to = "Year",
    values_to = "Value"
  ) %>%
  mutate(
    Year = as.numeric(Year),  # Ensure Year is numeric
    Value = if_else(
      abs(as.numeric(Value)) >= 1000,  # Round up for numbers with more than 3 significant figures
      round(as.numeric(Value)),
      round(as.numeric(Value), digits = 2)
    )
  )

land_use_data <- agricultural_area_hectares %>% 
  select(-`% Change 2025 to 2024`) %>%
  filter(`Crop/Land use` %in% c("Total Crops, Fallow, And Set-Aside", "Total Grass", 
                                "Rough Grazing", "Total Sole Right Agricultural Area", "Common Grazings"))


land_use_subregion <- crops_grass_area_subregion %>%
  filter(`Land use by category` %in% c("Total Agricultural Area", "Total Sole Right Agricultural Area", "Total Grass and Rough Grazing", 
                                       "Sole Right Grazing", "Total Crops, Fallow, And Set-Aside", "Common Grazings", "Other Land (including Woodland)"))


# Subset for cereals data
cereals_data <- agricultural_area_hectares %>%
  select(-`% Change 2025 to 2024`) %>%
  filter(`Crop/Land use` %in% c("Wheat", "Triticale", "Winter Barley", "Spring Barley", "Barley Total", 
                                "Winter Oats", "Spring Oats", "Oats Total", "Rye", "Mixed Grain", 
                                "Total Cereals"))

# Subset for oilseeds data
oilseed_data <- agricultural_area_hectares %>%
  select(-`% Change 2025 to 2024`) %>%
  filter(`Crop/Land use` %in% c("Winter Oilseed Rape", "Spring Oilseed Rape", "Linseed", "Total Oilseeds")) %>%
  mutate(`2025` = round(`2025`, 0))

# Subset for potatoes data
potatoes_data <- agricultural_area_hectares %>%
  select(-`% Change 2025 to 2024`) %>%
  filter(`Crop/Land use` %in% c("Seed Potatoes", "Ware Potatoes", "Total Potatoes")) %>%
  mutate(`2025` = round(`2025`, 0))

# Subset for beans data
beans_data <- agricultural_area_hectares %>%
  select(-`% Change 2025 to 2024`) %>%
  filter(`Crop/Land use` %in% c("Protein Peas", "Field Beans")) %>%
  mutate(`2025` = round(`2025`, 0))

# Subset for animal feed data
stockfeeding_data <- agricultural_area_hectares %>%
  select(-`% Change 2025 to 2024`) %>%
  filter(`Crop/Land use` %in% c("Turnips/Swedes", "Kale/Cabbage", "Maize", "Rape", "Fodder Beet", 
                                "Lupins", "Other Crops For Stockfeeding", "Total Crops For Stockfeeding"))


stockfeeding_data <- stockfeeding_data %>%
  mutate(`2025` = round(`2025`, 0))

# Subset for human vegetables data
human_vegetables_data <- vegetables_bulbs_fruit_area %>%
  select(-`% Change 2025 to 2024`) %>%
  filter(`Vegetables and fruits for human consumption` %in% c(
    "Peas For Canning, Freezing Or Drying",
    "Beans For Canning, Freezing Or Drying",
    "Turnips/Swedes",
    "Calabrese",
    "Cauliflower",
    "Carrots",
    "Other Vegetables",
    "Total Vegetables"
  )) %>%
  mutate(`2025` = round(`2025`, 0))

# Subset for soft fruit data
fruit_data <- vegetables_bulbs_fruit_area %>%
  select(-`% Change 2025 to 2024`) %>%
  filter(`Vegetables and fruits for human consumption` %in% c(
    "Strawberries Grown In The Open",
    "Raspberries Grown In The Open",
    "Blueberries Grown In The Open",
    "Blackcurrants And Other Fruit Grown In The Open",
    "Total Soft Fruit Grown In The Open",
    "Tomatoes Grown Under Cover",
    "Strawberries Grown Under Cover",
    "Raspberries Grown Under Cover",
    "Blueberries Grown Under Cover",
    "Other Fruit Grown Under Cover",
    "Vegetables Grown Under Cover",
    "Strawberries Grown In Open/Under Cover",
    "Raspberries Grown In Open/Under Cover",
    "Blackcurrants Grown In Open/Under Cover",
    "Blueberries Grown In Open/Under Cover",
    "Tomatoes Grown In Open/Under Cover",
    "Other Fruit Grown In Open/Under Cover",
    "Total Soft Fruit"
  )) %>%
  mutate(`2025` = round(`2025`, 0))
# Subset for cereals_subregion
cereals_subregion <- crops_grass_area_subregion %>%
  filter(`Land use by category` %in% c(
    "Wheat",
    "Winter Barley",
    "Spring Barley",
    "Barley Total",
    "Oats and Mixed Grain"
  )) %>%
  mutate(across(where(is.numeric), ~ round(.x, 0)))

# Subset for oilseed_subregion
oilseed_subregion <- crops_grass_area_subregion %>%
  filter(`Land use by category` %in% c(
    "Oilseeds (including Linseed)"
  )) %>%
  mutate(across(where(is.numeric), ~ round(.x, 0)))

# Subset for potato_subregion
potatoes_subregion <- crops_grass_area_subregion %>%
  filter(`Land use by category` %in% c(
    "Potatoes"
  )) %>%
  mutate(across(where(is.numeric), ~ round(.x, 0)))

# Subset for beans_subregion
beans_subregion <- crops_grass_area_subregion %>%
  filter(`Land use by category` %in% c(
    "Peas and Beans for Combining"
  )) %>%
  mutate(across(where(is.numeric), ~ round(.x, 0)))

# Subset for stockfeeding_subregion
stockfeeding_subregion <- crops_grass_area_subregion %>%
  filter(`Land use by category` %in% c(
    "Stockfeeding Crops"
  )) %>%
  mutate(across(where(is.numeric), ~ round(.x, 0)))

# Subset for human_veg_subregion
human_vegetables_subregion <- crops_grass_area_subregion %>%
  filter(`Land use by category` %in% c(
    "Vegetables For Human Consumption"
  )) %>%
  mutate(across(where(is.numeric), ~ round(.x, 0)))

# Subset for fruit_subregion
fruit_subregion <- crops_grass_area_subregion %>%
  filter(`Land use by category` %in% c(
    "Orchard and Soft Fruit"
  )) %>%
  mutate(across(where(is.numeric), ~ round(.x, 0)))

# Saving all the subsets to an RData file
save(
  crops_summary_data,
  land_use_data,
  land_use_subregion,
  cereals_data,
  oilseed_data,
  potatoes_data,
  beans_data,
  stockfeeding_data,
  human_vegetables_data,
  fruit_data,
  cereals_subregion,
  oilseed_subregion,
  potatoes_subregion,
  beans_subregion,
  stockfeeding_subregion,
  human_vegetables_subregion,
  fruit_subregion,
  
  file = "Data/crops_data.RData"
)

### Animals summary data - Requires census tables

load("Data/census_data.RData")

# Remove % change columns
number_of_cattle <- number_of_cattle %>%
  select(-`% Change 2025 to 2024`)

number_of_sheep <- number_of_sheep %>%
  select(-`% Change 2025 to 2024`)

number_of_pigs <- number_of_pigs %>%
  select(-`% Change 2025 to 2024`)

number_of_poultry <- number_of_poultry %>% 
  select(-`% Change 2025 to 2024`)

number_of_other_livestock <- number_of_other_livestock %>%
  select(-`% Change 2025 to 2024`)

# Convert the wide format data into long format using pivot_longer
number_of_pigs_long <- number_of_pigs %>%
  pivot_longer(cols = -`Pigs by category`, names_to = "Year", values_to = "Total") %>%
  filter(`Pigs by category` == "Total Pigs") %>%
  select(Year, `Total Pigs` = Total)

number_of_poultry_long <- number_of_poultry %>%
  pivot_longer(cols = -`Poultry by category`, names_to = "Year", values_to = "Total") %>%
  filter(`Poultry by category` == "Total Poultry") %>%
  select(Year, `Total Poultry` = Total)

number_of_sheep_long <- number_of_sheep %>%
  pivot_longer(cols = -`Sheep by category`, names_to = "Year", values_to = "Total") %>%
  filter(`Sheep by category` == "Total Sheep") %>%
  select(Year, `Total Sheep` = Total)

number_of_cattle_long <- number_of_cattle %>%
  pivot_longer(cols = -`Cattle by category`, names_to = "Year", values_to = "Total") %>%
  filter(`Cattle by category` == "Total Cattle") %>%
  select(Year, `Total cattle` = Total)

# Merge the dataframes on the 'Year' column
total_animals <- number_of_pigs_long %>%
  inner_join(number_of_poultry_long, by = "Year") %>%
  inner_join(number_of_sheep_long, by = "Year") %>%
  inner_join(number_of_cattle_long, by = "Year")

# Convert Year column to numeric
total_animals$Year <- as.numeric(total_animals$Year)

# save total animals 
save(total_animals, file = "Data/total_animals.RData")



#########################################################################
### 
###
### Emissions Data Processing
###
###
##########################################################################

#agri_gas = Gas Breakdown of Emissions from Scottish agriculture greenhouse gas emissions and nitrogen use
#subsector_total = Agricultural emissions broken down by subsector from Scottish agriculture greenhouse gas emissions and nitrogen use
#subsector_source = Breakdown of subsectors by source from Scottish agriculture greenhouse gas emissions and nitrogen use
#national_total = Yearly breakdown of Scottish Emissions from Scottish Greenhouse Gas Emissions publication

# As of 2022-23 emissions publication, these were made available in excel sheet before being inputted into R
# This could be repeated, or data could be read in from the difference sources, and manipulated into the same format

# To update data, replace file path and run the below script.

# Load  data from the Excel file
#file_path <- "C:/Users/U456727/OneDrive - SCOTS Connect/Economic Statistics/FBS/GHG 2023-24/Sectoral analysis/GHG inventory data 2023.xlsx"

agri_gas <- read_excel(file_path, sheet = "agri_gas")
national_total <- read_excel(file_path, sheet = "national_total")
subsector_total <- read_excel(file_path, sheet = "subsector_total")
subsector_source <- read_excel(file_path, sheet = "subsector_source")

agri_gas <- agri_gas %>% 
  rename(Gas = ...1)

# Reshape the data to long format
agri_gas <- agri_gas %>% pivot_longer(cols = -Gas, names_to = "Year", values_to = "Value")
national_total <- national_total %>% pivot_longer(cols = -Industry, names_to = "Year", values_to = "Value")
subsector_total <- subsector_total %>% pivot_longer(cols = -Subsector, names_to = "Year", values_to = "Value")

# Convert Year to numeric
agri_gas$Year <- as.numeric(agri_gas$Year)
national_total$Year <- as.numeric(national_total$Year)
subsector_total$Year <- as.numeric(subsector_total$Year)

# Merge the specified sources into 'Other emission source'
subsector_source <- subsector_source %>% filter(!Source %in% c("Urea application", "Non-energy products from fuels and solvent use"))
  # mutate(
  #   Source = ifelse(Source %in% c("Urea application", "Non-energy products from fuels and solvent use"), 
  #                   "Other emission source", 
  #                   Source)
  # ) %>%
  # group_by(Source) %>%
  # summarise(across(everything(), sum)) %>%
  # ungroup()

save(subsector_total, agri_gas, national_total, subsector_source, file = "ghg_data.RData")

# load data to test
#load("ghg_data.RData")





################################################################################

###############################
####
#### Module 2023 data
####
#### This data comes from the 2023 Agricultural Census Module Results
####
#### https://www.gov.scot/publications/results-from-the-scottish-agricultural-census-module-june-2023/
####
#### The script extracts tables looking at soil management, fertiliser usage, manure, nitrogen
#### and formats the data into formatting to be used in the modules.
#### This minimises the amount of data processing done within the R Shiny app, improving running efficiency. 
####
#### This script should not need re-run as the module data will not be updated, though can be used
#### as a baseline to include future year's module data.
####
#################################

# Load necessary libraries
library(readxl)
library(stringr)
library(dplyr)

# Define the file path
file_path <- "Data/June+Agricultural+Census+2023+-+Module+Report+-+Production+methods+and+nutrient+application+-+Tables.xlsx"

# Define the sheet names to read
sheets_to_read <- c("Table_4", "Table_5", "Table_7", "Table_8", "Table_9", "Table_12")

# Define shortened names for the dataframes
short_names <- c("soil_nutrient_mgmt", "grass_crop_nutrient_mgmt", "nitrogen_250", 
                 "nitrogen_400", "manure_qty", "fertiliser_use")

# Function to clean header names by removing text within brackets, any '\r\n', extra spaces, and specific region names
clean_headers <- function(headers) {
  headers <- str_replace_all(headers, "\\s*\\([^\\)]+\\)", "")  # Remove text within brackets
  headers <- str_replace_all(headers, "\r\n", " ")  # Remove \r\n
  headers <- str_replace_all(headers, "\\s+", " ")  # Replace multiple spaces with a single space
  headers <- str_replace_all(headers, "\\b(North West|North East|South East|South West)\\b", "")  # Remove specific region names
  headers <- str_trim(headers)  # Trim again to remove any resulting leading or trailing spaces
  return(headers)
}

# Function to read and process each sheet
read_and_process_sheet <- function(sheet) {
  df <- read_excel(file_path, sheet = sheet, skip = 5)  # Skip the first 5 rows
  colnames(df) <- clean_headers(colnames(df))  # Clean the headers
  return(df)
}

# Function to round all numeric columns to 2 decimal places
round_df <- function(df) {
  df[] <- lapply(df, function(x) if(is.numeric(x)) round(x, 2) else x)
  return(df)
}

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

################################


#### Module 2024 Data ####

#### This data comes from the June Agricultural Census 2024
####
#### https://www.gov.scot/publications/results-from-the-scottish-agricultural-census-june-2024/
####
#### This script looks at the modular vehicle data



# subset for total vehicle numbers
total_number_vehicles_data <- number_of_ag_mach_fuel_type %>%
  select(`Agricultural machinery`, `All fuel types`) %>%
  filter(`Agricultural machinery` %in% c("All-terrain vehicle (ATV)/Quads", "Combine harvesters",
                                "Other lifting equipment (such as wheeled loaders, diggers and fork-lifts)",
                                "Side-by-side utility vehicles", "Self-propelled sprayers", "Telescopic material handlers (such as telehandlers)",
                                "All tractors", "All agricultural machinery"))

# subset by farm type
ag_mach_farm_type_data <- number_of_ag_mach_farm_type %>%
  select(`Main farm type`, `All tractors`, `Combine harvesters`,
         `Self-propelled sprayers`, `Telescopic material handlers`,
         `All-terrain vehicle/Quads`, `Side-by-side utility vehicles`,
         `Other lifting equipment`) %>%
  filter(`Main farm type` %in% c("General cropping", "General cropping; forage", "LFA cattle and sheep",
                                 "Mixed holdings", "Non-LFA cattle and sheep", "Specialist cereals",
                                 "Specialist dairy", "Specialist horticulture & permanent crops",
                                 "Specialist pigs", "Specialist poultry", "Unclassified", "Unknown"))
# subset by ownership
ag_mach_ownership_data <- number_of_ag_mach_ownership %>%
  select(-`All ownership status`) %>%
  filter(`Agricultural machinery` %in% c("All-terrain vehicle (ATV)/Quads", "Combine harvesters",
                                         "Side-by-side utility vehicles", "Self-propelled sprayers", "Telescopic material handlers (such as telehandlers)",
                                         "All tractors"))
ag_mach_ownership_data <- ag_mach_ownership_data %>%
  pivot_longer(cols = -`Agricultural machinery`, names_to = "Status", values_to = "Value") # Pivot wider to turn category rows into columns
ag_mach_ownership_data <- ag_mach_ownership_data %>%
  pivot_wider(names_from = `Agricultural machinery`, values_from = Value)


# subset by fuel
ag_mach_fuel_data <- number_of_ag_mach_fuel_type %>%
  select(-`All fuel types`) %>%
  filter(`Agricultural machinery` %in% c("All-terrain vehicle (ATV)/Quads", "Combine harvesters", "Other lifting equipment (such as wheeled loaders, diggers and fork-lifts)",
                                         "Side-by-side utility vehicles", "Self-propelled sprayers", "Telescopic material handlers (such as telehandlers)",
                                         "All tractors"))
ag_mach_fuel_data <- ag_mach_fuel_data %>%
  pivot_longer(cols = -`Agricultural machinery`, names_to = "Fuel type", values_to = "Value") # Pivot wider to turn category rows into columns
ag_mach_fuel_data <- ag_mach_fuel_data %>%
  pivot_wider(names_from = `Agricultural machinery`, values_from = Value)

# Saving all the subsets to an RData file
save(
  total_number_vehicles_data,
  ag_mach_farm_type_data,
  ag_mach_ownership_data,
  ag_mach_fuel_data,
  file = "vehicle_data.RData"
)


#### Module 2025 Data ####
irrigation_drought_flood_protection <- irrigation_drought_flood_protection %>%
  filter(if_any(everything(), ~ grepl("[A-Za-z]", .)))
save(
  irrigation_methods,
  irrigation_drought_flood_protection,
  file = "Data/irrigation_2025.RData"
)



#### Constituency ####

library(readxl)
library(purrr)

xlsx_path <- "Data/constituency_data.xlsx"

# Get sheet names
sheets <- excel_sheets(xlsx_path)

# Read each sheet into a named list of tibbles
wb_list <- map(sheets, ~ read_excel(xlsx_path, sheet = .x))
names(wb_list) <- sheets

safe_names <- make.names(names(wb_list))              # base R safe names
safe_names <- str_replace_all(safe_names, "\\.", "_") # turn dots into underscores

# 2) Assign to the current environment (or specify .GlobalEnv)
list2env(setNames(wb_list, safe_names), envir = .GlobalEnv)


# subsetting

land_use_constituency <- constituency_crops_area %>%
  select(Constituency, `Total Crops, Fallow, And Set-Aside (Hectares)`, `Total Grass and Rough Grazing (Hectares)`,
         `Other Land (including woodland) (Hectares)`) %>%
  pivot_longer(cols = -Constituency, names_to = "land use", values_to = "value") %>%
  pivot_wider(names_from  = Constituency, values_from = value)


workforce_constituency <- constituency_workforce_numbers %>%
  select(Constituency, `Regular Full-Time Staff Total (Number)`,
         `Regular Part-Time Staff Total (Number)`, `Total Casual And Seasonal Staff (Number)`,
         `Total Agricultural Workforce (Number)`) %>%
  pivot_longer(cols = -Constituency, names_to = "workforce", values_to = "value") %>%
  pivot_wider(names_from  = Constituency, values_from = value)

occupiers_constituency <- constituency_workforce_numbers %>%
  select(Constituency, `Total Working Occupiers (Number)`, `Occupiers Not Working On The Holding (Number)`) %>%
  pivot_longer(cols = -Constituency, names_to = "occupier", values_to = "value") %>%
  pivot_wider(names_from  = Constituency, values_from = value)

cattle_constituency <- constituency_livestock_numbers %>%
  select(Constituency, `Total Cattle (Number)`, `Total Female Dairy Cattle (Number)`,
         `Total Female Beef Cattle (Number)`, `Total Male Cattle (Number)`,
         `Total Calves (Number)`) %>%
  pivot_longer(cols = -Constituency, names_to = "livestock", values_to = "value") %>%
  pivot_wider(names_from  = Constituency, values_from = value)

sheep_constituency <- constituency_livestock_numbers %>%
  select(Constituency, `Total Sheep (Number)`, `Ewes for breeding (Number)`,
         `Other sheep 1 year and over for breeding (Number)`, `Rams for service (Number)`,
         `Lambs (Number)`) %>%
  pivot_longer(cols = -Constituency, names_to = "livestock", values_to = "value") %>%
  pivot_wider(names_from  = Constituency, values_from = value)

pigs_constituency <- constituency_livestock_numbers %>%
  select(Constituency, `Total Pigs (Number)`, `Female pigs breeding herd (Number)`,
         `All other non-breeding pigs (Number)`) %>%
  pivot_longer(cols = -Constituency, names_to = "livestock", values_to = "value") %>%
  pivot_wider(names_from  = Constituency, values_from = value)

poultry_constituency <- constituency_livestock_numbers %>%
  select(Constituency, `Total Poultry (Number)`, `Fowls for producing eggs (Number)`,
         `Fowls for breeding (Number)`, `Broilers and other table fowls and other poultry (Number)`) %>%
  pivot_longer(cols = -Constituency, names_to = "livestock", values_to = "value") %>%
  pivot_wider(names_from  = Constituency, values_from = value)

other_animals_constituency <- constituency_livestock_numbers %>%
  select(Constituency, `Goats and kids (Number)`, `Deer (Number)`, `Horses (Number)`,
         `Donkeys (Number)`, `Camelids (Number)`, `Beehives (Number)`) %>%
  pivot_longer(cols = -Constituency, names_to = "livestock", values_to = "value") %>%
  pivot_wider(names_from  = Constituency, values_from = value)

cereals_constituency <- constituency_crops_area %>%
  select(Constituency, `Wheat (Hectares)`, `Winter Barley (Hectares)`, 
         `Spring Barley (Hectares)`, `Oats and mixed grain (Hectares)`) %>%
  pivot_longer(cols = -Constituency, names_to = "crop", values_to = "value") %>%
  pivot_wider(names_from  = Constituency, values_from = value)

oilseeds_constituency <- constituency_crops_area %>%
  select(Constituency, `Oilseeds (Including Linseed) (Hectares)`) %>%
  pivot_longer(cols = -Constituency, names_to = "crop", values_to = "value") %>%
  pivot_wider(names_from  = Constituency, values_from = value)

potatoes_constituency <- constituency_crops_area %>%
  select(Constituency, `Potatoes (Hectares)`) %>%
  pivot_longer(cols = -Constituency, names_to = "crop", values_to = "value") %>%
  pivot_wider(names_from  = Constituency, values_from = value)

peas_beans_constituency <- constituency_crops_area %>%
  select(Constituency, `Peas and Beans for Combining (Hectares)`) %>%
  pivot_longer(cols = -Constituency, names_to = "crop", values_to = "value") %>%
  pivot_wider(names_from  = Constituency, values_from = value)

stockfeeding_constituency <- constituency_crops_area %>%
  select(Constituency, `Stockfeeding Crops (Hectares)`) %>%
  pivot_longer(cols = -Constituency, names_to = "crop", values_to = "value") %>%
  pivot_wider(names_from  = Constituency, values_from = value)

vegetables_constituency <- constituency_crops_area %>%
  select(Constituency, `Vegetables For Human Consumption (Hectares)`) %>%
  pivot_longer(cols = -Constituency, names_to = "crop", values_to = "value") %>%
  pivot_wider(names_from  = Constituency, values_from = value)

fruit_constituency <- constituency_crops_area %>%
  select(Constituency, `Orchard and soft fruit (Hectares)`) %>%
  pivot_longer(cols = -Constituency, names_to = "crop", values_to = "value") %>%
  pivot_wider(names_from  = Constituency, values_from = value)

save(
  land_use_constituency,
  workforce_constituency,
  occupiers_constituency,
  cattle_constituency,
  sheep_constituency,
  pigs_constituency,
  poultry_constituency,
  other_animals_constituency,
  cereals_constituency,
  oilseeds_constituency,
  potatoes_constituency,
  peas_beans_constituency,
  stockfeeding_constituency,
  vegetables_constituency,
  fruit_constituency,
  
  file = "Data/constituency_data.RData"
)
