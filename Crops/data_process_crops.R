main_cereals <- cereals_data %>%
  filter(`Crop/Land use` %in% c("Wheat", "Barley Total", "Oats Total")) %>%
  summarise(across(-`Crop/Land use`, ~ sum(.x, na.rm = TRUE))) %>%
  mutate(`Crop/Land use` = "Main Cereals (Barley, Oats and Wheat)") 


# Bind back into the wide dataset
cereals_data_census <- bind_rows(cereals_data, main_cereals)%>% 
  distinct(`Crop/Land use`, .keep_all = TRUE)%>%
  mutate(`Crop/Land use` = recode(`Crop/Land use`,
                                  "Barley Total" = "Total Barley",
                                  "Oats Total"   = "Total Oats"
                                  ))
save(cereals_data_census, file="Data/cereals_data_census.Rda")

Cereals_census_data_long <- cereals_data_census %>% 
  pivot_longer(
    cols = -`Crop/Land use`,
    names_to = "Year",
    values_to = "Value"
  ) %>% 
  mutate(
    Measure = "Area",
    `Crop/Land use` = as.factor(`Crop/Land use`),  
    Measure = as.factor(Measure),
    Year = as.integer(Year)
  )

Oilseed_census_data_long <- oilseed_data %>% 
  pivot_longer(
    cols = -`Crop/Land use`,
    names_to = "Year",
    values_to = "Value"
  ) %>% 
  mutate(Measure = "Area",
         `Crop/Land use` = as.factor(`Crop/Land use`),  
         Measure = as.factor(Measure),
         Year = as.integer(Year)
  )


cereals_tiff_data_path <- "//s0196a/ADM-Rural and Environmental Science-Farming Statistics/Agriculture/Source/TIFF/Cereals/"
file_path <- paste0(cereals_tiff_data_path, "CH_data_final.csv")

cereals_tiff_data <- read.csv(file_path, stringsAsFactors = FALSE)

cereals_tiff_data_long <- cereals_tiff_data %>% 
  filter(!is.na(Year) & Year != "" & Year != 0) %>% 
  mutate(Barley_Yield = as.numeric(Barley_Yield)) %>%
  pivot_longer(
    cols = -Year,   # keep Year (or other ID columns) as is
    names_to = c("Crop/Land use", "Measure"),
    names_pattern = "(.*)_(.*)",   # everything before last "_" = Crop, after = Measure
    values_to = "Value"
  ) %>% 
  mutate(
    `Crop/Land use` = as.factor(`Crop/Land use`),  
    Measure = as.factor(Measure), 
    `Crop/Land use` = recode(`Crop/Land use`,
                  "S_Barley" = "Spring Barley",
                  "W_Barley" = "Winter Barley",
                  "Barley"  = "Total Barley",
                  "Oats"  = "Total Oats",
                  "Cereals" = "Main Cereals (Barley, Oats and Wheat)"
                  )
  ) %>% 
  filter(`Crop/Land use` != "OSR",
         Measure != "Area")

# Get the set of years where Area exists in Cereals_census_data_long
years_with_area <- Cereals_census_data_long %>%
  filter(Measure == "Area") %>%
  pull(Year) %>%
  unique()

# Filter cereals_tiff_data_long to only those years
cereals_tiff_data_long_filtered <- cereals_tiff_data_long %>%
  filter(Year %in% years_with_area)

# Combine the two datasets
cereals_combined_long <- bind_rows(Cereals_census_data_long,
                                   cereals_tiff_data_long_filtered)

#save to data
save(cereals_combined_long, file="Data/Cereals_data.Rda")


oilseed_tiff_data_long <- cereals_tiff_data %>% 
  filter(!is.na(Year) & Year != "" & Year != 0) %>% 
  mutate(Barley_Yield = as.numeric(Barley_Yield)) %>%
  pivot_longer(
    cols = -Year,   # keep Year (or other ID columns) as is
    names_to = c("Crop/Land use", "Measure"),
    names_pattern = "(.*)_(.*)",   # everything before last "_" = Crop, after = Measure
    values_to = "Value"
  ) %>% 
  mutate(
    `Crop/Land use` = as.factor(`Crop/Land use`),  
    Measure = as.factor(Measure), 
    `Crop/Land use` = recode(`Crop/Land use`,
                             "OSR" = "Oilseed Rape"
    )
  ) %>% 
  filter(`Crop/Land use` == "Oilseed Rape",
         Measure != "Area")

# Filter cereals_tiff_data_long to only those years
oilseed_tiff_data_long_filtered <- oilseed_tiff_data_long %>%
  filter(Year %in% years_with_area)

oilseed_combined_long <- bind_rows(Oilseed_census_data_long, 
                                   oilseed_tiff_data_long_filtered)

#save to data
save(oilseed_combined_long, file="Data/Oilseed_data.Rda")
