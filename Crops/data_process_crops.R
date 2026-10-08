load(here("Data", "census_data.RData"))

main_cereals <- cereals_data %>%
  filter(`Crop/land use` %in% c("Wheat", "Barley total", "Oats total")) %>%
  summarise(across(-`Crop/land use`, ~ sum(.x, na.rm = TRUE))) %>%
  mutate(`Crop/land use` = "Main cereals (barley, oats and wheat)") 


# Bind back into the wide dataset
cereals_data_census <- bind_rows(cereals_data, main_cereals)%>% 
  distinct(`Crop/land use`, .keep_all = TRUE)

save(cereals_data_census, file="Data/cereals_data_census.RData")

Cereals_census_data_long <- cereals_data_census %>% 
  pivot_longer(
    cols = -`Crop/land use`,
    names_to = "Year",
    values_to = "Value"
  ) %>% 
  mutate(
    Measure = "Area",
    `Crop/land use` = as.factor(`Crop/land use`),  
    Measure = as.factor(Measure),
    Year = as.integer(Year)
  )

Oilseed_census_data_long <- oilseed_data %>% 
  pivot_longer(
    cols = -`Crop/land use`,
    names_to = "Year",
    values_to = "Value"
  ) %>% 
  mutate(Measure = "Area",
         `Crop/land use` = as.factor(`Crop/land use`),  
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
    names_to = c("Crop/land use", "Measure"),
    names_pattern = "(.*)_(.*)",   # everything before last "_" = Crop, after = Measure
    values_to = "Value"
  ) %>% 
  mutate(
    `Crop/land use` = as.factor(`Crop/land use`),  
    Measure = as.factor(Measure), 
    `Crop/land use` = recode(`Crop/land use`,
                  "S_Barley" = "Spring barley",
                  "W_Barley" = "Winter barley",
                  "Barley"  = "Barley total",
                  "Oats"  = "Oats total",
                  "Cereals" = "Main cereals (barley, oats and wheat)"
                  )
  ) %>% 
  filter(`Crop/land use` != "OSR",
         Measure != "Area") %>% 
  distinct(`Crop/land use`, Year, Measure, .keep_all = TRUE)

save(cereals_tiff_data_long, file="Data/cereals_tiff_data_long.RData")


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
save(cereals_combined_long, file="Data/cereals_combined_long.RData")


oilseed_tiff_data_long <- cereals_tiff_data %>% 
  filter(!is.na(Year) & Year != "" & Year != 0) %>% 
  mutate(Barley_Yield = as.numeric(Barley_Yield)) %>%
  pivot_longer(
    cols = -Year,   # keep Year (or other ID columns) as is
    names_to = c("Crop/land use", "Measure"),
    names_pattern = "(.*)_(.*)",   # everything before last "_" = Crop, after = Measure
    values_to = "Value"
  ) %>% 
  mutate(
    `Crop/land use` = as.factor(`Crop/land use`),  
    Measure = as.factor(Measure), 
    `Crop/land use` = recode(`Crop/land use`,
                             "OSR" = "Oilseed Rape"
    )
  ) %>% 
  filter(`Crop/land use` == "Oilseed Rape",
         Measure != "Area")%>%
  filter(Year %in% years_with_area)

save(oilseed_tiff_data_long, file="Data/oilseed_tiff_data_long.RData")


oilseed_combined_long <- bind_rows(Oilseed_census_data_long, 
                                   oilseed_tiff_data_long)

#save to data
save(oilseed_combined_long, file="Data/oilseed_combined_long.RData")
