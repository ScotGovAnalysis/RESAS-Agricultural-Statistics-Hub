Cereals_census_data_long <- cereals_data %>% 
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
  filter(!is.na(Year) & Year != "") %>% 
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
                  "Barley"  = "Barley Total",
                  "Oats"  = "Oats Total",
                  "Cereals" = "Total Cereals"
                  )
  ) %>% 
  filter(`Crop/Land use` != "OSR",
         Measure != "Area")
  
cereals_combined_long <- bind_rows(Cereals_census_data_long, cereals_tiff_data_long)

#save to data
save(cereals_combined_long, file="Data/Cereals_data.Rda")
