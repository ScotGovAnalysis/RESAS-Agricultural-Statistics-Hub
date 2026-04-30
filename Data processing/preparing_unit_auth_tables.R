##########################################################################
# Name of file - preparing_unit_auth_tables.R
# Written/run on - RStudio Desktop
# Version of R - 4.4.2
# Author: Euan Wakefield (euan.wakefield@gov.scot)
# Date created: 27/02/2026
#
# Description: The information on tables 15 to 22 on the June Agricultural Census
# 2025 by by unitary authority rather than region.
# Source: June Agricultural Census 2025.
# Requester: Calli Dougall
##########################################################################

# 1. Set up ----
rm(list = ls())

library(tidyr)
library(sf)
library(readr)
library(dplyr)
library(openxlsx)
library(data.table)
library(RtoSQLServer)
library(janitor)
library(openxlsx)
library(haven)
library(stringr)
library(geojsonio)
library(sp)

server <- "s0196a\\ADM"
database <- "RuralAndEnvironmentalScienceFarmingStatistics"
schema <- "juneagriculturalsurvey2025"

# 2. Import files ----

jac <- read_table_from_db(server=server,
                          database=database,
                          schema=schema,
                          table_name="JAC25_final_dataset_corrections_feb")

shape_data <- st_read("utility/Local Authority Boundaries Scotland/pub_las.shp")

# 3. Wrangle data ----
jac <- jac %>% 
  mutate(
    
    # If occupier is marked as doing no work and also some work, remove no work entry
    # If multiple working times are selected, full-time overrides all others 
    # More than half-time overrides less than half-time
    item2566 = ifelse(rowSums(across(c(item177, item178, item179, item2566))) > 1 & item2566 > 0, 0, item2566),
    item178 = ifelse(item177 > 0 & rowSums(across(c(item177, item178, item179))) > 1, 0, item178),
    item179 = case_when(
      item177 > 0 & rowSums(across(c(item177, item178, item179))) > 1 ~ 0,
      item178 > 0 & rowSums(across(c(item177, item178, item179))) > 1 ~ 0,
      TRUE ~ item179),
    
    item2567 = ifelse(rowSums(across(c(item182, item183, item184, item2567))) > 1 & item2567 > 0, 0, item2567),
    item183 = ifelse(item182 > 0 & rowSums(across(c(item182, item183, item184))) > 1, 0, item183),
    item184 = case_when(
      item182 > 0 & rowSums(across(c(item182, item183, item184))) > 1 ~ 0,
      item183 > 0 & rowSums(across(c(item182, item183, item184))) > 1 ~ 0,
      TRUE ~ item184),
    
    # If gender is marked as both male and female, set occupier 1 as male
    # and occupier 2 as female
    item2878 = if_else(item2877 == 1 & item2878 == 1, 0, item2878),
    item3056 = if_else(item3056 == 1 & item3057 == 1, 0, item3056),
    
    # Round up where a proportion of a worker is recorded
    item195 = if_else(item195 == 0.5, 1, item195),
    item198 = if_else(item198 == 0.2, 1, item198),
    item200 = if_else(item200 == 0.7, 2, item200),
    
    # Replace out-of-range legal responsibility response with unknown
    item2980 = ifelse(item2980 == 1949, 0, item2980))

ua_subset_shape <- shape_data %>%
  select(local_auth, code) %>%
  rename(unitauth = local_auth)
ua_subset_shape <- st_drop_geometry(ua_subset_shape)
  
unitary_authorities <- jac %>%
  mutate(unitauth = if_else(unitauth == 100, "Aberdeen City", as.character(unitauth)),
         unitauth = if_else(unitauth == 110, "Aberdeenshire", as.character(unitauth)),
         unitauth = if_else(unitauth == 120, "Angus", as.character(unitauth)),
         unitauth = if_else(unitauth == 130, "Argyll and Bute", as.character(unitauth)),
         unitauth = if_else(unitauth == 150, "Clackmannanshire", as.character(unitauth)),
         unitauth = if_else(unitauth == 170, "Dumfries and Galloway", as.character(unitauth)),
         unitauth = if_else(unitauth == 180, "Dundee City", as.character(unitauth)),
         unitauth = if_else(unitauth == 190, "East Ayrshire", as.character(unitauth)),
         unitauth = if_else(unitauth == 200, "East Dunbartonshire", as.character(unitauth)),
         unitauth = if_else(unitauth == 210, "East Lothian", as.character(unitauth)),
         unitauth = if_else(unitauth == 220, "East Renfrewshire", as.character(unitauth)),
         unitauth = if_else(unitauth == 230, "City of Edinburgh", as.character(unitauth)),
         unitauth = if_else(unitauth == 235, "Na h-Eileanan an Iar", as.character(unitauth)),
         unitauth = if_else(unitauth == 240, "Falkirk", as.character(unitauth)),
         unitauth = if_else(unitauth == 250, "Fife", as.character(unitauth)),
         unitauth = if_else(unitauth == 260, "Glasgow City", as.character(unitauth)),
         unitauth = if_else(unitauth == 270, "Highland", as.character(unitauth)),
         unitauth = if_else(unitauth == 280, "Inverclyde", as.character(unitauth)),
         unitauth = if_else(unitauth == 290, "Midlothian", as.character(unitauth)),
         unitauth = if_else(unitauth == 300, "Moray", as.character(unitauth)),
         unitauth = if_else(unitauth == 310, "North Ayrshire", as.character(unitauth)),
         unitauth = if_else(unitauth == 320, "North Lanarkshire", as.character(unitauth)),
         unitauth = if_else(unitauth == 330, "Orkney Islands", as.character(unitauth)),
         unitauth = if_else(unitauth == 340, "Perth and Kinross", as.character(unitauth)),
         unitauth = if_else(unitauth == 350, "Renfrewshire", as.character(unitauth)), 
         unitauth = if_else(unitauth == 355, "Scottish Borders", as.character(unitauth)),       
         unitauth = if_else(unitauth == 360, "Shetland Islands", as.character(unitauth)),
         unitauth = if_else(unitauth == 370, "South Ayrshire", as.character(unitauth)),
         unitauth = if_else(unitauth == 380, "South Lanarkshire", as.character(unitauth)),
         unitauth = if_else(unitauth == 390, "Stirling", as.character(unitauth)),
         unitauth = if_else(unitauth == 395, "West Dunbartonshire", as.character(unitauth)),
         unitauth = if_else(unitauth == 400, "West Lothian", as.character(unitauth))
  )


table_01 <- unitary_authorities %>%
  filter(completedata == 1) %>%
  mutate(
    "Wheat" = item14,
    "Winter Barley" = item16, 
    "Spring Barley" = item18,
    "Total Barley" = item16 + item18,
    "Oats and mixed grain" = item17 + item20 + item22,
    "Triticale" = item15,
    "Oilseeds (Including Linseed)" = item19 + item23 + item21,
    "Potatoes" = item24 + item2320,
    "Peas and Beans for Combining" = item27 + item28,
    "Stockfeeding Crops" = item27725,
    "Vegetables For Human Consumption" = item27730,
    "Orchard and soft fruit" = item27735,
    "Bulbs, Flowers & Nursery Stock" = item84,
    "All Other Crops" = item38 + item3156 - (item84 + item87 + item2556 + item2557 + item2836 + item6000 + item2036),
    "Fallow - Under 5 Years" = item2469,
    "Fallow - 5th Year & Over" = item2470,
    "Total Crops, Fallow, And Set-Aside" = item40,
    "Rotational Grass - Under 5 Years" = item2321,
    "Permanent Grassland - 5th Year & Over" = item2322,
    "Sole Right Grazing" = item47,
    "Total Grass and Rough Grazing" = `Rotational Grass - Under 5 Years` + `Permanent Grassland - 5th Year & Over` + `Sole Right Grazing`,
    "Holdings with Utilised Agricultural Area (UAA), (excl. common grazing)" = item46 + `Sole Right Grazing`,
    "Other Land (including woodland)" = item48 + item49,
    "Number of Holdings" = 1
  ) %>%
  select(
    "Wheat", "Winter Barley", "Spring Barley", "Total Barley",
    "Oats and mixed grain", "Triticale", "Oilseeds (Including Linseed)", "Potatoes",
    "Peas and Beans for Combining", "Stockfeeding Crops",
    "Vegetables For Human Consumption", "Orchard and soft fruit",
    "Bulbs, Flowers & Nursery Stock", "All Other Crops",
    "Fallow - Under 5 Years", "Fallow - 5th Year & Over",
    "Total Crops, Fallow, And Set-Aside", "Rotational Grass - Under 5 Years",
    "Permanent Grassland - 5th Year & Over", "Sole Right Grazing", "Total Grass and Rough Grazing",
    "Holdings with Utilised Agricultural Area (UAA), (excl. common grazing)",
    "Other Land (including woodland)", "Number of Holdings", unitauth
  )

# Change all positive values to 1 and all negative to 0
table_01 <- table_01 %>%
  dplyr::mutate(across(Wheat:"Number of Holdings", ~ ifelse(. > 0, 1, 0)))

# Summarise the table, grouping by constituency
table_01 <- table_01 %>%
  group_by(unitauth) %>%
  dplyr::summarise(across(everything(), sum, na.rm = TRUE))

# Join on S codes
table_01 <- table_01 %>%
  left_join(ua_subset_shape, by = "unitauth") %>%
  relocate(code, .before = unitauth)

# Table 2: Area with crops and grass ----

table_02 <- unitary_authorities %>%
  filter(completedata == 1) %>%
  mutate(
    "Wheat" = item14,
    "Winter Barley" = item16, 
    "Spring Barley" = item18,
    "Total Barley" = item16 + item18,
    "Oats and mixed grain" = item17 + item20 + item22,
    "Triticale" = item15,
    "Oilseeds (Including Linseed)" = item19 + item23 + item21,
    "Potatoes" = item24 + item2320,
    "Peas and Beans for Combining" = item27 + item28,
    "Stockfeeding Crops" = item27725,
    "Vegetables For Human Consumption" = item27730,
    "Orchard and soft fruit" = item27735,
    "Bulbs, Flowers & Nursery Stock" = item84,
    "All Other Crops" = item38 + item3156 - (item84 + item87 + item2556 + item2557 + item2836 + item6000 + item2036),
    "Fallow - Under 5 Years" = item2469,
    "Fallow - 5th Year & Over" = item2470,
    "Total Crops, Fallow, And Set-Aside" = item40,
    "Rotational Grass - Under 5 Years" = item2321,
    "Permanent Grassland - 5th Year & Over" = item2322,
    "Sole Right Grazing" = item47,
    "Total Grass and Rough Grazing" = `Rotational Grass - Under 5 Years` + `Permanent Grassland - 5th Year & Over` + `Sole Right Grazing`,
    "Holdings with Utilised Agricultural Area (UAA), (excl. common grazing)" = item46 + `Sole Right Grazing`,
    "Other Land (including woodland)" = item48 + item49,
    "Total Sole Right Agricultural Area" = item50,
    "Number of Holdings" = 1
  ) %>%
  select(
    "Wheat", "Winter Barley", "Spring Barley", "Total Barley",
    "Oats and mixed grain", "Triticale", "Oilseeds (Including Linseed)", "Potatoes",
    "Peas and Beans for Combining", "Stockfeeding Crops",
    "Vegetables For Human Consumption", "Orchard and soft fruit",
    "Bulbs, Flowers & Nursery Stock", "All Other Crops",
    "Fallow - Under 5 Years", "Fallow - 5th Year & Over",
    "Total Crops, Fallow, And Set-Aside", "Rotational Grass - Under 5 Years",
    "Permanent Grassland - 5th Year & Over", "Sole Right Grazing", "Total Grass and Rough Grazing",
    "Holdings with Utilised Agricultural Area (UAA), (excl. common grazing)",
    "Other Land (including woodland)", "Total Sole Right Agricultural Area", "Number of Holdings", unitauth
  )

# Summarise table and group by constituency
table_02 <- table_02 %>%
  group_by(unitauth) %>%
  dplyr::summarise(across(everything(), sum, na.rm = TRUE))

# Join on S codes
table_02 <- table_02 %>%
  left_join(ua_subset_shape, by = "unitauth") %>%
  relocate(code, .before = unitauth)

# Table 3: Number of holdings with livestock ----

table_03 <- unitary_authorities %>%
  dplyr::mutate(
    "Female Dairy Cattle aged 1 to 2 years" = cts304,
    "Female Dairy Cattle 2 years and over with offspring" = cts306,
    "Female Dairy Cattle 2 years and over without offspring" = cts308,
    "Total Female Dairy Cattle" = 
      `Female Dairy Cattle aged 1 to 2 years` + `Female Dairy Cattle 2 years and over with offspring` +
      `Female Dairy Cattle 2 years and over without offspring`,
    
    "Female Beef Cattle aged 1 to 2 years" = cts303,
    "Female Beef Cattle 2 years and over with offspring" = cts305,
    "Female Beef Cattle 2 years and over without offspring" = cts307,
    "Total Female Beef Cattle" =
      `Female Beef Cattle aged 1 to 2 years` + `Female Beef Cattle 2 years and over with offspring` +
      `Female Beef Cattle 2 years and over without offspring`,
    
    "Male Cattle aged 1 to 2 years" = cts310,
    "Male Cattle aged 2 and over" = cts311,
    "Total Male Cattle" = `Male Cattle aged 1 to 2 years` + `Male Cattle aged 2 and over`,
    
    "Female Dairy Cattle under 1 year (calves)" = cts302,
    "Female Beef Cattle under 1 year (calves)" = cts301,
    "Male Cattle under 1 year (calves)" = cts309,
    "Total Calves" = `Female Dairy Cattle under 1 year (calves)` +
      `Female Beef Cattle under 1 year (calves)` + `Male Cattle under 1 year (calves)`,
    "Total Cattle" = `Total Female Dairy Cattle` + `Total Female Beef Cattle` +
      `Total Male Cattle` + `Total Calves`,
    
    "Ewes for breeding" = item172,
    "Other sheep 1 year and over for breeding" = item141,
    "Rams for service" = item173,
    "Lambs" = item144,
    "Other sheep not for breeding" = item143,
    "Total Sheep" = `Ewes for breeding` + `Other sheep 1 year and over for breeding` +
      `Rams for service` + Lambs + `Other sheep not for breeding`,
    
    "Female pigs breeding herd" = item146 + item147 + item148,
    "All other non-breeding pigs" = item149 + item150 + item151 + item27760 + 
      item27765 + item27770,
    "Total Pigs" = `Female pigs breeding herd` + `All other non-breeding pigs`,
    
    "Fowls for producing eggs" = item158 + item159 + item161,
    "Fowls for breeding" = item160 + item162 + item163,
    "Broilers and other table fowls and other poultry" = item164 + item1708 + 
      item2038 + item2039 + item167,
    "Total Poultry" = item170,
    
    "Goats and kids" = item27780,
    Deer = item94, 
    Horses = item27775,
    Donkeys = item2868,
    Camelids = item2472 + item2473 + item2474,
    Beehives = item2826,
    
    "Total Agricultural Holdings" = 1,
  ) %>%
  select(
    "Female Dairy Cattle aged 1 to 2 years", "Female Dairy Cattle 2 years and over with offspring",
    "Female Dairy Cattle 2 years and over without offspring", "Total Female Dairy Cattle",
    "Female Beef Cattle aged 1 to 2 years", "Female Beef Cattle 2 years and over with offspring",
    "Female Beef Cattle 2 years and over without offspring", "Total Female Beef Cattle",
    "Male Cattle aged 1 to 2 years", "Male Cattle aged 2 and over",
    "Total Male Cattle", "Female Dairy Cattle under 1 year (calves)",
    "Female Beef Cattle under 1 year (calves)", "Male Cattle under 1 year (calves)",
    "Total Calves", "Total Cattle", "Ewes for breeding", "Other sheep 1 year and over for breeding",
    "Rams for service", "Lambs", "Other sheep not for breeding", "Total Sheep",
    "Female pigs breeding herd", "All other non-breeding pigs", "Total Pigs",
    "Fowls for producing eggs", "Fowls for breeding", "Broilers and other table fowls and other poultry",
    "Total Poultry", "Goats and kids", Deer, Horses, Donkeys, Camelids,  Beehives,
    "Total Agricultural Holdings", unitauth
  )


# Change all positive values to 1 and all negative to 0
table_03 <- table_03 %>%
  mutate(across(`Female Dairy Cattle aged 1 to 2 years`:`Total Agricultural Holdings`, ~ ifelse(. > 0, 1, 0)))

# Summarise tables grouped by constituency
table_03 <- table_03 %>%
  group_by(unitauth) %>%
  dplyr::summarise(across(everything(), sum, na.rm = TRUE))

# Join on S codes
table_03 <- table_03 %>%
  left_join(ua_subset_shape, by = "unitauth") %>%
  relocate(code, .before = unitauth)

# Table 4: Number of livestock ----

table_04 <- unitary_authorities %>%
  dplyr::mutate(
    "Female Dairy Cattle aged 1 to 2 years" = cts304,
    "Female Dairy Cattle 2 years and over with offspring" = cts306,
    "Female Dairy Cattle 2 years and over without offspring" = cts308,
    "Total Female Dairy Cattle" = 
      `Female Dairy Cattle aged 1 to 2 years` + `Female Dairy Cattle 2 years and over with offspring` +
      `Female Dairy Cattle 2 years and over without offspring`,
    
    "Female Beef Cattle aged 1 to 2 years" = cts303,
    "Female Beef Cattle 2 years and over with offspring" = cts305,
    "Female Beef Cattle 2 years and over without offspring" = cts307,
    "Total Female Beef Cattle" =
      `Female Beef Cattle aged 1 to 2 years` + `Female Beef Cattle 2 years and over with offspring` +
      `Female Beef Cattle 2 years and over without offspring`,
    
    "Male Cattle aged 1 to 2 years" = cts310,
    "Male Cattle aged 2 and over" = cts311,
    "Total Male Cattle" = `Male Cattle aged 1 to 2 years` + `Male Cattle aged 2 and over`,
    
    "Female Dairy Cattle under 1 year (calves)" = cts302,
    "Female Beef Cattle under 1 year (calves)" = cts301,
    "Male Cattle under 1 year (calves)" = cts309,
    "Total Calves" = `Female Dairy Cattle under 1 year (calves)` +
      `Female Beef Cattle under 1 year (calves)` + `Male Cattle under 1 year (calves)`,
    "Total Cattle" = `Total Female Dairy Cattle` + `Total Female Beef Cattle` +
      `Total Male Cattle` + `Total Calves`,
    
    "Ewes for breeding" = item172,
    "Other sheep 1 year and over for breeding" = item141,
    "Rams for service" = item173,
    "Lambs" = item144,
    "Other sheep not for breeding" = item143,
    "Total Sheep" = `Ewes for breeding` + `Other sheep 1 year and over for breeding` +
      `Rams for service` + Lambs + `Other sheep not for breeding`,
    
    "Female pigs breeding herd" = item146 + item147 + item148,
    "All other non-breeding pigs" = item149 + item150 + item151 + item27760 + 
      item27765 + item27770,
    "Total Pigs" = `Female pigs breeding herd` + `All other non-breeding pigs`,
    
    "Fowls for producing eggs" = item158 + item159 + item161,
    "Fowls for breeding" = item160 + item162 + item163,
    "Broilers and other table fowls and other poultry" = item164 + item1708 + 
      item2038 + item2039 + item167,
    "Total Poultry" = item170,
    
    "Goats and kids" = item27780,
    Deer = item94, 
    Horses = item27775,
    Donkeys = item2868,
    Camelids = item2472 + item2473 + item2474,
    Beehives = item2826,
    
    "Total Agricultural Holdings" = 1,
  ) %>%
  select(
    "Female Dairy Cattle aged 1 to 2 years", "Female Dairy Cattle 2 years and over with offspring",
    "Female Dairy Cattle 2 years and over without offspring", "Total Female Dairy Cattle",
    "Female Beef Cattle aged 1 to 2 years", "Female Beef Cattle 2 years and over with offspring",
    "Female Beef Cattle 2 years and over without offspring", "Total Female Beef Cattle",
    "Male Cattle aged 1 to 2 years", "Male Cattle aged 2 and over",
    "Total Male Cattle", "Female Dairy Cattle under 1 year (calves)",
    "Female Beef Cattle under 1 year (calves)", "Male Cattle under 1 year (calves)",
    "Total Calves", "Total Cattle", "Ewes for breeding", "Other sheep 1 year and over for breeding",
    "Rams for service", "Lambs", "Other sheep not for breeding", "Total Sheep",
    "Female pigs breeding herd", "All other non-breeding pigs", "Total Pigs",
    "Fowls for producing eggs", "Fowls for breeding", "Broilers and other table fowls and other poultry",
    "Total Poultry", "Goats and kids", Deer, Horses, Donkeys, Camelids,  Beehives,
    "Total Agricultural Holdings", unitauth
  )

# Summarise table grouped by constituency
table_04 <- table_04 %>%
  group_by(unitauth) %>%
  dplyr::summarise(across(everything(), sum, na.rm = TRUE))

# Join on S codes
table_04 <- table_04 %>%
  left_join(ua_subset_shape, by = "unitauth") %>%
  relocate(code, .before = unitauth)

# Table 5 - Number of holdings with occupiers and employees ----
table_05 <- unitary_authorities %>%
  mutate(
    "Occupiers - Full Time" = item177 + item182,
    "Occupiers - Half Time Or More" = item178 + item183,
    "Occupiers - Less Than Half Time" = item179 + item184,
    "Total Working Occupiers" = `Occupiers - Full Time` + `Occupiers - Half Time Or More` + `Occupiers - Less Than Half Time`,
    
    "Occupiers Not Working On The Holding" = item2566 + item2567,
    
    "Ft Males Hired" = item1715,
    "Ft Males Family" = item1716,
    "Ft Males Partners" = item1714,
    "Ft Males Total" = `Ft Males Hired` + `Ft Males Family` + `Ft Males Partners`,
    
    "Ft Females Hired" = item192,
    "Ft Females Family" = item193,
    "Ft Females Partners" = item1717,
    "Ft Females Total" = `Ft Females Hired` + `Ft Females Family` + `Ft Females Partners`,
    
    "Regular Full-Time Staff Total" = `Ft Males Total` + `Ft Females Total`,
    
    "Pt Males Hired" = item194,
    "Pt Males Family" = item195, 
    "Pt Males Partners" = item1718,
    "Pt Males Total" = `Pt Males Hired` + `Pt Males Family` + `Pt Males Partners`,
    
    "Pt Females Hired" = item196,
    "Pt Females Family" = item197, 
    "Pt Females Partners" = item1719,
    "Pt Females Total" = `Pt Females Hired` + `Pt Females Family` + `Pt Females Partners`,
    
    "Regular Part-Time Staff Total" = `Pt Males Total` + `Pt Females Total`,
    
    "Total Regular Full-Time And Part-Time Staff" = `Regular Full-Time Staff Total` + `Regular Part-Time Staff Total`,
    
    "Males Casual And Seasonal Staff" = item198,
    "Females Casual And Seasonal Staff" = item199,
    "Total Casual And Seasonal Staff" = `Males Casual And Seasonal Staff` + `Females Casual And Seasonal Staff`,
    "Total Agricultural Workforce" = item200,
    "European Migrant Labour (Person Working Days)" = item2536,
    "Non-European Migrant Labour (Person Working Days)" = item2511,
    "Migrant Labour (Person Working Days)" = item2536 + item2511,
    "Total Workforce (including occupiers)" = `Total Agricultural Workforce` + `Total Working Occupiers`,
    "Total Agricultural Holdings" = 1
  ) %>%
  select(
    "Occupiers - Full Time", "Occupiers - Half Time Or More", "Occupiers - Less Than Half Time",
    "Total Working Occupiers", "Occupiers Not Working On The Holding","Ft Males Partners",
    "Ft Males Hired",
    "Ft Males Family","Ft Females Partners", "Ft Females Hired", 
    "Ft Females Family", "Regular Full-Time Staff Total",
    "Pt Males Partners", "Pt Males Hired", "Pt Males Family",
    "Pt Females Partners", "Pt Females Hired", "Pt Females Family",
    "Regular Part-Time Staff Total",
    "Males Casual And Seasonal Staff", "Females Casual And Seasonal Staff",
    "Total Casual And Seasonal Staff", "Total Agricultural Workforce",
    "Total Workforce (including occupiers)", "Total Agricultural Holdings", unitauth
  )

# Change all positive values to 1 and all negative to 0
table_05 <- table_05 %>%
  mutate(across(`Occupiers - Full Time`:`Total Agricultural Holdings`, ~ ifelse(. > 0, 1, 0)))

# Summarise table grouped by constituency
table_05 <- table_05 %>%
  group_by(unitauth) %>%
  dplyr::summarise(across(everything(), sum, na.rm = TRUE))

# Join on S codes
table_05 <- table_05 %>%
  left_join(ua_subset_shape, by = "unitauth") %>%
  relocate(code, .before = unitauth)

# Table 6 - Number of occupiers and employees ----
table_06 <- unitary_authorities %>%
  mutate(
    "Occupiers - Full Time" = item177 + item182,
    "Occupiers - Half Time Or More" = item178 + item183,
    "Occupiers - Less Than Half Time" = item179 + item184,
    "Total Working Occupiers" = `Occupiers - Full Time` + `Occupiers - Half Time Or More` + `Occupiers - Less Than Half Time`,
    
    "Occupiers Not Working On The Holding" = item2566 + item2567,
    
    "Ft Males Hired" = item1715,
    "Ft Males Family" = item1716,
    "Ft Males Partners" = item1714,
    "Ft Males Total" = `Ft Males Hired` + `Ft Males Family` + `Ft Males Partners`,
    
    "Ft Females Hired" = item192,
    "Ft Females Family" = item193,
    "Ft Females Partners" = item1717,
    "Ft Females Total" = `Ft Females Hired` + `Ft Females Family` + `Ft Females Partners`,
    
    "Regular Full-Time Staff Total" = `Ft Males Total` + `Ft Females Total`,
    
    "Pt Males Hired" = item194,
    "Pt Males Family" = item195, 
    "Pt Males Partners" = item1718,
    "Pt Males Total" = `Pt Males Hired` + `Pt Males Family` + `Pt Males Partners`,
    
    "Pt Females Hired" = item196,
    "Pt Females Family" = item197, 
    "Pt Females Partners" = item1719,
    "Pt Females Total" = `Pt Females Hired` + `Pt Females Family` + `Pt Females Partners`,
    
    "Regular Part-Time Staff Total" = `Pt Males Total` + `Pt Females Total`,
    
    "Total Regular Full-Time And Part-Time Staff" = `Regular Full-Time Staff Total` + `Regular Part-Time Staff Total`,
    
    "Males Casual And Seasonal Staff" = item198,
    "Females Casual And Seasonal Staff" = item199,
    "Total Casual And Seasonal Staff" = `Males Casual And Seasonal Staff` + `Females Casual And Seasonal Staff`,
    "Total Agricultural Workforce" = item200,
    "European Migrant Labour (Person Working Days)" = item2536,
    "Non-European Migrant Labour (Person Working Days)" = item2511,
    "Migrant Labour (Person Working Days)" = item2536 + item2511,
    "Total Workforce (including occupiers)" = `Total Agricultural Workforce` + `Total Working Occupiers`
  ) %>%
  select(
    "Occupiers - Full Time", "Occupiers - Half Time Or More", "Occupiers - Less Than Half Time",
    "Total Working Occupiers", "Occupiers Not Working On The Holding","Ft Males Partners",
    "Ft Males Hired",
    "Ft Males Family","Ft Females Partners", "Ft Females Hired", 
    "Ft Females Family", "Regular Full-Time Staff Total",
    "Pt Males Partners", "Pt Males Hired", "Pt Males Family",
    "Pt Females Partners", "Pt Females Hired", "Pt Females Family",
    "Regular Part-Time Staff Total",
    "Males Casual And Seasonal Staff", "Females Casual And Seasonal Staff",
    "Total Casual And Seasonal Staff", "Total Agricultural Workforce",
    "Total Workforce (including occupiers)", unitauth
  )

# Summarise table grouped by constituency
table_06 <- table_06 %>%
  group_by(unitauth) %>%
  dplyr::summarise(across(everything(), sum, na.rm = TRUE))

# Join on S codes
table_06 <- table_06 %>%
  left_join(ua_subset_shape, by = "unitauth") %>%
  relocate(code, .before = unitauth)

# Table 7 and 8: Number of holdings by farm type ----

# Farm type labels
robust_new_labels <- c(
  "1" = "Specialist cereals",
  "2" = "General cropping",
  "3" = "Specialist Horticulture & permanent crops",
  "4" = "Specialist pigs",
  "5" = "Specialist poultry",
  "6" = "Specialist dairy",
  "7" = "LFA Cattle and sheep",
  "8" = "Non_LFA Cattle and sheep",
  "9" = "Mixed holdings",
  "10" = "General cropping; forage",
  "11" = "Unclassified")

# Sub-region labels
agricreg_labels <- c(
  "1" = "Shetland",
  "2" = "Orkney",
  "3" = "Eileanan an Iar",
  "4" = "Highland",
  "5" = "NE Scotland",
  "6" = "Tayside",
  "7" = "Fife",
  "8" = "Lothian",
  "9" = "Scottish Borders",
  "10" = "East Central",
  "11" = "Argyll & Bute",
  "12" = "Clyde Valley",
  "13" = "Ayrshire",
  "14" = "Dumfries & Galloway")

## Farm types
farmtypes <- unitary_authorities %>%
  
  # Convert NA values of robust_new to 11 (unclassified) and add labels
  mutate(robust_new = ifelse(is.na(robust_new), "11", as.character(robust_new)),
         robust_new = factor(robust_new, levels = names(robust_new_labels), labels = robust_new_labels)) %>%
  filter(completedata == 1)


# Number of holdings by sub-region and farm type
no_subreg_farmtype <- farmtypes %>%
  group_by(robust_new, unitauth) %>%
  summarise(holdings = n(), .groups = "drop") %>%
  pivot_wider(names_from = unitauth, values_from = holdings, values_fill = 0) %>%
  adorn_totals("row") %>% 
  adorn_totals("col")
table_07 <- no_subreg_farmtype %>%
  pivot_longer(
    cols = -robust_new,
    names_to = "unitauth",
    values_to = "Value"
  ) %>%
  pivot_wider(
    names_from = robust_new,
    values_from = Value
  )

# Join on S codes
table_07 <- table_07 %>%
  left_join(ua_subset_shape, by = "unitauth") %>%
  relocate(code, .before = unitauth)

# Area of holdings by sub-region and farm type
area_subreg_farmtype <- farmtypes %>%
  group_by(robust_new, unitauth) %>%
  summarise(hectares = sum(item50, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = unitauth, values_from = hectares, values_fill = 0) %>%
  adorn_totals("row") %>% 
  adorn_totals("col")

table_08 <- area_subreg_farmtype %>%
  pivot_longer(
    cols = -robust_new,
    names_to = "unitauth",
    values_to = "Value"
  ) %>%
  pivot_wider(
    names_from = robust_new,
    values_from = Value
  )

# Join on S codes
table_08 <- table_08 %>%
  left_join(ua_subset_shape, by = "unitauth") %>%
  relocate(code, .before = unitauth)


# Step 5: Cover information ----

cover_list <- list(
  "An Accredited Official Statistics Publication for Scotland" = c(""),
  "Information" = c("Data in this workbook relates to the 'June Agricultural Census: June 2025' statistical release.", 
                    "This workbook contains data from the Scottish Agricultural Census: June 2025 Results.  Final results from the 2025 June Agricultural Census on land use, crop areas, livestock and the number of people working on agricultural holdings.",
                    "Some tables refer to notes. When notes are mentioned the note marker is presented in square brackets. The note text can be found in the Notes worksheet.",
                    "Full details of the survey methodology are available from the Methodology and Quality Assurance Report.")
)

contents_df <- data.frame(
  "Table number" = c("Notes", "Table 1", "Table 2",
                     "Table 3", "Table 4", "Table 5", "Table 6",
                     "Table 7", 
                     "Table 8"
  ),
  "Table name" = c(
    "Notes used in this workbook.",
    "Number of holdings with crops and grass by Unitary Authority, June 2025",
    "Area of crops and grass by Unitary Authority, June 2025",
    "Number of holdings with livestock by Unitary Authority, June 2025",
    "Number of livestock by Unitary Authority, June 2025",
    "Number of holdings with occupiers and employees by Unitary Authority, June 2025",
    "Number of occupiers and employees by Unitary Authority, June 2025",
    "Number of holdings by main farm type and by Unitary Authority, June 2025",
    "Area of holdings by main farm type and by Unitary Authority, June 2025"
  ),
  check.names = FALSE
)

notes_df <- data.frame(
  "Note number" = paste0("Note ", 1:30),
  "Note text" = c("Comparisons are made as a percentage change between 2025 to the 5 year average calculated from 2020 to 2024."
                  
  ),
  check.names = FALSE
)
#checks
notes_df
contents_df
notes_df
cover_list

library(aftables)
jac_aftables <- 
  aftables::create_aftable(
    tab_titles = c("Cover",
                   "Contents",
                   "Notes",
                   "Table 1",
                   "Table 2",
                   "Table 3",
                   "Table 4",
                   "Table 5",
                   "Table 6",
                   "Table 7",
                   "Table 8"
    ),
    sheet_types = c("cover", 
                    "contents",
                    "notes",
                    "tables",
                    "tables",
                    "tables",
                    "tables",
                    "tables",
                    "tables",
                    "tables",
                    "tables"
    ),
    sheet_titles = c(
      "Scottish Agricultural Census: June 2025 Results",
      "Table of contents.",
      "Notes.",
      "Table 1: Number of holdings with crops and grass by Unitary Authority, June 2025 [Note 16, 17, 18, 26]",
      "Table 2: Area of crops and grass by Unitary Authority, June 2025 [Note 17, 18, 19, 26]",
      "Table 3: Number of holdings with livestock by Unitary Authority, June 2025 [Note 20, 21, 26]",
      "Table 4: Number of livestock by Unitary Authority, June 2025 [Note 20, 21, 26]",
      "Table 5: Number of holdings with occupiers and employees by Unitary Authority, June 2025 [Note 22, 26]",
      "Table 6: Number of occupiers and employees by Unitary Authority, June 2025 [Note 22, 26]",
      "Table 7: Number of holdings by main farm type and by Unitary Authority, June 2025",
      "Table 8: Area of holdings by main farm type and by Unitary Authority, June 2025"
      
    ), 
    blank_cells = c(
      NA_character_,
      NA_character_,
      NA_character_,
      "Some cells refer to notes which can be found on the Notes Worksheet.",
      "Some cells refer to notes which can be found on the Notes Worksheet.",
      "Some cells refer to notes which can be found on the Notes Worksheet.",
      "Some cells refer to notes which can be found on the Notes Worksheet.",
      "Some cells refer to notes which can be found on the Notes Worksheet.",
      "Some cells refer to notes which can be found on the Notes Worksheet.",
      "Some cells refer to notes which can be found on the Notes Worksheet.",
      "Some cells refer to notes which can be found on the Notes Worksheet."
    ),
    custom_rows = list(
      NA_character_,
      NA_character_,
      NA_character_,
      " ",
      " ",
      " ",
      " ",
      " ",
      " ",
      " ",
      " "
    ),
    sources = c(
      NA_character_,
      NA_character_,
      NA_character_,
      "[JAC 2025](https://www.gov.scot/publications/results-from-the-scottish-agricultural-census-june-2025//)",
      "[JAC 2025](https://www.gov.scot/publications/results-from-the-scottish-agricultural-census-june-2025//)",
      "[JAC 2025](https://www.gov.scot/publications/results-from-the-scottish-agricultural-census-june-2025//)",
      "[JAC 2025](https://www.gov.scot/publications/results-from-the-scottish-agricultural-census-june-2025//)",
      "[JAC 2025](https://www.gov.scot/publications/results-from-the-scottish-agricultural-census-june-2025//)",
      "[JAC 2025](https://www.gov.scot/publications/results-from-the-scottish-agricultural-census-june-2025//)",
      "[JAC 2025](https://www.gov.scot/publications/results-from-the-scottish-agricultural-census-june-2025//)",
      "[JAC 2025](https://www.gov.scot/publications/results-from-the-scottish-agricultural-census-june-2025//)"
      
      
      
    ),
    tables = list(cover_list, contents_df, notes_df, table_01, table_02, table_03, table_04,
                  table_05, table_06, table_07, table_08
    )
  )
my_wb1 <- aftables::generate_workbook(jac_aftables)
openxlsx::saveWorkbook(my_wb1, paste0("Data/Workbook_Draft_Grouped_By_Unitary_Authority_new_", format(Sys.Date(), "%Y_%m_%d"), ".xlsx"))

