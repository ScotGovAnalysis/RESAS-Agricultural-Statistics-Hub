##########################################################################
# Name of file - preparing_unit_auth_tables.R
# Written/run on - RStudio Desktop
# Version of R - 4.4.2
# Author: Euan Wakefield (euan.wakefield@gov.scot)
# Date created: 27/02/2026
# Last updated: 20/10/2026
#
# Description: Crop areas, livestock, and workforce by local authority.
# Source: June Agricultural Census 2026.
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
schema <- "juneagriculturalsurvey2026"


# 2. Import files ----

jac <- read_table_from_db(server=server,
                          database=database,
                          schema=schema,
                          table_name="jac26_string_corrections_oct26")

shape_data <- st_read("utility/Local Authority Boundaries Scotland/pub_las.shp")


# 3. Wrangle data ----

jac <- jac %>% 
  mutate(across(where(is.numeric), ~ replace_na(.x, 0)))
  
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
    "Winter barley" = item16, 
    "Spring barley" = item18,
    "Total barley" = item16 + item18,
    "Oats and mixed grain" = item17 + item20 + item22,
    "Triticale and rye" = item15 + item3156,
    "Total cereals" = item14 + item15 + item16 + item18 + item17 + item20 + 
      item3156 + item22,
    "Oilseeds (including linseed)" = item19 + item23 + item21,
    "Potatoes" = item24 + item2320,
    "Peas and beans for combining" = item27 + item28,
    "Stockfeeding crops" = item27725 + item2034,
    "Vegetables for human consumption" = item27730,
    "Orchard and soft fruit" = item36 + item70 + item71 + item72 +  item75 + 
      item87 + item2556 + item2557 + item2036 + item2832 + item2836 + item6000,
    "Bulbs, flowers & nursery stock" = item84,
    "Other crops" = item38 - (item84 + item87 + item2556 + item2557 + item2836 + item6000 + item2036),
    "Fallow land for 5 years or less" = item2469,
    "Fallow land for more than 5 years" = item2470,
    "Total crops, fallow, and set-aside" = item40,
    "Grass under 5 years old" = item2321,
    "Grass 5 years old and over" = item2322,
    "Rough grazing" = item47,
    "Total grass and rough grazing" = item2321 + item2322 + item47,
    "Utilised agricultural area (UAA)" = item46 + item47,
    "Other land (including woodland)" = item48 + item49,
    "Number of holdings" = 1
  ) %>%
  select(
    "Wheat", "Winter barley", "Spring barley", "Total barley",
    "Oats and mixed grain", "Triticale and rye",
    "Total cereals", "Oilseeds (including linseed)", "Potatoes",
    "Peas and beans for combining", "Stockfeeding crops",
    "Vegetables for human consumption", "Orchard and soft fruit",
    "Bulbs, flowers & nursery stock", "Other crops",
    "Fallow land for 5 years or less", "Fallow land for more than 5 years",
    "Total crops, fallow, and set-aside", "Grass under 5 years old",
    "Grass 5 years old and over", "Rough grazing", "Total grass and rough grazing",
    "Utilised agricultural area (UAA)",
    "Other land (including woodland)", "Number of holdings", unitauth
  )

# Change all positive values to 1 and all negative to 0
table_01 <- table_01 %>%
  dplyr::mutate(across(Wheat:"Number of holdings", ~ ifelse(. > 0, 1, 0)))

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
    "Winter barley" = item16, 
    "Spring barley" = item18,
    "Total barley" = item16 + item18,
    "Oats and mixed grain" = item17 + item20 + item22,
    "Triticale and rye" = item15 + item3156,
    "Total cereals" = item14 + item15 + item16 + item18 + item17 + item20 + 
      item3156 + item22,
    "Oilseeds (including linseed)" = item19 + item23 + item21,
    "Potatoes" = item24 + item2320,
    "Peas and beans for combining" = item27 + item28,
    "Stockfeeding crops" = item27725 + item2034,
    "Vegetables for human consumption" = item27730,
    "Orchard and soft fruit" = item36 + item70 + item71 + item72 +  item75 + 
      item87 + item2556 + item2557 + item2036 + item2832 + item2836 + item6000,
    "Bulbs, flowers & nursery stock" = item84,
    "Other crops" = item38 - (item84 + item87 + item2556 + item2557 + item2836 + item6000 + item2036),
    "Fallow land for 5 years or less" = item2469,
    "Fallow land for more than 5 years" = item2470,
    "Total crops, fallow, and set-aside" = item40,
    "Grass under 5 years old" = item2321,
    "Grass 5 years old and over" = item2322,
    "Rough grazing" = item47,
    "Total grass and rough grazing" = item2321 + item2322 + item47,
    "Utilised agricultural area (UAA)" = item46 + item47,
    "Other land (including woodland)" = item48 + item49,
    "Total sole right agricultural area" = item50,
  ) %>%
  select(
    "Wheat", "Winter barley", "Spring barley", "Total barley",
    "Oats and mixed grain", "Triticale and rye",
    "Total cereals", "Oilseeds (including linseed)", "Potatoes",
    "Peas and beans for combining", "Stockfeeding crops",
    "Vegetables for human consumption", "Orchard and soft fruit",
    "Bulbs, flowers & nursery stock", "Other crops",
    "Fallow land for 5 years or less", "Fallow land for more than 5 years",
    "Total crops, fallow, and set-aside", "Grass under 5 years old",
    "Grass 5 years old and over", "Rough grazing", "Total grass and rough grazing",
    "Utilised agricultural area (UAA)", "Other land (including woodland)", 
    "Total sole right agricultural area", unitauth
  )

# Summarise table and group by constituency
table_02 <- table_02 %>%
  group_by(unitauth) %>%
  dplyr::summarise(across(everything(), sum, na.rm = TRUE))

# Create common grazing dataset for joining
table_02_cg <- unitary_authorities %>%
  select(`Common grazings` = cgarea, unitauth) %>% 
  group_by(unitauth) %>%
  summarise(across(everything(), sum, na.rm = TRUE))

# Join common grazings and calculate total agricultural area
table_02_joined <- table_02 %>% 
  bind_cols(table_02_cg %>% select(-unitauth)) %>% 
  mutate(
    "Total agricultural area" = `Total sole right agricultural area` + `Common grazings`
  )

# Join on S codes
table_02 <- table_02_joined %>%
  left_join(ua_subset_shape, by = "unitauth") %>%
  relocate(code, .before = unitauth)




# Table 3: Number of holdings with livestock ----

table_03 <- unitary_authorities %>%
  dplyr::mutate(
    "Female dairy cattle aged 1 to 2 years" = cts304,
    "Female dairy cattle 2 years and over with offspring" = cts306,
    "Female dairy cattle 2 years and over without offspring" = cts308,
    "Total female dairy cattle" = cts304 + cts306 + cts308,
    
    "Female beef cattle aged 1 to 2 years" = cts303,
    "Female beef cattle 2 years and over with offspring" = cts305,
    "Female beef cattle 2 years and over without offspring" = cts307,
    "Total female beef cattle" = cts303 + cts305 + cts307,
    
    "Male cattle aged 1 to 2 years" = cts310,
    "Male cattle aged 2 and over" = cts311,
    "Total male cattle" = cts310 + cts311,
    
    "Female dairy cattle under 1 year (calves)" = cts302,
    "Female beef cattle under 1 year (calves)" = cts301,
    "Male cattle under 1 year (calves)" = cts309,
    "Total calves" = cts302 + cts301 + cts309,
    "Total cattle" = `Total female dairy cattle` + `Total female beef cattle` +
      `Total male cattle` + `Total calves`,
    
    "Ewes for breeding" = item165,
    "Other sheep 1 year and over for breeding" = item141,
    "Rams for service" = item166,
    "Lambs" = item144,
    "Other sheep not for breeding" = item143,
    "Total sheep" = item165 + item141 + item166 + item144 + item143,
    
    "Female pigs breeding herd" = item146 + item147 + item148,
    "All other non-breeding pigs" = item149 + item150 + item151 + item152 +
      item153 + item154 + item155 + item27765 + item27770,
    "Total pigs" = `Female pigs breeding herd` + `All other non-breeding pigs`,
    
    "Fowls for producing eggs" = item158 + item159 + item161,
    "Fowls for breeding" = item160 + item162 + item163,
    "Broilers, other table fowls and other poultry" = item164 + item1708 + 
      item2038 + item2039 + item167,
    "Total poultry" = item170,
    
    "Goats and kids" = item27780,
    Deer = item94, 
    Horses = item27775,
    Donkeys = item2868,
    Camelids = item2472 + item2473 + item2474,
    Beehives = item2826,
    
    "Total agricultural holdings" = 1,
  ) %>%
  select(
    "Female dairy cattle aged 1 to 2 years", "Female dairy cattle 2 years and over with offspring",
    "Female dairy cattle 2 years and over without offspring", "Total female dairy cattle",
    "Female beef cattle aged 1 to 2 years", "Female beef cattle 2 years and over with offspring",
    "Female beef cattle 2 years and over without offspring", "Total female beef cattle",
    "Male cattle aged 1 to 2 years", "Male cattle aged 2 and over",
    "Total male cattle", "Female dairy cattle under 1 year (calves)",
    "Female beef cattle under 1 year (calves)", "Male cattle under 1 year (calves)",
    "Total calves", "Total cattle", "Ewes for breeding", "Other sheep 1 year and over for breeding",
    "Rams for service", "Lambs", "Other sheep not for breeding", "Total sheep",
    "Female pigs breeding herd", "All other non-breeding pigs", "Total pigs",
    "Fowls for producing eggs", "Fowls for breeding", "Broilers, other table fowls and other poultry",
    "Total poultry", "Goats and kids", Deer, Horses, Donkeys, Camelids,  Beehives,
    "Total agricultural holdings", unitauth
  )


# Change all positive values to 1 and all negative to 0
table_03 <- table_03 %>%
  mutate(across(`Female dairy cattle aged 1 to 2 years`:`Total agricultural holdings`, ~ ifelse(. > 0, 1, 0)))

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
    "Female dairy cattle aged 1 to 2 years" = cts304,
    "Female dairy cattle 2 years and over with offspring" = cts306,
    "Female dairy cattle 2 years and over without offspring" = cts308,
    "Total female dairy cattle" = cts304 + cts306 + cts308,
    
    "Female beef cattle aged 1 to 2 years" = cts303,
    "Female beef cattle 2 years and over with offspring" = cts305,
    "Female beef cattle 2 years and over without offspring" = cts307,
    "Total female beef cattle" = cts303 + cts305 + cts307,
    
    "Male cattle aged 1 to 2 years" = cts310,
    "Male cattle aged 2 and over" = cts311,
    "Total male cattle" = cts310 + cts311,
    
    "Female dairy cattle under 1 year (calves)" = cts302,
    "Female beef cattle under 1 year (calves)" = cts301,
    "Male cattle under 1 year (calves)" = cts309,
    "Total calves" = cts302 + cts301 + cts309,
    "Total cattle" = `Total female dairy cattle` + `Total female beef cattle` +
      `Total male cattle` + `Total calves`,
    
    "Ewes for breeding" = item165,
    "Other sheep 1 year and over for breeding" = item141,
    "Rams for service" = item166,
    "Lambs" = item144,
    "Other sheep not for breeding" = item143,
    "Total sheep" = item165 + item141 + item166 + item144 + item143,
    
    "Female pigs breeding herd" = item146 + item147 + item148,
    "All other non-breeding pigs" = item149 + item150 + item151 + item152 +
      item153 + item154 + item155 + item27765 + item27770,
    "Total pigs" = `Female pigs breeding herd` + `All other non-breeding pigs`,
    
    "Fowls for producing eggs" = item158 + item159 + item161,
    "Fowls for breeding" = item160 + item162 + item163,
    "Broilers, other table fowls and other poultry" = item164 + item1708 + 
      item2038 + item2039 + item167,
    "Total poultry" = item170,
    
    "Goats and kids" = item27780,
    Deer = item94, 
    Horses = item27775,
    Donkeys = item2868,
    Camelids = item2472 + item2473 + item2474,
    Beehives = item2826,
    ) %>%
  
  select(
    "Female dairy cattle aged 1 to 2 years", "Female dairy cattle 2 years and over with offspring",
    "Female dairy cattle 2 years and over without offspring", "Total female dairy cattle",
    "Female beef cattle aged 1 to 2 years", "Female beef cattle 2 years and over with offspring",
    "Female beef cattle 2 years and over without offspring", "Total female beef cattle",
    "Male cattle aged 1 to 2 years", "Male cattle aged 2 and over",
    "Total male cattle", "Female dairy cattle under 1 year (calves)",
    "Female beef cattle under 1 year (calves)", "Male cattle under 1 year (calves)",
    "Total calves", "Total cattle", "Ewes for breeding", "Other sheep 1 year and over for breeding",
    "Rams for service", "Lambs", "Other sheep not for breeding", "Total sheep",
    "Female pigs breeding herd", "All other non-breeding pigs", "Total pigs",
    "Fowls for producing eggs", "Fowls for breeding", "Broilers, other table fowls and other poultry",
    "Total poultry", "Goats and kids", Deer, Horses, Donkeys, Camelids,  Beehives,
     unitauth
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
    "Occupiers - full time" = item177 + item182,
    "Occupiers - half time or more" = item178 + item183,
    "Occupiers - less than half time" = item179 + item184,
    "Total working occupiers" = item177 + item182 + item178 + item183 + item179 + item184,
    "Occupiers not working on the holding" = item2566 + item2567,
    
    "Regular full-time male staff" = item1715 + item1716 + item1714,
    "Regular full-time female staff" = item192 + item193 + item1717,
    "Regular full-time staff total" = item1715 + item1716 + item1714 + item192  + item193  + item1717,
    
    "Regular part-time male staff" = item194 + item195 + item1718,
    "Regular part-time female staff" = item196 + item197 + item1719,
    "Regular part-time staff total" = item194 + item195 + item1718 + item196 + item197 + item1719,
    
    "Male casual and seasonal staff" = item198,
    "Female casual and seasonal staff" = item199,
    "Total casual and seasonal staff" = item198 + item199,
    
    "Total employees" = item200,
    "Total workforce (including occupiers)" = `Total employees` + `Total working occupiers`,
    "Total agricultural holdings" = 1
  ) %>%
  
  select(
    "Occupiers - full time", "Occupiers - half time or more", "Occupiers - less than half time",
    "Total working occupiers", "Occupiers not working on the holding",
    "Regular full-time male staff", "Regular full-time female staff",
    "Regular full-time staff total", "Regular part-time male staff",
    "Regular part-time female staff", "Regular part-time staff total",
    "Male casual and seasonal staff", "Female casual and seasonal staff",
    "Total casual and seasonal staff", "Total employees",
    "Total workforce (including occupiers)", "Total agricultural holdings", unitauth
  )

# Change all positive values to 1 and all negative to 0
table_05 <- table_05 %>%
  mutate(across(`Occupiers - full time`:`Total agricultural holdings`, ~ ifelse(. > 0, 1, 0)))

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
    "Occupiers - full time" = item177 + item182,
    "Occupiers - half time or more" = item178 + item183,
    "Occupiers - less than half time" = item179 + item184,
    "Total working occupiers" = item177 + item182 + item178 + item183 + item179 + item184,
    "Occupiers not working on the holding" = item2566 + item2567,
    
    "Regular full-time male staff" = item1715 + item1716 + item1714,
    "Regular full-time female staff" = item192 + item193 + item1717,
    "Regular full-time staff total" = item1715 + item1716 + item1714 + item192  + item193  + item1717,
    
    "Regular part-time male staff" = item194 + item195 + item1718,
    "Regular part-time female staff" = item196 + item197 + item1719,
    "Regular part-time staff total" = item194 + item195 + item1718 + item196 + item197 + item1719,
    
    "Male casual and seasonal staff" = item198,
    "Female casual and seasonal staff" = item199,
    "Total casual and seasonal staff" = item198 + item199,
    
    "Total employees" = item200,
    "Total workforce (including occupiers)" = `Total employees` + `Total working occupiers`,
  ) %>%
  
  select(
    "Occupiers - full time", "Occupiers - half time or more", "Occupiers - less than half time",
    "Total working occupiers", "Occupiers not working on the holding",
    "Regular full-time male staff", "Regular full-time female staff",
    "Regular full-time staff total", "Regular part-time male staff",
    "Regular part-time female staff", "Regular part-time staff total",
    "Male casual and seasonal staff", "Female casual and seasonal staff",
    "Total casual and seasonal staff", "Total employees",
    "Total workforce (including occupiers)", unitauth
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
  "Information" = c("Data in this workbook relates to the 'June Agricultural Census: June 2026' statistical release.", 
                    "This workbook contains data from the Scottish Agricultural Census: June 2026 Results.  Final results from the 2026 June Agricultural Census on land use, crop areas, livestock and the number of people working on agricultural holdings.",
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
    "Number of holdings with crops and grass by Unitary Authority, June 2026",
    "Area of crops and grass by Unitary Authority, June 2026",
    "Number of holdings with livestock by Unitary Authority, June 2026",
    "Number of livestock by Unitary Authority, June 2026",
    "Number of holdings with occupiers and employees by Unitary Authority, June 2026",
    "Number of occupiers and employees by Unitary Authority, June 2026",
    "Number of holdings by main farm type and by Unitary Authority, June 2026",
    "Area of holdings by main farm type and by Unitary Authority, June 2026"
  ),
  check.names = FALSE
)

notes_df <- data.frame(
  "Note number" = paste0("Note ", 1),
  "Note text" = c("Comparisons are made as a percentage change between 2026 to the 5 year average calculated from 2020 to 2024."
                  
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
      "Scottish Agricultural Census: June 2026 Results",
      "Table of contents.",
      "Notes.",
      "Table 1: Number of holdings with crops and grass by Unitary Authority, June 2026",
      "Table 2: Area of crops and grass by Unitary Authority, June 2026",
      "Table 3: Number of holdings with livestock by Unitary Authority, June 2026",
      "Table 4: Number of livestock by Unitary Authority, June 2026",
      "Table 5: Number of holdings with occupiers and employees by Unitary Authority, June 2026",
      "Table 6: Number of occupiers and employees by Unitary Authority, June 2026",
      "Table 7: Number of holdings by main farm type and by Unitary Authority, June 2026",
      "Table 8: Area of holdings by main farm type and by Unitary Authority, June 2026"
      
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
      "[JAC 2026](https://www.gov.scot/publications/results-from-the-scottish-agricultural-census-june-2026//)",
      "[JAC 2026](https://www.gov.scot/publications/results-from-the-scottish-agricultural-census-june-2026//)",
      "[JAC 2026](https://www.gov.scot/publications/results-from-the-scottish-agricultural-census-june-2026//)",
      "[JAC 2026](https://www.gov.scot/publications/results-from-the-scottish-agricultural-census-june-2026//)",
      "[JAC 2026](https://www.gov.scot/publications/results-from-the-scottish-agricultural-census-june-2026//)",
      "[JAC 2026](https://www.gov.scot/publications/results-from-the-scottish-agricultural-census-june-2026//)",
      "[JAC 2026](https://www.gov.scot/publications/results-from-the-scottish-agricultural-census-june-2026//)",
      "[JAC 2026](https://www.gov.scot/publications/results-from-the-scottish-agricultural-census-june-2026//)"
      
      
      
    ),
    tables = list(cover_list, contents_df, notes_df, table_01, table_02, table_03, table_04,
                  table_05, table_06, table_07, table_08
    )
  )
my_wb1 <- aftables::generate_workbook(jac_aftables)
openxlsx::saveWorkbook(my_wb1, paste0("Data/unitary_authority_accessible_tables_", format(Sys.Date(), "%Y_%m_%d"), ".xlsx"))

