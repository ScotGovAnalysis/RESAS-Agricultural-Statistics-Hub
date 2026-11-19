
############################
####
#### Map geojson processing
####
############################

### This code should not need to be modified or rerun, as the geojson is saved within the file,
### unless changes to regions occur.


library(sf)
library(dplyr)
library(highcharter)
library(geojsonio)
library(rmapshaper)

# Load the shapefile
local_authorities <- st_read("utility/Local_Authority_Boundaries_-_Scotland/pub_las.shp")

mapping_data <- data.frame(
  region = c("North West", "North West", "North West", "North West", 
             "North East", "North East", "North East", 
             "South East", "South East", "South East", "South East", "South East", 
             "South East", "South East", "South East", "South East",
             "South West", "South West", "South West", "South West", "South West", 
             "South West", "South West", "South West", "South West", "South West", 
             "South West", "South West", "South West", "South West", "South West", "South West"
  ),
  sub_region = c("Shetland", "Orkney", "Na h-Eileanan Siar", "Highland", 
                 "Grampian", "Grampian", "Grampian", 
                 "Tayside", "Tayside", "Tayside", "Fife", "Lothian", "Lothian", "Lothian", "Lothian", 
                 "Scottish Borders", "East Central", "East Central", "East Central", 
                 "Argyll & Bute", "Clyde Valley", "Clyde Valley", "Clyde Valley", "Clyde Valley", 
                 "Clyde Valley", "Clyde Valley", "Clyde Valley", "Clyde Valley", "Ayrshire", "Ayrshire", "Ayrshire", 
                 "Dumfries & Galloway"),
  local_authority = c("Shetland Islands", "Orkney Islands", "Na h-Eileanan an Iar", "Highland", 
                      "Aberdeen City", "Aberdeenshire", "Moray", 
                      "Angus", "Dundee City", "Perth and Kinross", "Fife", "East Lothian", "City of Edinburgh", 
                      "Midlothian", "West Lothian", "Scottish Borders", 
                      "Clackmannanshire", "Falkirk", "Stirling", 
                      "Argyll and Bute", "East Dunbartonshire", "East Renfrewshire", "Glasgow City", 
                      "Inverclyde", "North Lanarkshire", "Renfrewshire", "South Lanarkshire", 
                      "West Dunbartonshire", "East Ayrshire", "North Ayrshire", "South Ayrshire", 
                      "Dumfries and Galloway")
)

# Merge the shapefile with the mapping data
local_authorities <- local_authorities %>%
  left_join(mapping_data, by = c("local_auth" = "local_authority"))

# Ensure geometries are valid
local_authorities <- st_make_valid(local_authorities)

sub_regions <- local_authorities %>%
  group_by(sub_region) %>%
  summarise(geometry = st_union(geometry))

st_write(sub_regions, "sub_regions.geojson", driver = "GeoJSON")

# Load the GeoJSON file
geojson_data <- geojson_read("sub_regions.geojson", what = "sp")

geojson_data <- ms_simplify(geojson_data, keep = 0.001)

geojson_write(geojson_data, file = "subregions_simplified.geojson")


regions <- local_authorities %>%
  group_by(region) %>%
  summarise(geometry = st_union(geometry))

st_write(regions, "regions.geojson", driver = "GeoJSON")

Sys.setenv(OGR_GEOJSON_MAX_OBJ_SIZE = "0")

regions_geojson <- geojson_read("regions.geojson", what = "sp")

regions_geojson <- ms_simplify(regions_geojson, keep = 0.001)

geojson_write(regions_geojson, file = "regions_simplified.geojson")