###########################
####
#### Map geojson processing - constituencies
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
constituencies <- st_read("utility/new boundaries/SP_2nd_Review_Constituencies_Final_Recommendations.shp")

mapping_data <- data.frame(
  region = c("Central Scotland and Lothians West", "Central Scotland and Lothians West",
             "Central Scotland and Lothians West", "Central Scotland and Lothians West",
             "Central Scotland and Lothians West", "Central Scotland and Lothians West",
             "Central Scotland and Lothians West", "Central Scotland and Lothians West",
             "Central Scotland and Lothians West", "Edinburgh and Lothians East",
             "Edinburgh and Lothians East", "Edinburgh and Lothians East",
             "Edinburgh and Lothians East", "Edinburgh and Lothians East",
             "Edinburgh and Lothians East", "Edinburgh and Lothians East",
             "Edinburgh and Lothians East", "Edinburgh and Lothians East",
             "Glasgow", "Glasgow", "Glasgow", "Glasgow", "Glasgow", "Glasgow",
             "Glasgow", "Glasgow", "Highlands and Islands", "Highlands and Islands",
             "Highlands and Islands", "Highlands and Islands", "Highlands and Islands",
             "Highlands and Islands", "Highlands and Islands", "Highlands and Islands",
             "Mid Scotland and Fife", "Mid Scotland and Fife", "Mid Scotland and Fife",
             "Mid Scotland and Fife", "Mid Scotland and Fife", "Mid Scotland and Fife",
             "Mid Scotland and Fife", "Mid Scotland and Fife", "Mid Scotland and Fife",
             "North East Scotland", "North East Scotland", "North East Scotland",
             "North East Scotland", "North East Scotland", "North East Scotland",
             "North East Scotland", "North East Scotland", "North East Scotland",
             "North East Scotland", "South Scotland", "South Scotland", "South Scotland",
             "South Scotland", "South Scotland", "South Scotland", "South Scotland",
             "South Scotland", "South Scotland", "South Scotland",
             "West Scotland", "West Scotland", "West Scotland", "West Scotland",
             "West Scotland", "West Scotland", "West Scotland", "West Scotland",
             "West Scotland", "West Scotland"
  ),
  constituency = c("Airdrie", "Almond Valley", "Bathgate", "Coatbridge and Chryston",
                   "Cumbernauld and Kilsyth", "Falkirk East and Linlithgow",
                   "Falkirk West", "Motherwell and Wishaw", "Uddingston and Bellshill",
                   "East Lothian Coast and Lammermuirs", "Edinburgh Central",
                   "Edinburgh Eastern, Musselburgh and Tranent",
                   "Edinburgh North Eastern and Leith",
                   "Edinburgh North Western", "Edinburgh Northern",
                   "Edinburgh South Western", "Edinburgh Southern", "Midlothian North",
                   "Glasgow Anniesland", "Glasgow Baillieston and Shettleston",
                   "Glasgow Cathcart and Pollok", "Glasgow Central",
                   "Glasgow Easterhouse and Springburn", "Glasgow Kelvin and Maryhill",
                   "Glasgow Southside", "Rutherglen and Cambuslang",
                   "Argyll and Bute", "Caithness, Sutherland and Ross",
                   "Inverness and Nairn", "Moray", "Na h-Eileanan an Iar",
                   "Orkney Islands", "Shetland Islands", "Skye, Lochaber and Badenoch",
                   "Clackmannanshire and Dunblane", 
                   "Cowdenbeath", "Dunfermline", "Fife North East", "Kirkcaldy",
                   "Mid Fife and Glenrothes", "Perthshire North",
                   "Perthshire South and Kinross-shire", "Stirling",
                   "Aberdeen Central", "Aberdeen Deeside and North Kincardine",
                   "Aberdeen Donside", "Aberdeenshire East", "Aberdeenshire West",
                   "Angus North and Mearns", "Angus South", "Banffshire and Buchan Coast",
                   "Dundee City East", "Dundee City West", 
                   "Ayr", "Carrick, Cumnock and Doon Valley", "Clydesdale",
                   "Dumfriesshire", "East Kilbride", "Ettrick, Roxburgh and Berwickshire",
                   "Galloway and West Dumfries", "Hamilton, Larkhall and Stonehouse",
                   "Kilmarnock and Irvine Valley", "Midlothian South, Tweeddale and Lauderdale",
                   "Clydebank and Milngavie", "Cunninghame North", "Cunninghame South",
                   "Dumbarton", "Eastwood", "Inverclyde", "Paisley", "Renfrewshire North and Cardonald",
                   "Renfrewshire West and Levern Valley", "Strathkelvin and Bearsden"
  )
)

# Merge the shapefile with the mapping data
constit_shp <- constituencies %>%
  left_join(mapping_data, by = c("NAME" = "constituency"))

# Ensure geometries are valid
constit_shp <- st_make_valid(constit_shp)

constit_shp_subset <- constit_shp %>%
  group_by(NAME) %>%
  summarise(geometry = st_union(geometry))

st_write(constit_shp_subset, "utility/constituencies.geojson", driver = "GeoJSON")

# Load the GeoJSON file
geojson_data <- geojson_read("utility/constituencies.geojson", what = "sp")

geojson_data <- ms_simplify(geojson_data, keep = 0.001)

geojson_write(geojson_data, file = "utility/constituencies_simplified.geojson")
