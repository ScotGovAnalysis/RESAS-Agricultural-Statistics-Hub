###########################
####
#### Map geojson processing - unitary authorities
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
unitary_authorities <- st_read("utility/Local Authority Boundaries Scotland/pub_las.shp")

# Ensure geometries are valid
unitary_authorities <- st_make_valid(unitary_authorities)

unitary_authorities_subset <- unitary_authorities %>%
  group_by(local_auth) %>%
  summarise(geometry = st_union(geometry))

st_write(unitary_authorities_subset, "utility/unitary_authorities.geojson", driver = "GeoJSON")

# Load the GeoJSON file
geojson_data <- geojson_read("utility/unitary_authorities.geojson", what = "sp")

geojson_data <- ms_simplify(geojson_data, keep = 0.001)

geojson_write(geojson_data, file = "utility/unitary_authorities_simplified.geojson")
