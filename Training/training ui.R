### training UI script - demo-ing module use in shiny app

### --------------------------------
### See script (utility/util_packages_need) to see what packages that
### you may need to install.

### ------------------------------
### Load utility scripts
source(here("utility", "util_updates.R"))
source(here("utility", "util_functions.R"))
source(here("utility", "util_options.R"))
source(here("utility", "hc_theme.R"))
###------------------------------
### Load libraries
# library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(highcharter)
library(scales)
library(shiny)
library(highcharter)
library(geojsonio)
###------------------------------
### Load scripts with functions used to create charts in module scripts
source(here("charts_tables_functions", "function_summary.R"))
source(here("charts_tables_functions", "function_line_chart.R"))
source(here("charts_tables_functions", "function_area_chart.R"))
source(here("charts_tables_functions", "function_bar_chart.R"))
source(here("charts_tables_functions", "function_percentage_bar_chart.R"))
source(here("charts_tables_functions", "function_timelapse_bar_chart.R"))
source(here("charts_tables_functions", "function_breakdown_chart.R"))
source(here("charts_tables_functions", "function_data_table.R"))
source(here("charts_tables_functions", "function_regions_map.R"))
source(here("charts_tables_functions", "function_map.R"))
source(here("charts_tables_functions", "function_multi_bar_chart.R"))
###-----------------------------
### Load training module
source(here("Training", "training module.R"))
###-----------------------------
### Load census data 
load(here("Data", "census_data.Rdata"))
###----------------------------
### Define main app ui and call in UI from training module


ui <- fluidPage(trainingUI("cattle_test"))


