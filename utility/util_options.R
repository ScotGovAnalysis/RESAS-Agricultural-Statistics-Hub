##options.R
# Libraries
library(shiny)
library(highcharter)
library(tidyverse)
library(shinyWidgets)
library(shinythemes)
library(shinydashboard)
library(rsconnect)
library(png)
library(htmltools)
library(DT)
library(shinyjs)
library(RColorBrewer)
library(shinyjs)
library(tidyr)
library(geojsonio)
library(scales)

# Load the .RData file containing the datasets
load("Data/ghg_data.RData")
load("Data/census_data.RData")
load("Data/crops_data.RData")
load("Data/total_animals.RData")
load("Data/module_2023.RData")
load("Data/vehicle_data.RData")
load("Data/manure_fertiliser.RData")
load("Data/FBS_data.Rda")

# Load FBS options
source("Economy/FBS/fbs_utility.R")


# Highchart options
hcoptslang <- getOption("highcharter.lang")
hcoptslang$thousandsSep <- ","
hcoptslang$numericSymbols <- " "
options(highcharter.lang = hcoptslang)

# Load the theme
thm <- source("utility/hc_theme.R")$value


# Preset list of colors
preset_colors <- c("#002d54", "#2b9c93", "#6a2063", "#e5682a", "#0b4c0b", "#5d9f3c", "#592c20", "#ca72a2")

# Function to assign colors to variables
assign_colors <- function(data, colors) {
  first_col_name <- names(data)[1]
  variables <- unique(data[[first_col_name]])
  setNames(colors[1:length(variables)], variables)
}

safe_as_numeric <- function(x) {
  suppressWarnings({
    result <- as.numeric(x)
    if (is.na(result) && !is.na(x)) {
      return(NA_real_)
    }
    return(result)
  })
}

