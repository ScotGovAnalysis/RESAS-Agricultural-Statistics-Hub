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
library(here)

# Load the .RData file containing the datasets
load(here("Data", "ghg_data.RData"))
load(here("Data", "census_data.RData"))
load(here("Data", "crops_data.RData"))
load(here("Data", "total_animals.RData"))
load(here("Data", "module_2023.RData"))
load(here("Data", "vehicle_data.RData"))
load(here("Data", "manure_fertiliser.RData"))
load(here("Data", "FBS_data.Rda"))
load(here("Data", "TIFF_data.Rda"))

# Load FBS options
source(here("Economy/FBS", "fbs_utility.R"))


# Highchart options
hcoptslang <- getOption("highcharter.lang")
hcoptslang$thousandsSep <- ","
hcoptslang$numericSymbols <- " "
options(highcharter.lang = hcoptslang)

# Load the theme
thm <- source(here("utility", "hc_theme.R"))$value


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

