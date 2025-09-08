#####################################
####
#### When updating census data, ensure to update the census_year, and relevant footers below. 
#### Include title, publication dates, link to publication
####
#####################################

### IMPORTANT
#  WHENEVER YOU ARE UPDATING THE APP, ENSURE TO UPDATE THE FOOTER, IN UI.R WITH THE CURRENT DATE


# Year of census data - some years need manually updated, breakdown server in emissions

census_year <- 2024

emissions_year <- 2023

fbs_year <-2023-24 

#emissions_year 

#some footers need manually updated - e.g. poultry - run print_code and search for 2022 / 2023 to find issues
census_footer <- '<div style="font-size: 16px; font-weight: bold;"><a href="https://www.gov.scot/publications/results-from-the-scottish-agricultural-census-june-2024/">Source: Scottish Agricultural Census: June 2024</a></div>'

emissions_footer <- '<div style="font-size: 16px; font-weight: bold;"> <a href="https://www.gov.scot/collections/scottish-agriculture-greenhouse-gas-emissions-and-nitrogen-use/" target="_blank">Source: Scottish agriculture greenhouse gas emissions and nitrogen use 2023-24</a>, analysis based on results of the <a href="https://www.gov.scot/publications/scottish-greenhouse-gas-statistics-2023/" target="_blank">Scottish Greenhouse Gas Statistics 2023</a>.</div>'

fbs_footer <- HTML(
  '<div style="font-size: 16px;">
    <strong>Real (constant) prices</strong>: figures are adjusted for inflation using 
    <a href="https://www.gov.uk/government/statistics/gdp-deflators-at-market-prices-and-money-gdp-december-2024-quarterly-national-accounts" target="_blank">
      GDP deflators published 23 December 2024
    </a>
  </div>
  <div style="font-size: 16px; font-weight: bold; margin-top: 8px;">
    <a href="https://www.gov.scot/collections/scottish-farm-business-income-fbi-annual-estimates/" target="_blank">
      Source: Scottish farm business income: annual estimates 2023–24
    </a>
  </div>'
)


tiff_footer <- '<div style="font-size: 16px; font-weight: bold;"> <a href="https://www.gov.scot/publications/total-income-from-farming-estimates-2018-2024/">Source: Total income from farming estimates: 2018-2024</a></div>'


# Function to generate the census data table footer with a light grey background
generateCensusTableFooter <- function() {
  div(
    style = "background-color: #f0f0f0; padding: 15px; border-radius: 5px; margin-top: 20px;",
    "This data is sourced from the ",
    tags$a(href = "https://www.gov.scot/publications/results-from-the-scottish-agricultural-census-june-2024/",
           "Scottish Agricultural Census: June 2024"),
    " which was published on 24 October 2024.",
    tags$br(),
    "Where data is unavailable, findings have been suppressed to prevent disclosure of individual holdings.",
    tags$br(),
    "Full data tables and detailed analysis are available within the full report."
  )
}

generateEmissionsTableFooter <- function() {
  div(
    style = "background-color: #f0f0f0; padding: 15px; border-radius: 5px; margin-top: 20px;",
    "This data is sourced from the ",
    tags$a(href = "https://www.gov.scot/collections/scottish-agriculture-greenhouse-gas-emissions-and-nitrogen-use/",
           "Scottish agriculture greenhouse gas emissions and nitrogen use 2023-24"),
    " which was published on 10 June 2025, with its analysis based on the ",
    tags$a(href = "https://www.gov.scot/publications/scottish-greenhouse-gas-statistics-2023/", 
           "Scottish Greenhouse Gas Statistics 2023."),
    tags$br(),
    "Full data tables and detailed analysis are available within the full reports."
  )
}


generate2023ModuleTableFooter <- function() {
  div(
    style = "background-color: #f0f0f0; padding: 15px; border-radius: 5px; margin-top: 20px;",
    "This data is sourced from the ",
    tags$a(href = "https://www.gov.scot/publications/results-from-the-scottish-agricultural-census-module-june-2023/",
           "Scottish Agricultural Census: Module June 2023"),
    " which was published on 23 May 2024.",
    tags$br(),
    "This module provides insights into soil cover, tillage, irrigation, nutrient management, and fertiliser application and storage.",
    tags$br(),
    "The report highlights the significant role these practices play in reducing emissions and improving soil and nutrient management across Scottish agricultural holdings.",
    tags$br(),
    "Information about the presentation of nitrogen usage results is available in the ",
    tags$a(href = "https://www.gov.scot/publications/results-from-the-scottish-agricultural-census-module-june-2023/pages/data-and-methodology/", "data and methodology."),
    tags$br(),
    "Where data is unavailable, findings have been suppressed to prevent disclosure of individual holdings.",
    tags$br(),
    "Full data tables and detailed analysis are available within the full report."
  )
}

# Function to generate the census data table footer with a light grey background
generateFBSTableFooter <- function() {
  div(
    style = "background-color: #f0f0f0; padding: 15px; border-radius: 5px; margin-top: 20px;",
    "This data is sourced from ",
    tags$a(href = "https://www.gov.scot/collections/scottish-farm-business-income-fbi-annual-estimates/",
           "Scottish farm business income: annual estimates 2023-24"),
    " which was published on 3 April 2025.",
    tags$br(),
    "Full data tables and detailed analysis are available within the full report."
  )
}

generatetiffTableFooter <- function() {
  div(
    style = "background-color: #f0f0f0; padding: 15px; border-radius: 5px; margin-top: 20px;",
    "This data is sourced from the ",
    tags$a(href = "https://www.gov.scot/publications/total-income-from-farming-estimates-2018-2024/",
           "Total income from farming estimates: 2018-2024"),
    " which was published on 29 May 2025.",
    tags$br(),
    "Total income from farming (TIFF) is the official measure of the profit gained by the agricultural industry in Scotland.",
    tags$br(),
    "The report provides a breakdown of the value of farm production, support payments and costs.",
    tags$br(),
    "Where data is unavailable, findings have been suppressed to prevent disclosure of individual holdings.",
    tags$br(),
    "Full data tables and detailed analysis are available within the full report."
  )
}

