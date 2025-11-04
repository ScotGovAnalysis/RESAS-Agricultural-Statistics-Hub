
# File: module_crops_summary.R

# Reactive data setup for Crops
full_data_crops <- reactive({
  crops_summary_data 
})



# UI for Summary Crops Module
cropsSummaryUI <- function(id) {
  ns <- NS(id)
  tagList(
    sidebarLayout(
      sidebarPanel(
        width = 3,
        div("Adjust the sliders to compare data from different years.", 
            style = "font-size: 14px; font-weight: bold; margin-bottom: 10px;"),
        sliderInput(ns("summary_current_year_crops"), "Year of interest", min = 2013, max = census_year, value = census_year, step = 1, sep = ""),
        sliderInput(ns("summary_comparison_year_crops"), "Comparison year", min = 2013, max = census_year, value = census_year - 1, step = 1, sep = "")
      ),
      mainPanel(
        id = ns("mainpanel"),
        width = 9,
        tabsetPanel(
          id = ns("tabs"),
          tabPanel("Summary Page",
                   value = "Summary_Page",
                   fluidRow(
                     column(width = 6, valueBoxUI(ns("Total Combine Harvested Crops")), style = "padding-right: 0; padding-left: 0; padding-bottom: 10px;"),
                     column(width = 6, valueBoxUI(ns("Total Crops For Stockfeeding")), style = "padding-right: 0; padding-left: 0; padding-bottom: 10px;")
                   ),
                   fluidRow(
                     column(width = 6, valueBoxUI(ns("Vegetables For Human Consumption")), style = "padding-right: 0; padding-left: 0;"),
                     column(width = 6, valueBoxUI(ns("Soft Fruit")), style = "padding-right: 0; padding-left: 0;")
                   ),
                   # Add the footer text
                   div(
                     style = "margin-top: 20px; padding: 10px; border-top: 1px solid #ddd;",
                     
                   )
          ),
          tabPanel("Data Table",
                   fluidRow(
                     column(12, 
                            DTOutput(ns("data_table")),
                            tags$div(
                              style = "margin-top: 20px;",
                              downloadButton(ns("download_data"), "Download Data")
                            )
                     )
                   )
          ),
          footer = generateCensusTableFooter() 
        )
      )
    )
  )
}

library(scales)

cropsSummaryServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    current_year <- reactive({ input$summary_current_year_crops })
    comparison_year <- reactive({ input$summary_comparison_year_crops })
    
    valueBoxServer("Total Combine Harvested Crops", full_data_crops, "Crop/Land use", reactive("Total Combine Harvested Crops"), current_year, comparison_year, "hectares")
    valueBoxServer("Total Crops For Stockfeeding", full_data_crops, "Crop/Land use", reactive("Total Crops For Stockfeeding"), current_year, comparison_year, "hectares")
    valueBoxServer("Vegetables For Human Consumption", full_data_crops, "Crop/Land use", reactive("Vegetables For Human Consumption"), current_year, comparison_year, "hectares")
    valueBoxServer("Soft Fruit", full_data_crops, "Crop/Land use", reactive("Soft Fruit"), current_year, comparison_year, "hectares")
    
    # Pivot the data wider for the data table and format numbers with commas, excluding the 'Year' column
    pivoted_data <- reactive({
      full_data_crops() %>%
        pivot_wider(names_from = `Crop/Land use`, values_from = `Value`) %>%
        mutate(across(where(is.numeric) & !contains("Year"), comma))  # Format all numeric columns except 'Year' with commas
    })
    
    # Render the pivoted data table
    output$data_table <- renderDT({
      datatable(pivoted_data(), options = list(
        scrollX = TRUE,   # Enable horizontal scrolling
        pageLength = 20   # Show 20 entries by default
      ))
    })
    
    # Download handler for the pivoted data
    output$download_data <- downloadHandler(
      filename = function() {
        paste("Crops_Summary_Data_", Sys.Date(), ".csv", sep = "")
      },
      content = function(file) {
        write.csv(pivoted_data(), file, row.names = FALSE)
      }
    )
  })
}



# Testing module
content_demo <- function() {
  ui <- fluidPage(cropsSummaryUI("summary_crops_test"))
  server <- function(input, output, session) {
    cropsSummaryServer("summary_crops_test")
  }
  shinyApp(ui, server)
}

content_demo()
