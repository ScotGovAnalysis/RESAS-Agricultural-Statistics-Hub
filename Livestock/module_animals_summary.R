# module_summary_animals.R

# Reactive data setup for Animals
full_data_animals <- reactive({
  total_animals %>%
    pivot_longer(
      cols = starts_with("Total"),
      names_to = "Animal_Type",
      values_to = "Value"
    ) %>%
    mutate(Value = as.numeric(Value))  # Ensure the 'Value' column is numeric
})


animalsSummaryUI <- function(id) {
  ns <- NS(id)
  tagList(
    sidebarLayout(
      sidebarPanel(
        width = 3,
        div("Adjust the sliders to compare data from different years.", 
            style = "font-size: 14px; font-weight: bold; margin-bottom: 10px;"),
        sliderInput(ns("summary_current_year_animals"), "Year of interest", min = 2012, max = census_year, value = census_year, step = 1, sep = ""),
        sliderInput(ns("summary_comparison_year_animals"), "Comparison year", min = 2012, max = census_year, value = census_year - 1, step = 1, sep = "")
      ),
      mainPanel(
        id = ns("mainpanel"),
        width = 9,
        tabsetPanel(
          id = ns("tabs"),
          tabPanel("Summary Page",
                   value = "Summary_Page",
                   fluidRow(
                     column(width = 6, valueBoxUI(ns("Total cattle")), style = "padding-right: 0; padding-left: 0; padding-bottom: 10px;"),
                     column(width = 6, valueBoxUI(ns("Total Sheep")), style = "padding-right: 0; padding-left: 0; padding-bottom: 10px;")
                   ),
                   fluidRow(
                     column(width = 6, valueBoxUI(ns("Total Pigs")), style = "padding-right: 0; padding-left: 0;"),
                     column(width = 6, valueBoxUI(ns("Total Poultry")), style = "padding-right: 0; padding-left: 0;")
                   ),
                   # Add the footer text
                   div(
                     style = "margin-top: 20px; padding: 10px; border-top: 1px solid #ddd;",
                     HTML("<strong>Note:</strong> Poultry estimates for 2023 are not comparable to previous years due to methodological improvements.")
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

animalsSummaryServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    current_year <- reactive({ input$summary_current_year_animals })
    comparison_year <- reactive({ input$summary_comparison_year_animals })
    
    valueBoxServer("Total cattle", full_data_animals, "Animal_Type", reactive("Total cattle"), current_year, comparison_year, "cattle")
    valueBoxServer("Total Sheep", full_data_animals, "Animal_Type", reactive("Total Sheep"), current_year, comparison_year, "sheep")
    valueBoxServer("Total Pigs", full_data_animals, "Animal_Type", reactive("Total Pigs"), current_year, comparison_year, "pigs")
    valueBoxServer("Total Poultry", full_data_animals, "Animal_Type", reactive("Total Poultry"), current_year, comparison_year, "poultry")
    
    # Pivot the data wider
    pivoted_data <- reactive({
      full_data_animals() %>%
        pivot_wider(names_from = Animal_Type, values_from = Value) %>%
        mutate(across(where(is.numeric) & !contains("Year"), comma))
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
        paste("Animal_Summary_Data_", Sys.Date(), ".csv", sep = "")
      },
      content = function(file) {
        write.csv(pivoted_data(), file, row.names = FALSE)
      }
    )
  })
}



# Testing module
content_demo <- function() {
  ui <- fluidPage(animalsSummaryUI("summary_animals_test"))
  server <- function(input, output, session) {
    animalsSummaryServer("summary_animals_test")
  }
  shinyApp(ui, server)
}

content_demo()
