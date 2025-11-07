soilTestingUI <- function(id) {
  ns <- NS(id)
  tagList(
    tabsetPanel(
      tabPanel("Soil Testing",
               fluidRow(
                 column(12,
                        div(style = "font-size: 20px; font-weight: bold;", "Soil testing is more likely to be carried out on cropping land than grassland (2023)"),
                        percentageBarChartUI(ns("soilTestingOverview"), chart_height = 420)
                 )
               ),
               fluidRow(
                 column(12,
                        div(style = "font-size: 20px; font-weight: bold;", "Many holdings made changes to nutrient application based on soil testing (2023)"),
                        percentageBarChartUI(ns("soilTestingEffectiveness"), chart_height = 240)
                 )
               ),
               fluidRow(
                 column(12,
                        div(style = "font-size: 20px; font-weight: bold;", "pH testing is more likely to be carried out on cropping land than grassland (2023)"),
                        percentageBarChartUI(ns("phTestingEffectiveness"), chart_height = 340)
                 )
               )
      ),
      tabPanel("Nutrient Management Plans",
               fluidRow(
                 column(12,
                        div(style = "font-size: 20px; font-weight: bold;", "Nutrient management plans are more likely to be conducted on cropping land than grassland (2023)"),
                        percentageBarChartUI(ns("nutrientManagementOverview"), chart_height = 480)
                 )
               ),
               fluidRow(
                 column(12,
                        div(style = "font-size: 20px; font-weight: bold;", "Few holdings regularly update a nutrient management plan (2023)"),
                        percentageBarChartUI(ns("nutrientManagementEffectiveness"), chart_height = 240)
                 )
               )
      ),
      tabPanel("Data Table",
               fluidRow(
                 column(3,
                        radioButtons(
                          ns("table_type"),
                          "Select Chart Type",
                          choices = c("Soil Testing", "Nutrient Management"),
                          selected = "Soil Testing"
                        )
                 ),
                 column(9,
                        DTOutput(ns("table")),
                        tags$div(
                          style = "margin-top: 20px;",
                          downloadButton(ns("download_data"), "Download Data")
                        )
                 )
               )
      )
    ),
    generate2023ModuleTableFooter()  # Place the footer outside the tabsetPanel, but still inside the mainPanel
  )
}



# Server for Soil Testing and Nutrient Management Module
soilTestingServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Data filtering and preparation for Soil Testing
    overview_data <- reactive({
      combined_nutrient_mgmt %>% 
        filter(`Soil nutrient management` %in% c(
          "Soil testing on grassland in last five years",
          "Soil testing on cropland in last five years",
          "Soil testing on either grass or crops in last five years"
        )) 
    })
    
    effectiveness_data <- reactive({
      combined_nutrient_mgmt %>%
        filter(`Soil nutrient management` %in% c(
          "Soil testing resulted in change of crop nutrient application"
        ))
    })
    
    ph_testing_data <- reactive({
      combined_nutrient_mgmt %>%
        filter(`Soil nutrient management` %in% c(
          "Regular pH testing on grassland",
          "Regular pH testing on cropland"
        ))
    })
    
    # Data filtering and preparation for Nutrient Management
    nutrient_management_overview_data <- reactive({
      combined_nutrient_mgmt %>%
        filter(`Soil nutrient management` %in% c(
          "Nutrient management plan used on grassland",
          "Nutrient management plan used on cropland"
        ))
    })
    
    nutrient_management_effectiveness_data <- reactive({
      combined_nutrient_mgmt %>%
        filter(`Soil nutrient management` %in% c(
          "Wrote or updated nutrient management plan in last year"
        ))
    })
    
    precision_farming_data <- reactive({
      combined_nutrient_mgmt %>%
        filter(`Soil nutrient management` %in% c(
          "Uses precision farming technology to vary nitrogen application rate"
        ))
    })
    
    # Bar charts for Soil Testing
    percentageBarChartServer(
      id = "soilTestingOverview",
      chart_data = overview_data,
      title = NULL,  # Titles handled by the fluid rows
      yAxisTitle = "Percentage of Holdings",
      xAxisTitle = "",
      x_col = "Soil nutrient management",
      y_col = "Percentage of holdings",
      unit = "%"
    )
    
    percentageBarChartServer(
      id = "soilTestingEffectiveness",
      chart_data = effectiveness_data,
      title = NULL,
      yAxisTitle = "Percentage of Holdings",
      xAxisTitle = "",
      x_col = "Soil nutrient management",
      y_col = "Percentage of holdings",
      unit = "%"
    )
    
    percentageBarChartServer(
      id = "phTestingEffectiveness",
      chart_data = ph_testing_data,
      title = NULL,
      yAxisTitle = "Percentage of Holdings",
      xAxisTitle = "",
      x_col = "Soil nutrient management",
      y_col = "Percentage of holdings",
      unit = "%"
    )
    
    # Bar charts for Nutrient Management
    percentageBarChartServer(
      id = "nutrientManagementOverview",
      chart_data = nutrient_management_overview_data,
      title = NULL,  # Titles handled by the fluid rows
      yAxisTitle = "Percentage of Holdings",
      xAxisTitle = "",
      x_col = "Soil nutrient management",
      y_col = "Percentage of holdings",
      unit = "%"
    )
    
    percentageBarChartServer(
      id = "nutrientManagementEffectiveness",
      chart_data = nutrient_management_effectiveness_data,
      title = NULL,
      yAxisTitle = "Percentage of Holdings",
      xAxisTitle = "",
      x_col = "Soil nutrient management",
      y_col = "Percentage of holdings",
      unit = "%"
    )
    
    # Reactive data based on selected chart type
    table_data <- reactive({
      if (input$table_type == "Soil Testing") {
        combined_nutrient_mgmt %>% 
          filter(`Soil nutrient management` %in% c(
            "Soil testing on grassland in last five years",
            "Soil testing on cropland in last five years",
            "Soil testing on either grass or crops in last five years"
           
          )) %>%
              rename(`Soil testing` = `Soil nutrient management` )
      } else {
        combined_nutrient_mgmt %>%
          filter(`Soil nutrient management` %in% c(
            "Nutrient management plan used on grassland",
            "Nutrient management plan used on cropland"
          ))
      }
    })
    
    # Render data table
    output$table <- renderDT({
      datatable(table_data(), options = list(scrollX = TRUE))
    })
    
    # Download handler for data
    output$download_data <- downloadHandler(
      filename = function() {
        paste("Soil_testing_data", Sys.Date(), ".csv", sep = "")
      },
      content = function(file) {
        write.csv(table_data(), file, row.names = FALSE)
      }
    )

  })
}

# Testing function for the entire module
soilTestingDemo <- function() {
  ui <- fluidPage(soilTestingUI("soil_testing_demo"))
  server <- function(input, output, session) {
    soilTestingServer("soil_testing_demo")
  }
  shinyApp(ui, server)
}

soilTestingDemo()
