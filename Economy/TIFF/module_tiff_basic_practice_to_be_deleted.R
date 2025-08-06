library(shiny)
library(DT)
library(dplyr)

# Dummy lineChartUI and lineChartServer definitions (replace with your actual module)
lineChartUI <- function(id) {
  ns <- NS(id)
  tagList(
    plotOutput(ns("plot"))
  )
}

lineChartServer <- function(id, chart_data, title, yAxisTitle, xAxisTitle, footer, x_col, y_col) {
  moduleServer(id, function(input, output, session) {
    output$plot <- renderPlot({
      df <- chart_data()
      req(df)
      plot(df[[x_col]], df[[y_col]], type = "l", main = title, 
           xlab = xAxisTitle, ylab = yAxisTitle)
    })
  })
}

# Minimal tiffUI
tiffUI <- function(id) {
  ns <- NS(id)
  fluidPage(
    sidebarLayout(
      sidebarPanel(
        # Just a fixed variable for minimal example
        checkboxGroupInput(ns("selected_var"), "Select variables",
                           choices = c("Var1", "Var2"), selected = "Var1"),
        sliderInput(ns("selected_year"), "Select year range", 
                    min = 1989, max = 2024, value = c(2014, 2024))
      ),
      mainPanel(
        lineChartUI(ns("line"))
      )
    )
  )
}

# Minimal tiffServer
tiffServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    # Dummy data simulating your main_tiff_data_long
    main_tiff_data_long <- data.frame(
      Measure = rep(c("Var1", "Var2"), each = 21),
      Year = rep(1989:2024, 2),
      Value = c(seq(1, 21), seq(21, 1))
    )
    
    chart_data <- reactive({
      req(input$selected_var, input$selected_year)
      year_start <- input$selected_year[1]
      year_end <- input$selected_year[2]
      
      main_tiff_data_long %>%
        filter(Measure %in% input$selected_var,
               Year >= year_start, Year <= year_end)
    })
    
    lineChartServer(
      id = "line",
      chart_data = chart_data,
      title = "TIFF Timeseries",
      yAxisTitle = "Value",
      xAxisTitle = "Year",
      footer = NULL,
      x_col = "Year",
      y_col = "Value"
    )
  })
}

# Run the app
shinyApp(
  ui = tiffUI("tiff"),
  server = function(input, output, session) {
    tiffServer("tiff")
  }
)

