# Module UI
doubleBarChartUI <- function(id) {
  ns <- NS(id)
  tagList(
    highchartOutput(ns("doubleBarChart"))
  )
}

# Module Server
doubleBarChartServer <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    output$doubleBarChart <- renderHighchart({
      # Reshape data
      reshaped_data <- data %>%
        pivot_longer(cols = -`Occupier working time`, names_to = "Category", values_to = "Value")
      
      # Create a list of unique categories for series
      categories <- unique(reshaped_data$Category)
      
      # Create a highchart object
      hc <- highchart() %>%
        hc_chart(type = "column") %>%
        hc_title(text = "Occupiers by Employment Status and Age Group") %>%
        hc_xAxis(categories = unique(reshaped_data$`Occupier working time`), title = list(text = "Category")) %>%
        hc_yAxis(title = list(text = "Number of Occupiers")) %>%
        hc_plotOptions(column = list(dataLabels = list(enabled = TRUE))) %>%
        hc_legend(enabled = TRUE)
      
      # Add series to the chart
      for (cat in categories) {
        series_data <- reshaped_data %>%
          filter(Category == cat) %>%
          arrange(`Occupier working time`)
        
        hc <- hc %>%
          hc_add_series(
            name = cat,
            data = series_data$Value
          )
      }
      
      hc
    })
  })
}

# Testing module
doubleBarChart_demo <- function() {
  # Assumes occupiers_age_gender is already loaded in the environment
  
  ui <- fluidPage(
    doubleBarChartUI("x")
  )
  
  server <- function(input, output, session) {
    doubleBarChartServer("x", occupiers_age_gender)
  }
  
  shinyApp(ui, server)
}

# Uncomment the line below to run the testing module
 doubleBarChart_demo()
