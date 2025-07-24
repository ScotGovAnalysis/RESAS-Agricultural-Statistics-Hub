percentageBarChartUI <- function(id, chart_height = 300) {
  ns <- NS(id)
  tagList(
    htmlOutput(ns("title")),
    highchartOutput(ns("bar_chart"), height = paste0(chart_height, "px"))
  )
}
percentageBarChartServer <- function(id, chart_data, title, yAxisTitle, xAxisTitle, x_col, y_col, unit = "") {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    output$bar_chart <- renderHighchart({
      data <- chart_data()
      if (nrow(data) == 0) return(NULL)
      
      # Ensure consistent handling of single-row data frames
      data <- data %>%
        mutate(Remaining = 100 - !!sym(y_col)) %>%
        select(!!sym(x_col), Percentage = !!sym(y_col), Remaining)
      
      # Ensure the x-axis categories remain a vector and not a simplified structure
      categories <- data[[x_col]]
      if (is.factor(categories)) {
        categories <- as.character(categories)
      }
      if (length(categories) == 1) {
        categories <- list(categories)  # Force single element into a list to maintain structure
      }
      
      highchart() %>%
        hc_chart(type = "bar", inverted = TRUE, zoomType = "xy") %>%
        hc_xAxis(categories = categories, title = list(text = xAxisTitle),
                 labels = list(
                   style = list(width = "250px"),  # Fixed width for labels
                   useHTML = TRUE
                 )) %>%
        hc_yAxis(title = list(text = yAxisTitle), max = 100) %>%
        hc_plotOptions(bar = list(
          stacking = "normal",
          dataLabels = list(enabled = FALSE),
          groupPadding = 0,  # Minimum space between groups
          pointPadding = 0.1,  # Controls space between bars in a group
          borderWidth = 0,
          pointWidth = 90  # Explicitly set the bar width
        )) %>%
        hc_add_series(
          name = "Remaining",
          data = data$Remaining,
          color = "#d3d3d3",
          showInLegend = FALSE  # Hide the series from the legend
        ) %>%
        hc_add_series(
          name = "Percentage",
          data = data$Percentage,
          color = "#2b9c93",
          showInLegend = FALSE  # Hide the series from the legend
        ) %>%
        hc_tooltip(
          useHTML = TRUE,
          headerFormat = "<b>{point.key}</b><br/>",
          pointFormatter = JS(sprintf("function() {
            var value = this.y;
            var formattedValue = value.toLocaleString(undefined, {minimumFractionDigits: 1, maximumFractionDigits: 1});
            return this.series.name + ': ' + formattedValue + ' %s';
          }", unit))
        ) %>%
        hc_add_theme(thm)
    })
  })
}
