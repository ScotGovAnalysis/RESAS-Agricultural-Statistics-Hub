# File: test_function_bar_chart.R

multibarChartUI <- function(id) {
  ns <- NS(id)
  tagList(
    htmlOutput(ns("title")),
    highchartOutput(ns("bar_chart")),
    htmlOutput(ns("footer")),
    div(
      class = "note",
      style = "margin-top: 20px; padding: 10px; border-top: 1px solid #ddd;",
      HTML(
        "<strong>Note:</strong><ul>
        <li>To add or remove a series from the chart, select/deselect the variable from the sidebar menu.</li>
          <li>You can see data values for a specific variable by hovering your mouse over the bars.</li>

        </ul>"
      )
    )
  )
}


multibarChartServer <- function(id, chart_data, title, yAxisTitle, xlab, xAxisTitle, footer, x_col, y_col, unit = "", 
                           tooltip_unit,
                           maintain_order = FALSE) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    reactive_colors <- reactive({ assign_colors(chart_data(), preset_colors) })
    
    output$title <- renderUI({
      HTML(paste0("<div style='font-size: 20px; font-weight: bold;'>", title, "</div>"))
    })
    
    output$footer <- renderUI({
      HTML(footer)
    })
    
    output$bar_chart <- renderHighchart({
      data <- chart_data()
      if (nrow(data) == 0) return(NULL)
      colors <- reactive_colors()
      
      if (is.reactive(y_col)) {
        y_col_value <- y_col()
      } else {
        y_col_value <- y_col
      }
      
      if (!maintain_order && length(y_col_value) == 1) {
        data <- data %>%
          arrange(desc(.data[[y_col_value]]))
      }
      
      # Unique x-axis categories are the y-variable names
      measures <- unique(data$Measure)
      years <- unique(data$year)
      
      if (length(measures) == 1) {
        measures <- list(measures)  # wrap in list to prevent char splitting
      }
      hc <- highchart() %>%
        hc_chart(type = "bar", inverted = TRUE, zoomType = "xy") %>%
        hc_xAxis(categories = measures, title = list(text = xAxisTitle))%>%
        hc_yAxis(title = FALSE) |>  #list(text = if (is.reactive(yAxisTitle)) yAxisTitle() else yAxisTitle),
               #  allowDecimals = FALSE) %>%
        hc_plotOptions(bar = list(
          dataLabels = list(enabled = FALSE),
          groupPadding = 0.1,
          pointPadding = 0.2,
          borderWidth = 0
        )) %>%
        hc_tooltip(
          useHTML = TRUE,
          headerFormat = "<b>{point.key}</b><br/>",
          pointFormatter = JS(sprintf(
            "function() {
          var value = this.y;
          var formattedValue;
          if (value >= 1000) {
            formattedValue = value.toLocaleString(undefined, {minimumFractionDigits: 0, maximumFractionDigits: 0});
          } else {
            formattedValue = value.toLocaleString(undefined, {minimumFractionDigits: 1, maximumFractionDigits: 2});
          }
          return this.series.name + ': £'  + formattedValue + ' %s';
        }", tooltip_unit()))
        )
      
      # Add one series per year
      for (yr in years) {
        yr_data <- data %>% filter(year == yr)
        hc <- hc %>%
          hc_add_series(
            name = as.character(yr),
            data = list_parse2(data.frame(
              name = yr_data$Measure,
              y = yr_data$value,
              color = colors[yr_data$Measure]
            )),
            showInLegend = TRUE,
            colorByPoint = FALSE
          )
      }
      
      hc
    })
  })
}

