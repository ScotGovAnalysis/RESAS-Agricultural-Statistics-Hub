lineChartUI <- function(id, note_type = 1) {
  ns <- NS(id)
  
  note_content <- if (note_type == 2) {
    "<strong>Note:</strong><ul>
      <li>To add a series to the chart, click inside the white box on the sidebar and select a variable.</li>
      <li>To remove a series, click the x beside the variable name within the sidebar.</li>
    <li>Zoom into the graph by clicking and dragging over the area you wish to focus on.</li>
      <li>You can see data values for a specific year by hovering your mouse over the line.</li>
    </ul>"
  } else {
    "<strong>Note:</strong><ul>
      <li>To add or remove a series from the chart, select/deselect the variable from the sidebar menu.</li>
      <li>Zoom into the graph by clicking and dragging over the area you wish to focus on.</li>
      <li>You can see data values for a specific year by hovering your mouse over the line.</li>
    </ul>"
  }
  
  tagList(
    htmlOutput(ns("title")),
    highchartOutput(ns("line_chart")),
    htmlOutput(ns("footer")),
    div(
      class = "note",
      style = "margin-top: 20px; padding: 10px; border-top: 1px solid #ddd;",
      HTML(note_content)
    )
  )
}


lineChartServer <- function(id, chart_data, chart_title, yAxisTitle, xAxisTitle, unit = "", footer, x_col, y_col) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    reactive_colors <- reactive({ assign_colors(chart_data(), preset_colors) })
    
    output$chart_title <- renderUI({
      year_min <- min(as.numeric(chart_data()[[x_col]]), na.rm = TRUE)
      year_max <- max(as.numeric(chart_data()[[x_col]]), na.rm = TRUE)
      
      HTML(paste0(
        "<div style='font-size: 20px; font-weight: bold;'>",
        chart_title(),
        ", ", year_min, " to ", year_max,
        "</div>"
      ))
    })
    
   
    output$line_chart <- renderHighchart({
      data <- chart_data()
      colors <- reactive_colors()
      group_column <- setdiff(names(data), c(x_col, y_col))[1] # Assuming only one group column
      
      hc <- highchart() %>%
        hc_chart(type = "line", zoomType = "xy") %>%
        hc_yAxis(title = list(text = yAxisTitle)) %>%
        hc_xAxis(title = list(text = xAxisTitle), type = "category", tickInterval = 5) %>%
        hc_plotOptions(line = list(colorByPoint = FALSE)) %>%
        hc_legend(align = "left", alignColumns = FALSE, layout = "horizontal")
        #hc_add_theme(thm)
      
      unique_groups <- unique(data[[group_column]])
      lapply(unique_groups, function(g) {
        series_data <- data[data[[group_column]] == g, ]
        
        # Create a complete sequence of years
        complete_years <- seq(min(series_data[[x_col]], na.rm = TRUE), max(series_data[[x_col]], na.rm = TRUE))
        complete_series <- merge(data.frame(x = complete_years), series_data, by.x = "x", by.y = x_col, all.x = TRUE)
        complete_series <- complete_series %>% transmute(x = as.numeric(x), y = !!sym(y_col))
        
        hc <<- hc %>%
          hc_add_series(name = g, data = list_parse2(complete_series), color = colors[[g]])
      })
      
      hc %>%
        hc_tooltip(
          useHTML = TRUE,
          headerFormat = "<b>{point.key}</b><br/>",
          pointFormatter = JS(sprintf("function() {
            var value = this.y;
            var formattedValue;
            if (value >= 1000) {
              formattedValue = value.toLocaleString(undefined, {minimumFractionDigits: 0, maximumFractionDigits: 0});
            } else {
              formattedValue = value.toLocaleString(undefined, {minimumFractionDigits: 0, maximumFractionDigits: 2});
            }
            return this.series.name + ': ' + formattedValue + ' %s';
          }", unit))
        )
    })
  })
}
