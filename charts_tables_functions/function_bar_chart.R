# File: function_bar_chart.R

barChartUI <- function(id) {
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

# File: module_bar_chart.R

barChartServer <- function(id, chart_data, title, yAxisTitle, xAxisTitle, footer, x_col, y_col, unit = "", 
                           tooltip_unit, #unit_position = "",
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
      group_column <- setdiff(names(data), c(x_col, if (is.reactive(y_col)) y_col() else y_col))[1]
      
      if (is.reactive(y_col)) {
        y_col_value <- y_col()
      } else {
        y_col_value <- y_col
      }
      
      if (!maintain_order) {
        data <- data %>%
          arrange(desc(!!sym(y_col_value)))  # Sort data by value in descending order
      }
      
      highchart() %>%
        hc_chart(type = "bar", inverted = TRUE, zoomType = "xy") %>%
        hc_xAxis(categories = data[[x_col]], title = list(text = xAxisTitle)) %>%
        hc_yAxis(title = list(text = if (is.reactive(yAxisTitle)) yAxisTitle() else yAxisTitle), allowDecimals = FALSE) %>%
        hc_plotOptions(bar = list(
          dataLabels = list(enabled = FALSE),
          colorByPoint = TRUE,
          groupPadding = 0,
          pointPadding = 0.1,
          borderWidth = 0
        )) %>%
        hc_add_series(
          name = "",
          data = data %>% mutate(y = !!sym(y_col_value), color = colors[!!sym(x_col)]) %>% 
            select(name = !!sym(x_col), y, color),
          colorByPoint = TRUE,
          showInLegend = FALSE
        ) %>%
        hc_tooltip(
          useHTML = TRUE,
          headerFormat = "<b>{point.key}</b><br/>",
          pointFormatter = (
          JS(sprintf("function() {
            var value = this.y;
            var formattedValue;
            if (value >= 1000) {
              formattedValue = value.toLocaleString(undefined, {minimumFractionDigits: 0, maximumFractionDigits: 0});
            } else {
              formattedValue = value.toLocaleString(undefined, {minimumFractionDigits: 1, maximumFractionDigits: 2});
            }
            return this.series.name  + formattedValue + ' %s';
          }"
    ,tooltip_unit())
    
    #  to do: try to make position of unit responsive to reactive input variable (i.e. £ infront of value for SOs) )
     
     # hc_tooltip(
      #   useHTML = TRUE,
      #   headerFormat = "<b>{point.key}</b><br/>",
      #   pointFormatter = (
      #     JS(sprintf("function() {
      #       var value = this.y;
      #       var formattedValue;
      #       if (value >= 1000) {
      #         formattedValue = value.toLocaleString(undefined, {minimumFractionDigits: 0, maximumFractionDigits: 0});
      #       } else {
      #         formattedValue = value.toLocaleString(undefined, {minimumFractionDigits: 1, maximumFractionDigits: 2});
      #       }
      # 
      #       var unit = '%s'; // Get the unit
      #       if (%s === 'before') {
      #         return unit + ' ' + formattedValue + ' ' + this.series.name; // Unit before formatted value
      #       } else {
      #         return formattedValue + ' ' + unit + ' ' + this.series.name; // Unit after formatted value
      #       }
      #     }",
      #                tooltip_unit(), unit_position())
             
        ))) #%>%
        
       # hc_add_theme(thm)
    })
  })
}

