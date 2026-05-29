nitrogenline_ChartUI <- function(id, in_out_type = NULL) {
  ns <- NS(id)
  
  base_note <- HTML("<strong>Note:</strong><ul>
    <li>Zoom into the graph by clicking and dragging over the area you wish to focus on.</li>
    <li>You can see data values for a specific year by hovering your mouse over the line.</li>
  </ul>")
  
  tagList(
    htmlOutput(ns("title")),
    highchartOutput(ns("nitrogenline_chart")),
    htmlOutput(ns("footer")),
    div(
      class = "note",
      style = "margin-top: 20px; padding: 10px; border-top: 1px solid #ddd;",
      base_note
    )
  )
}




nitrogenline_ChartServer <- function(id, chart_data, title, yAxisTitle, xAxisTitle, unit, footer, x_col, y_col) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    reactive_colors <- reactive({ assign_colors(chart_data(), preset_colors) })
    
    output$title <- renderUI({
      HTML(paste0("<div style='font-size: 20px; font-weight: bold;'>", title, "</div>"))
    })
    
    
    
    output$footer <- renderUI({
      HTML(footer)
    })
    
    output$nitrogenline_chart <- renderHighchart({
      data <- chart_data()
      colors <- reactive_colors()
      
      group_column <- "Measure"  # assuming fixed group column
      
      # Ensure x_col is character
      data[[x_col]] <- as.character(data[[x_col]])
      
      # Create x-axis categories
      x_categories <- unique(data[[x_col]])
      x_categories <- x_categories[x_categories != ""] 

      hc <- highchart() %>%
        hc_chart(type = "line", zoomType = "xy") %>%
        hc_yAxis(title = list(text = yAxisTitle)) %>%
        hc_xAxis(
          title = list(text = xAxisTitle),
          categories = x_categories,  # predefined categories vector
          type = "category"
        ) %>%
        hc_plotOptions(line = list(colorByPoint = FALSE)) %>%
        hc_legend(align = "left", alignColumns = FALSE, layout = "horizontal")
      
      unique_groups <- unique(data[[group_column]])
      lapply(unique_groups, function(g) {
        series_data <- data[data[[group_column]] == g, ]
        
        complete_years <- nitrogen_year_levels
        
        complete_series <- merge(
          data.frame(x = complete_years),
          series_data,
          by.x = "x",
          by.y = x_col,
          all.x = TRUE
        ) %>%
          transmute(x = x, y = .data[[y_col]]) %>%
          mutate(x = as.character(x), y = as.numeric(y))
        
        if (all(is.na(complete_series$y))) {
          message(paste("Skipping group", g, "- no data"))
          return(NULL)
        }
        
        valid_points <- complete_series %>% filter(!is.na(y))
        if (nrow(valid_points) == 1) {
          padded_series <- tibble(
            x = c("", valid_points$x, ""),
            y = c(NA, valid_points$y, NA)
          )
          complete_series <- padded_series
        }
        
        print(paste("Group:", g))
        print(complete_series)
        
        color_to_use <- if (!is.null(colors[[g]])) colors[[g]] else NULL
        
        hc <<- hc %>%
          hc_add_series(name = g, data = list_parse2(complete_series), color = color_to_use)
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
          }", unit()))
        )
    })
  })
}
