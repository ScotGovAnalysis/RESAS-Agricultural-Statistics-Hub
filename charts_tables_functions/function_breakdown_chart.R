breakdownChartUI <- function(id) {
  ns <- NS(id)
  tagList(
    htmlOutput(ns("title")),
    highchartOutput(ns("breakdown_chart")),
    htmlOutput(ns("footer")),
    div(
      class = "note",
      style = "margin-top: 20px; padding: 10px; border-top: 1px solid #ddd;",
      HTML(
        "<strong>Note:</strong><ul>
          <li>This chart shows the breakdown of emissions by subsector.</li>
          <li>Use the filters on the sidebar to refine the data displayed.</li>
          <li>Hover over the bars to see detailed information for each subsector.</li>
        </ul>"
      )
    )
  )
}

breakdownChartServer <- function(id, chart_data, title, yAxisTitle, xAxisTitle, unit = "", footer) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    output$title <- renderUI({
      HTML(paste0("<div style='font-size: 20px; font-weight: bold;'>", title, "</div>"))
    })
    
    output$footer <- renderUI({
      HTML(footer)
    })
    
    output$breakdown_chart <- renderHighchart({
      data_long <- chart_data() %>%
        pivot_longer(cols = -Source, names_to = "Subsector", values_to = "Value") %>%
        arrange(match(Source, rev(unique(Source))))
      breakdown_colors = c("#2b9c93", "#002d54", "#6a2063", "#e5682a", "#0b4c0b", "#5d9f3c")
      
      unique_sources <- unique(data_long$Source)
      color_map <- setNames(rev(breakdown_colors)[1:6], unique_sources)
      
      series_data <- lapply(unique_sources, function(source) {
        list(
          name = source,
          data = data_long %>%
            filter(Source == source) %>%
            arrange(match(Subsector, unique(data_long$Subsector))) %>%
            pull(Value),
          color = color_map[[source]]
        )
      })
      
      highchart() %>%
        hc_chart(type = "bar", zoomType = "xy") %>%
        hc_xAxis(categories = unique(data_long$Subsector), 
                 labels = list(style = list(color = "#000000", fontSize = '16px', fontFamily = 'Arial'))) %>%
        hc_yAxis(title = list(text = yAxisTitle, style = list(color = "#000000", fontSize = '16px', fontFamily = 'Arial')),
                 labels = list(style = list(color = "#000000", fontSize = '16px', fontFamily = 'Arial')),
                 tickInterval = 0.5) %>%
        hc_plotOptions(bar = list(
          stacking = "normal",
          groupPadding = 0,
          pointPadding = 0.1,
          borderWidth = 0
        )) %>%
        hc_tooltip(
          useHTML = TRUE,
          headerFormat = "<span style='font-size: 16px; font-family: Arial'><b>{point.key}</b></span><br/>",
          pointFormatter = JS("function() {
            var value = this.y;
            var formattedValue;
            if (value >= 1000) {
              formattedValue = value.toLocaleString(undefined, {minimumFractionDigits: 0, maximumFractionDigits: 0});
            } else {
              formattedValue = value.toLocaleString(undefined, {minimumFractionDigits: 1, maximumFractionDigits: 2});
            }
            return this.series.name + ': ' + formattedValue + ' MtCO₂e';
          }"),
          style = list(fontSize = "16px", fontFamily = "Arial")
        ) %>%
        hc_legend(
          align = "right", 
          verticalAlign = "middle", 
          layout = "vertical",
          itemStyle = list(fontSize = '16px', fontFamily = 'Arial', fontWeight = 'normal')
        ) %>%
        hc_add_series_list(series_data)
    })
  })
}


