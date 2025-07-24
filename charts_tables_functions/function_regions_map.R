# File: module_regions_map.R

library(shiny)
library(highcharter)
library(geojsonio)
library(dplyr)

# Load the GeoJSON file
region_geojson_data <- geojson_read("utility/regions_simplified.geojson", what = "sp")

# Convert GeoJSON to a Highcharts-compatible format
region_geojson_list <- geojson_list(region_geojson_data)

mapRegionsUI <- function(id) {
  ns <- NS(id)
  tagList(
    mainPanel(
      width = 12,  # Ensures the main panel uses full width
      htmlOutput(ns("title")),
      highchartOutput(ns("map"), height = "75vh", width = "100%"),  # Set width to 100% for full utilization
      htmlOutput(ns("footer")),
      div(
        class = "note",
        style = "margin-top: 20px; padding: 10px; border-top: 1px solid #ddd;",
        HTML(
          "<strong>Note:</strong><ul>
            <li>To change the data shown, select a variable from the radio buttons within the sidebar.</li>
            <li>You can see data values for each variable by hovering your mouse over the region.</li>
            <li>To change the zoom level, use the + and - to the left of the graph, or scroll using your mouse wheel.</li>
          </ul>"
        )
      )
    )
  )
}

mapRegionsServer <- function(id, data, variable, unit = "", title, footer, legend_title = "Legend") {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    output$title <- renderUI({
      HTML(paste0("<div style='font-size: 20px; font-weight: bold;'>", title, "</div>"))
    })
    
    output$footer <- renderUI({
      HTML(footer)
    })
    
    filtered_data <- reactive({
      req(variable)
      req(variable())
      req(data())
      
      filtered <- data() %>%
        select(region, value) %>%
        mutate(region = as.character(region))
      
      filtered
    })
    
    output$map <- renderHighchart({
      data <- filtered_data()
      
      hc_data <- data %>% 
        list_parse()
      
      variable_name <- variable()
      
      highchart(type = "map") %>%
        hc_add_series(
          mapData = region_geojson_list, 
          joinBy = c("region", "region"),
          data = hc_data,
          borderColor = "#FFFFFF",
          borderWidth = 0.5,
          states = list(
            hover = list(
              color = "#BADA55"
            )
          ),
          dataLabels = list(
            enabled = FALSE
          ),
          tooltip = list(
            useHTML = TRUE,
            headerFormat = "<b>{point.key}</b><br/>",
            pointFormatter = JS(sprintf("function() {
              var value = this.value;
              var formattedValue;
              if (value >= 1000) {
                formattedValue = value.toLocaleString(undefined, {minimumFractionDigits: 0, maximumFractionDigits: 0});
              } else {
                formattedValue = value.toLocaleString(undefined, {minimumFractionDigits: 0, maximumFractionDigits: 2});
              }
              return '<b>' + this.region + '</b><br/>' +
                     '%s: ' + formattedValue + ' %s';
            }", variable_name, unit))
          ),
          nullColor = '#E0E0E0'  # Color for regions with no data
        ) %>%
        hc_mapNavigation(enabled = TRUE) %>%
        hc_colorAxis(
          min = 0,
          stops = color_stops(5),
          labels = list(
            format = "{value:,.0f}"  # Ensure the labels show the correct values
          )
        ) %>%
        hc_chart(reflow = TRUE) %>% # Make chart responsive
        hc_legend(
          layout = "vertical",            # Change layout to vertical
          align = "right",                # Align the legend to the right
          verticalAlign = "middle",       # Align the legend to the middle vertically
          x = -10,                        # Move the legend left by 10px for right padding
          title = list(text = legend_title, style = list(fontSize = '15px')),  # Use the provided or default title
          itemStyle = list(
            width = '300px',              # Adjust width as needed
            padding = "0 10px 0 0"        # Add padding-right of 10px
          ),
          symbolHeight = 300,             # Adjust the height of the color bar
          itemMarginTop = 10              # Adjust the margin at the top of each item
        )
    })
  })
}
