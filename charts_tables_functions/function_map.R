
# Load the GeoJSON file
geojson_data <- geojson_read("utility/subregions_simplified.geojson", what = "sp")

# Convert GeoJSON to a Highcharts-compatible format
geojson_list <- geojson_list(geojson_data)

mapUI <- function(id) {
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
          <li>Where areas are shaded in grey, the data has been suppressed to prevent disclosure of individual holdings.</li>
            <li>To change the data shown, select a variable from the radio buttons within the sidebar.</li>
            <li>You can see data values for each variable by hovering your mouse over the region.</li>
            <li>To change the zoom level, use the + and - to the left of the graph, or scroll using your mouse wheel.</li>
          </ul>"
        )
      )
    )
  )
}


mapServer <- function(id, data, variable, unit = "", title, footer, legend_title = "Legend") {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    output$title <- renderUI({
      HTML(paste0("<div style='font-size: 20px; font-weight: bold;'>", title, "</div>"))
    })
    
    output$footer <- renderUI({
      HTML(footer)
    })
    
    filtered_data <- reactive({
      req(variable)  # Ensure that variable is not null or missing
      req(variable())  # Ensure that variable() is not null or missing
      req(data())      # Ensure that data() is not null or missing
      
      first_col_name <- names(data())[1]  # Get the name of the first column dynamically
      data() %>%
        filter(!!sym(first_col_name) == variable())  # Use the first column name for filtering
    })
    
    output$map <- renderHighchart({
      data <- filtered_data()
      hc_data <- data %>% 
        mutate(sub_region = as.character(sub_region)) %>%
        select(sub_region, value) %>%
        list_parse()
      
      variable_name <- variable()  # Get the selected variable name
      
      highchart(type = "map") %>%
        hc_add_series(
          mapData = geojson_list, 
          joinBy = c("sub_region", "sub_region"),
          data = hc_data,
          borderColor = "#FFFFFF",
          borderWidth = 0.5,
          states = list(
            hover = list(
              color = "#BADA55"
            )
          ),
          dataLabels = list(
            enabled = FALSE  # Disable the overlays
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
          return '<b>' + this.sub_region + '</b><br/>' +
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
            format = "{value:,.0f}"))  %>%  
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
