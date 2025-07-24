# File: module_map.R

library(shiny)
library(highcharter)
library(geojsonio)
library(dplyr)

# Load the GeoJSON file
geojson_data <- geojson_read("subregions_simplified.geojson", what = "sp")

# Convert GeoJSON to a Highcharts-compatible format
geojson_list <- geojson_list(geojson_data)

mapUI <- function(id) {
  ns <- NS(id)
  tagList(
    mainPanel(
      htmlOutput(ns("title")),
      highchartOutput(ns("map"), height = "75vh"),  # Set the height to be responsive
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

mapServer <- function(id, data, variable, unit = "", title, footer) {
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
            format = "{value:,.0f}"  # Ensure the labels show the correct values
          )
        ) %>%
        hc_chart(reflow = TRUE) %>% # Make chart responsive
        hc_legend(
          layout = "vertical",        # Change layout to vertical
          align = "right",            # Align the legend to the right
          verticalAlign = "middle",   # Align the legend to the middle vertically
          title = list(text = "Legend", style = list(fontSize = '15px')),
          itemStyle = list(
            width = '150px',          # Adjust width as needed
            style = "padding-right: 20px;"  # Add custom padding-right
          ),
          symbolHeight = 300          # Adjust the height of the color bar
        )
    })
    
    
  })
}

# File: module_other_animals.R

otherAnimalsUI <- function(id) {
  ns <- NS(id)
  sidebarLayout(
    sidebarPanel(
      width = 3,
      conditionalPanel(
        condition = "input.tabsetPanel === 'Map'",
        ns = ns,
        radioButtons(
          ns("variable"), 
          "Select Variable", 
          choices = c(
            "Goats and kids" = "Goats and kids",
            "Deer" = "Deer",
            "Horses" = "Horses",
            "Donkeys" = "Donkeys",
            "Camelids" = "Camelids",
            "Beehives" = "Beehives"
          )
        )
      ),
      conditionalPanel(
        condition = "input.tabsetPanel === 'Time Series'",
        ns = ns,
        checkboxGroupInput(
          ns("timeseries_variables"),
          "Select Time Series Variables",
          choices = c(
            "Goats",
            "Deer",
            "Horses",
            "Donkeys",
            "Camelids",
            "Beehives"
          ),
          selected = c(
            "Goats",
            "Deer",
            "Horses",
            "Donkeys",
            "Camelids",
            "Beehives"
          )
        )
      ),
      conditionalPanel(
        condition = "input.tabsetPanel === 'Data Table'",
        ns = ns,
        radioButtons(
          ns("table_data"),
          "Select Data to Display",
          choices = c("Map Data" = "map", "Chart Data" = "timeseries"),
          selected = "map"
        )
      )
    ),
    mainPanel(
      width = 9,
      tabsetPanel(
        id = ns("tabsetPanel"),
        tabPanel("Map", mapUI(ns("map"))),
        tabPanel("Time Series", lineChartUI(ns("line"))),
        tabPanel("Data Table", DTOutput(ns("table")))
      )
    )
  )
}

otherAnimalsServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    other_animals_data <- livestock_subregion %>%
      filter(`Livestock by category` %in% c(
        "Goats and kids",
        "Deer",
        "Horses",
        "Donkeys",
        "Camelids",
        "Beehives"
      )) %>%
      select(-Scotland) %>%
      mutate(across(everything(), as.character)) %>%
      pivot_longer(cols = -`Livestock by category`, names_to = "sub_region", values_to = "value") %>%
      mutate(value = safe_as_numeric(value))
    
    mapServer(
      id = "map",
      data = reactive({
        req(input$variable)
        other_animals_data %>% filter(`Livestock by category` == input$variable)
      }),
      footer = '<div style="font-size: 16px; font-weight: bold;"><a href="https://www.gov.scot/publications/results-scottish-agricultural-census-june-2023/documents/">Source: Scottish Agricultural Census: June 2023</a></div>',
      variable = reactive(input$variable),
      title = "Other Animals Distribution by Region"
    )
    
    chart_data <- reactive({
      req(input$timeseries_variables)
      filtered_data <- number_of_other_livestock %>%
        mutate(across(-`Livestock by category`, as.numeric)) %>%
        filter(`Livestock by category` %in% input$timeseries_variables) %>%
        pivot_longer(cols = -`Livestock by category`, names_to = "year", values_to = "value") %>%
        mutate(year = as.numeric(year))  # Ensure year is numeric
      filtered_data
    })
    
    
    lineChartServer(
      id = "line",
      chart_data = chart_data,
      title = "Other Animals Area Chart Data",
      yAxisTitle = "Number of Animals (1,000)",
      xAxisTitle = "Year",
      footer = '<div style="font-size: 16px; font-weight: bold;"><a href="https://www.gov.scot/publications/results-scottish-agricultural-census-june-2023/documents/">Source: Scottish Agricultural Census: June 2023</a></div>',
      x_col = "year",
      y_col = "value"
    )
    
    output$table <- renderDT({
      req(input$tabsetPanel == "Data Table")
      if (input$table_data == "map") {
        req(input$variable)
        other_animals_data %>%
          filter(`Livestock by category` == input$variable) %>%
          datatable()
      } else {
        number_of_other_livestock %>%
          pivot_longer(cols = -`Cattle by category`, names_to = "year", values_to = "value") %>%
          datatable()
      }
    })
  })
}

# Testing module
other_animals_demo <- function() {
  ui <- fluidPage(otherAnimalsUI("other_animals_test"))
  server <- function(input, output, session) {
    otherAnimalsServer("other_animals_test")
  }
  shinyApp(ui, server)
}

other_animals_demo()
