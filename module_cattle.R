# File: module_cattle.R
cattleUI <- function(id) {
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
            "Total Cattle" = "Total Cattle",
            "Total Female Dairy Cattle" = "Total Female Dairy Cattle",
            "Total Female Beef Cattle" = "Total Female Beef Cattle",
            "Total Male Cattle" = "Total Male Cattle",
            "Total Calves" = "Total Calves"
          )
        )
      ),
      conditionalPanel(
        condition = "input.tabsetPanel === 'Time Series' || input.tabsetPanel === 'Area Chart'",
        ns = ns,
        selectizeInput(
          ns("timeseries_variables"),
          "Click within the box to select variables (not all shown)",
          choices = unique(number_of_cattle$`Cattle by category`),
          selected = c(
            "Total Female Dairy Cattle",
            "Total Female Beef Cattle",
            "Total Male Cattle",
            "Total Calves"
          ),
          multiple = TRUE,
          options = list(
            plugins = list('remove_button')
          )
        )
      ),
      conditionalPanel(
        condition = "input.tabsetPanel === 'Data Table'",
        ns = ns,
        radioButtons(
          ns("table_data"),
          "Select Data to Display",
          choices = c("Map Data" = "map", "Time Series Data" = "timeseries"),
          selected = "map"
        )
      )
    ),
    mainPanel(
      width = 9,
      tabsetPanel(
        id = ns("tabsetPanel"),
        tabPanel("Map", mapUI(ns("map"))),
        tabPanel("Time Series", lineChartUI(ns("line"), note_type = 2)),
        tabPanel("Area Chart", areaChartUI(ns("area"), note_type = 2)),
        tabPanel("Data Table", 
                 DTOutput(ns("table")),
                 downloadButton(ns("downloadData"), "Download Data"),
                 generateCensusTableFooter()
        )
      )
    )
  )
}


cattleServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Processing data for Map
    cattle_data <- livestock_subregion %>%
      filter(`Livestock by category` %in% c(
        "Total Female Dairy Cattle",
        "Total Female Beef Cattle",
        "Total Male Cattle",
        "Total Calves",
        "Total Cattle"
      )) %>%
      select(-Scotland) %>%
      mutate(across(everything(), as.character)) %>%
      pivot_longer(cols = -`Livestock by category`, names_to = "sub_region", values_to = "value") %>%
      mutate(value = safe_as_numeric(value))
    
    # Map Server
    mapServer(
      id = "map",
      data = reactive({
        req(input$variable)
        cattle_data %>% filter(`Livestock by category` == input$variable)
      }),
      footer = census_footer,
      variable = reactive(input$variable),
      title = paste("Cattle distribution by region in Scotland in", census_year),
      legend_title = "Number of cattle"
    )
    
    # Processing data for Area Chart and Time Series
    chart_data <- reactive({
      req(input$timeseries_variables)
      filtered_data <- number_of_cattle %>%
        filter(`Cattle by category` %in% input$timeseries_variables) %>%
        pivot_longer(cols = -`Cattle by category`, names_to = "year", values_to = "value") %>%
        mutate(year = as.numeric(year))  # Ensure year is numeric
      filtered_data
    })
    
    # Area Chart Server
    areaChartServer(
      id = "area",
      chart_data = chart_data,
      title = "Number of cattle by category across time",
      yAxisTitle = "Number of cattle (1,000)",
      xAxisTitle = "Year",
      footer = census_footer,
      x_col = "year",
      y_col = "value"
    )
    
    # Line Chart Server
    lineChartServer(
      id = "line",
      chart_data = chart_data,
      title = "Number of cattle by category across time",
      yAxisTitle = "Number of cattle (1,000)",
      xAxisTitle = "Year",
      footer = census_footer,
      x_col = "year",
      y_col = "value"
    )
    
    # Data Table output
    output$table <- renderDT({
      req(input$tabsetPanel == "Data Table")
      data <- if (input$table_data == "map") {
        cattle_data %>%
          filter(`Livestock by category` == input$variable) %>%
          pivot_wider(names_from = sub_region, values_from = value) %>%
          mutate(across(where(is.numeric) & !contains("Year"), comma)) # Pivot wider for map data
      } else {
        number_of_cattle %>%
          pivot_longer(cols = -`Cattle by category`, names_to = "year", values_to = "value") %>%
          pivot_wider(names_from = year, values_from = value) %>%
          mutate(across(where(is.numeric) & !contains("Year"), comma)) # Pivot wider for time series data
      }
      datatable(
        data,
        options = list(
          scrollX = TRUE,  # Enable horizontal scrolling
          pageLength = 20  # Show 20 entries by default
        )
      )
    })
    
    # Data Download Handler
    output$downloadData <- downloadHandler(
      filename = function() {
        if (input$table_data == "map") {
          paste("Cattle_Map_Data_", Sys.Date(), ".csv", sep = "")
        } else {
          paste("Cattle_Timeseries_Data_", Sys.Date(), ".csv", sep = "")
        }
      },
      content = function(file) {
        data <- if (input$table_data == "map") {
          cattle_data %>%
            filter(`Livestock by category` == input$variable) %>%
            pivot_wider(names_from = sub_region, values_from = value)
        } else {
          number_of_cattle %>%
            pivot_longer(cols = -`Cattle by category`, names_to = "year", values_to = "value") %>%
            pivot_wider(names_from = year, values_from = value)
        }
        write.csv(data, file, row.names = FALSE)
      }
    )
  })
}


cattle_demo <- function() {
  ui <- fluidPage(cattleUI("cattle_test"))
  server <- function(input, output, session) {
    cattleServer("cattle_test")
  }
  shinyApp(ui, server)
}

cattle_demo()
