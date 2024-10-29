# File: module_stockfeeding.R

stockfeedingUI <- function(id) {
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
          choices = unique(stockfeeding_subregion$`Land use by category`)
        )
      ),     
      conditionalPanel(
        condition = "input.tabsetPanel === 'Time Series' || input.tabsetPanel === 'Area Chart'",
        ns = ns,
        checkboxGroupInput(
          ns("timeseries_variables"),
          "Select Time Series Variables",
          choices = unique(stockfeeding_data$`Crop/Land use`),
          selected = c(
            "Turnips/swedes",
            "Kale/cabbage",
            "Maize",
            "Rape",
            "Fodder beet",
            "Lupins"
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
        tabPanel("Time Series", lineChartUI(ns("line"))),
        tabPanel("Area Chart", areaChartUI(ns("area"))),
        tabPanel("Data Table", 
                 DTOutput(ns("table")),
                 downloadButton(ns("download_data"), "Download Data"),
                 generateCensusTableFooter()

        )
      )
    )
  )
}

stockfeedingServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    stockfeeding_map <- stockfeeding_subregion %>%
      select(-`Scotland total`) %>%
      mutate(across(everything(), as.character)) %>%
      pivot_longer(cols = -`Land use by category`, names_to = "sub_region", values_to = "value") %>%
      mutate(value = safe_as_numeric(value))
    
    mapServer(
      id = "map",
      data = reactive({
        req(input$variable)
        stockfeeding_map %>% filter(`Land use by category` == input$variable)
      }),
      unit = "hectares",
      footer = census_footer,
      variable = reactive(input$variable),
      title = paste("Stockfeeding crops distribution by region in Scotland in", census_year),
      legend_title = "Area (hectares)"
    )
    
    chart_data <- reactive({
      req(input$timeseries_variables)
      filtered_data <- stockfeeding_data %>%
        filter(`Crop/Land use` %in% input$timeseries_variables) %>%
        pivot_longer(cols = -`Crop/Land use`, names_to = "year", values_to = "value") %>%
        mutate(year = as.numeric(year))  # Ensure year is numeric
      filtered_data
    })
    
    areaChartServer(
      id = "area",
      chart_data = chart_data,
      title = "Area used to grow crops for stockfeeding over time",
      yAxisTitle = "Area of crops (1,000 hectares)",
      xAxisTitle = "Year",
      unit = "hectares",
      footer = census_footer,
      x_col = "year",
      y_col = "value"
    )
    
    lineChartServer(
      id = "line",
      chart_data = chart_data,
      title = "Area used to grow crops for stockfeeding over time",
      yAxisTitle = "Area of crops (1,000 hectares)",
      xAxisTitle = "Year",
      unit = "hectares",
      footer = census_footer,
      x_col = "year",
      y_col = "value"
    )
    
    output$table <- renderDT({
      req(input$tabsetPanel == "Data Table")
      if (input$table_data == "map") {
        req(input$variable)
        stockfeeding_map %>%
          filter(`Land use by category` == input$variable) %>%
          pivot_wider(names_from = sub_region, values_from = value) %>%
          datatable(
            options = list(
              scrollX = TRUE,  # Enable horizontal scrolling
              pageLength = 20  # Show 20 entries by default
            )
          )
      } else {
        stockfeeding_data %>%
          pivot_longer(cols = -`Crop/Land use`, names_to = "year", values_to = "value") %>%
          pivot_wider(names_from = year, values_from = value) %>%
          datatable(
            options = list(
              scrollX = TRUE,  # Enable horizontal scrolling
              pageLength = 20  # Show 20 entries by default
            )
          )
      }
    })
    
    output$download_data <- downloadHandler(
      filename = function() {
        if (input$table_data == "map") {
          paste("Stockfeeding_Subregion_Data_", Sys.Date(), ".csv", sep = "")
        } else {
          paste("Stockfeeding_Timeseries_Data_", Sys.Date(), ".csv", sep = "")
        }
      },
      content = function(file) {
        data <- if (input$table_data == "map") {
          stockfeeding_map %>%
            filter(`Land use by category` == input$variable) %>%
            pivot_wider(names_from = sub_region, values_from = value) %>%
            mutate(across(where(is.numeric) & !contains("Year"), comma))
        } else {
          stockfeeding_data %>%
            pivot_longer(cols = -`Crop/Land use`, names_to = "year", values_to = "value") %>%
            pivot_wider(names_from = year, values_from = value) %>%
            mutate(across(where(is.numeric) & !contains("Year"), comma))
        }
        write.csv(data, file, row.names = FALSE)
      }
    )
  })
}

stockfeeding_demo <- function() {
  ui <- fluidPage(stockfeedingUI("stockfeeding_test"))
  server <- function(input, output, session) {
    stockfeedingServer("stockfeeding_test")
  }
  shinyApp(ui, server)
}
stockfeeding_demo()