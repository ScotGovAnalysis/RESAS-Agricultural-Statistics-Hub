# File: module_fruit.R

fruitUI <- function(id) {
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
          choices = unique(fruit_subregion$`Land use by category`)
        )
      ),     
      conditionalPanel(
        condition = "input.tabsetPanel === 'Time Series' || input.tabsetPanel === 'Area Chart'",
        ns = ns,
        selectizeInput(
          ns("timeseries_variables"),
          "Click within the box to select variables",
          choices = unique(fruit_data$`Vegetables and fruits for human consumption`),
          selected = c(
            "Strawberries grown in open/under cover",
            "Raspberries grown in open/under cover",
            "Blackcurrants grown in open/under cover",
            "Blueberries grown in open/under cover",
            "Tomatoes grown in open/under cover",
            "Other fruit grown in open/under cover"
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
                 downloadButton(ns("download_data"), "Download Data"),
                 generateCensusTableFooter()

        )
      )
    )
  )
}

fruitServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    fruit_map <- fruit_subregion %>%
      select(-`Scotland total`) %>%
      mutate(across(everything(), as.character)) %>%
      pivot_longer(cols = -`Land use by category`, names_to = "sub_region", values_to = "value") %>%
      mutate(value = as.numeric(value))
    
    mapServer(
      id = "map",
      data = reactive({
        req(input$variable)
        fruit_map %>% filter(`Land use by category` == input$variable)
      }),
      unit = "hectares",
      footer = census_footer,
      variable = reactive(input$variable),
      title = paste("Fruit distribution by region in Scotland in", census_year),
      legend_title = "Area (hectares)"
    )
    
    chart_data <- reactive({
      req(input$timeseries_variables)
      filtered_data <- fruit_data %>%
        filter(`Vegetables and fruits for human consumption` %in% input$timeseries_variables) %>%
        pivot_longer(cols = -`Vegetables and fruits for human consumption`, names_to = "year", values_to = "value") %>%
        mutate(year = as.numeric(year))  # Ensure year is numeric
      filtered_data
    })
    
    areaChartServer(
      id = "area",
      chart_data = chart_data,
      title = "Area used to grow fruit across time",
      yAxisTitle = "Area of fruit (hectares)",
      xAxisTitle = "Year",
      unit = "hectares",
      footer = census_footer,
      x_col = "year",
      y_col = "value"
    )
    
    lineChartServer(
      id = "line",
      chart_data = chart_data,
      title = "Area used to grow fruit across time",
      yAxisTitle = "Area of fruit (hectares)",
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
        fruit_map %>%
          filter(`Land use by category` == input$variable) %>%
          pivot_wider(names_from = sub_region, values_from = value) %>%
          mutate(across(where(is.numeric) & !contains("Year"), comma)) %>%
          datatable(
            options = list(
              scrollX = TRUE,  # Enable horizontal scrolling
              pageLength = 20  # Show 20 entries by default
            )
          )
      } else {
        fruit_data %>%
          pivot_longer(cols = -`Vegetables and fruits for human consumption`, names_to = "year", values_to = "value") %>%
          pivot_wider(names_from = year, values_from = value) %>%
          mutate(across(where(is.numeric) & !contains("Year"), comma)) %>%
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
          paste("Fruit_Subregion_Data_", Sys.Date(), ".csv", sep = "")
        } else {
          paste("Fruit_Timeseries_Data_", Sys.Date(), ".csv", sep = "")
        }
      },
      content = function(file) {
        data <- if (input$table_data == "map") {
          fruit_map %>%
            filter(`Land use by category` == input$variable) %>%
            pivot_wider(names_from = sub_region, values_from = value)
        } else {
          fruit_data %>%
            pivot_longer(cols = -`Vegetables and fruits for human consumption`, names_to = "year", values_to = "value") %>%
            pivot_wider(names_from = year, values_from = value)
        }
        write.csv(data, file, row.names = FALSE)
      }
    )
  })
}

fruit_demo <- function() {
  ui <- fluidPage(fruitUI("fruit_test"))
  server <- function(input, output, session) {
    fruitServer("fruit_test")
  }
  shinyApp(ui, server)
}
fruit_demo()