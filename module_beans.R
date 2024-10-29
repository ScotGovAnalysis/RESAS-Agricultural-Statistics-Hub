# File: module_beans.R

beansUI <- function(id) {
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
          choices = unique(beans_subregion$`Land use by category`)
        )
      ),     
      conditionalPanel(
        condition = "input.tabsetPanel === 'Time Series' || input.tabsetPanel === 'Area Chart'",
        ns = ns,
        checkboxGroupInput(
          ns("timeseries_variables"),
          "Select Time Series Variables",
          choices = unique(beans_data$`Crop/Land use`),
          selected = c(
            "Protein Peas",
            "Field Beans"
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

beansServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    beans_map <- beans_subregion %>%
      select(-`Scotland total`) %>%
      mutate(across(everything(), as.character)) %>%
      pivot_longer(cols = -`Land use by category`, names_to = "sub_region", values_to = "value") %>%
      mutate(value = safe_as_numeric(value))
    
    mapServer(
      id = "map",
      data = reactive({
        req(input$variable)
        beans_map %>% filter(`Land use by category` == input$variable)
      }),
      unit = "hectares",
      footer = census_footer,
      variable = reactive(input$variable),
      title = paste("Beans distribution by region in Scotland in", census_year),
      legend_title = "Area (hectares)"
    )
    
    chart_data <- reactive({
      req(input$timeseries_variables)
      filtered_data <- beans_data %>%
        filter(`Crop/Land use` %in% input$timeseries_variables) %>%
        pivot_longer(cols = -`Crop/Land use`, names_to = "year", values_to = "value") %>%
        mutate(year = as.numeric(year))  # Ensure year is numeric
      filtered_data
    })
    
    areaChartServer(
      id = "area",
      chart_data = chart_data,
      title = paste("Area used to grow peas and/or beans
                    over time"),
      yAxisTitle = "Area of peas/beans (1,000 hectares)",
      xAxisTitle = "Year",
      unit = "hectares",
      footer = census_footer,
      x_col = "year",
      y_col = "value"
    )
    
    lineChartServer(
      id = "line",
      chart_data = chart_data,
      title = paste("Area used to grow peas and/or beans
                    over time"),
      yAxisTitle = "Area of peas/beans (1,000 hectares)",
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
        datatable(
          beans_map %>%
            filter(`Land use by category` == input$variable) %>%
            pivot_wider(names_from = sub_region, values_from = value) %>%
            mutate(across(where(is.numeric), comma)), # Apply comma formatting to numeric columns
          options = list(scrollX = TRUE) # Enable horizontal scrolling
        )
      } else {
        datatable(
          beans_data %>%
            pivot_wider(names_from = year, values_from = value) %>%
            mutate(across(where(is.numeric) & !contains("Year"), comma)), # Apply comma formatting to numeric columns except 'Year'
          options = list(scrollX = TRUE) # Enable horizontal scrolling
        )
      }
    })
 
    output$download_data <- downloadHandler(
      filename = function() {
        if (input$table_data == "map") {
          paste("Beans_Subregion_Data_", Sys.Date(), ".csv", sep = "")
        } else {
          paste("Beans_Timeseries_Data_", Sys.Date(), ".csv", sep = "")
        }
      },
      content = function(file) {
        data <- if (input$table_data == "map") {
          beans_map %>%
            filter(`Land use by category` == input$variable) %>%
            pivot_wider(names_from = sub_region, values_from = value)
        } else {
          beans_data %>%
            pivot_longer(cols = -`Crop/Land use`, names_to = "year", values_to = "value") %>%
            pivot_wider(names_from = year, values_from = value)
        }
        write.csv(data, file, row.names = FALSE)
      }
    )
  })
}

beans_demo <- function() {
  ui <- fluidPage(beansUI("beans_test"))
  server <- function(input, output, session) {
    beansServer("beans_test")
  }
  shinyApp(ui, server)
}
beans_demo()
