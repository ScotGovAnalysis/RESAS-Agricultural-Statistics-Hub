# File: module_cereals.R

cerealsUI <- function(id) {
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
          choices = unique(cereals_subregion$`Land use by category`)
        )
      ),
      conditionalPanel(
        condition = "input.tabsetPanel === 'Time Series' || input.tabsetPanel === 'Area Chart'",
        ns = ns,
        selectizeInput(
          ns("timeseries_variables"),
          "Click within the box to select variables",
          choices = unique(cereals_data$`Crop/Land use`),
          selected = c(
            "Wheat",
            #"Triticale",
            "Barley Total",
            "Oats Total"
            #"Rye",
           # "Mixed grain"
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
      ),
      conditionalPanel(
        condition = "input.tabsetPanel === 'Cereals Summary'",
        ns = ns,
        div("Adjust the sliders to compare data from different years.", 
            style = "font-size: 14px; font-weight: bold; margin-bottom: 10px;"),
        sliderInput(ns("summary_current_year_cereals"), "Year of interest", min = 2012, max = census_year, value = census_year, step = 1, sep = ""),
        sliderInput(ns("summary_comparison_year_cereals"), "Comparison year", min = 2012, max = census_year, value = census_year-1, step = 1, sep = "")
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

        ),
        tabPanel("Summary",
                 fluidRow(
                   column(width = 6, h3("Cereals summary section")),
                   column(width = 3, selectInput(ns("summary_variable"), "Select Variable", choices = unique(cereals_data$`Crop/Land use`), selected = "Total cereals"))
                 ),
                 fluidRow(
                   column(width = 6, p("This content is under development.")),
                   column(width = 6, valueBoxUI(ns("summaryValueBox")), style = "padding-right: 0; padding-left: 0; padding-bottom: 10px;")
                 )
        )
      )
    )
  )
}

cerealsServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    cereals_map <- cereals_subregion %>%
      select(-`Scotland total`) %>%
      mutate(across(everything(), as.character)) %>%
      pivot_longer(cols = -`Land use by category`, names_to = "sub_region", values_to = "value") %>%
      mutate(value = as.numeric(value))
    
    mapServer(
      id = "map",
      data = reactive({
        req(input$variable)
        cereals_map %>% filter(`Land use by category` == input$variable)
      }),
      unit = "hectares",
      footer = census_footer,
      variable = reactive(input$variable),
      title = paste("Cereals distribution by region in Scotland in", census_year),
      legend_title = "Area (hectares)"
    )
    
    chart_data <- reactive({
      req(input$timeseries_variables)
      filtered_data <- cereals_data %>%
        filter(`Crop/Land use` %in% input$timeseries_variables) %>%
        pivot_longer(cols = -`Crop/Land use`, names_to = "year", values_to = "value") %>%
        mutate(year = as.numeric(year))  # Ensure year is numeric
      filtered_data
    })
    
    areaChartServer(
      id = "area",
      chart_data = chart_data,
      title = "Area used to grow cereals over time",
      yAxisTitle = "Area of cereals (1,000 hectares)",
      xAxisTitle = "Year",
      unit = "hectares",
      footer = census_footer,
      x_col = "year",
      y_col = "value"
    )
    
    lineChartServer(
      id = "line",
      chart_data = chart_data,
      title = "Area used to grow cereals over time",
      yAxisTitle = "Area of cereals (1,000 hectares)",
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
        cereals_map %>%
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
        cereals_data %>%
          pivot_longer(cols = -`Crop/Land use`, names_to = "year", values_to = "value") %>%
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
    
    output$downloadData <- downloadHandler(
      filename = function() {
        if (input$table_data == "map") {
          paste("Cereals_Map_Data_", Sys.Date(), ".csv", sep = "")
        } else {
          paste("Cereals_Timeseries_Data_", Sys.Date(), ".csv", sep = "")
        }
      },
      content = function(file) {
        data <- if (input$table_data == "map") {
          cereals_map %>%
            filter(`Land use by category` == input$variable) %>%
            pivot_wider(names_from = sub_region, values_from = value)
        } else {
          cereals_data %>%
            pivot_longer(cols = -`Crop/Land use`, names_to = "year", values_to = "value") %>%
            pivot_wider(names_from = year, values_from = value)
        }
        write.csv(data, file, row.names = FALSE)
      }
    )
    
    # Reactive expression for the selected variable and years
    summary_data <- reactive({
      cereals_data %>%
        filter(`Crop/Land use` == input$summary_variable) %>%
        pivot_longer(cols = -`Crop/Land use`, names_to = "Year", values_to = "Value") %>%
        mutate(Year = as.numeric(Year))
    })
    
    current_year <- reactive({ input$summary_current_year_cereals })
    comparison_year <- reactive({ input$summary_comparison_year_cereals })
    
    # Value box for the selected variable
    valueBoxServer("summaryValueBox", summary_data, "Crop/Land use", reactive(input$summary_variable), current_year, comparison_year, "ha")
  })
}

# Testing module
cereals_demo <- function() {
  ui <- fluidPage(cerealsUI("cereals_test"))
  server <- function(input, output, session) {
    cerealsServer("cereals_test")
  }
  shinyApp(ui, server)
}

cereals_demo()

