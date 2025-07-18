

ownedLandUI <- function(id) {
  ns <- NS(id)
  sidebarLayout(
    sidebarPanel(
      width = 3,
      checkboxGroupInput(
        ns("variables"), 
        "Choose variables to add to chart", 
        choices = unique(owned_rented_land$`Area owned or rented`[-4]), 
        selected = unique(owned_rented_land$`Area owned or rented`[-4])
      )
    ),
    mainPanel(
      width = 9,
      tabsetPanel(
        id = ns("tabs"),
        tabPanel("Timeseries", 
                 lineChartUI(ns("line_chart")), 
                 value = "timeseries"),
        tabPanel("Data Table", 
                 DTOutput(ns("data_table")), 
                 downloadButton(ns("downloadData"), "Download Data"),   
                 generateCensusTableFooter(),
                 value = "data_table")
      )
    )
  )
}

ownedLandServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Data Processing for Timeseries
    owned_rented_land_filtered <- owned_rented_land %>% 
      filter(`Area owned or rented` != "Percentage of area rented")
    
    owned_rented_land_long <- owned_rented_land_filtered %>%
      pivot_longer(cols = -`Area owned or rented`, names_to = "Year", values_to = "Value") %>%
      mutate(Year = as.numeric(Year))
    
    filtered_owned_rented_land <- reactive({
      req(input$variables)
      data <- owned_rented_land_long %>%
        filter(`Area owned or rented` %in% input$variables)
      data
    })
    
    # Pivot Wider for Data Table
    wide_data <- reactive({
      filtered_owned_rented_land() %>%
        pivot_wider(names_from = `Area owned or rented`, values_from = Value)
    })
    
    # Line Chart
    lineChartServer(
      id = "line_chart",
      chart_data = filtered_owned_rented_land,
      title = "Area of holdings owned and rented across time",
      yAxisTitle = "Area (1,000 hectares)",
      xAxisTitle = "Year",
      unit = "hectares",
      footer = census_footer,
      x_col = "Year",
      y_col = "Value"
    )
    
    # Data Table with Scrollable X-Axis
    output$data_table <- renderDT({
      datatable(
        wide_data()  %>%
          mutate(across(where(is.numeric) & !contains("Year"), comma)),
        options = list(
          scrollX = TRUE,
          pageLength = 20
        )
      )
    })
    
    # Download Handler
    output$downloadData <- downloadHandler(
      filename = function() {
        paste("owned_rented_land", Sys.Date(), ".csv", sep = "")
      },
      content = function(file) {
        write.csv(wide_data(), file, row.names = FALSE)
      }
    )
  })
}

# Testing module
content_demo <- function() {
  ui <- fluidPage(ownedLandUI("owned_land_test"))
  server <- function(input, output, session) {
    ownedLandServer("owned_land_test")
  }
  shinyApp(ui, server)
}

content_demo()
