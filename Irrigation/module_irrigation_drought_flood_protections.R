irrigationfloodUI <- function(id) {
  ns <- NS(id)
  sidebarLayout(
    sidebarPanel(
      width = 3,
      conditionalPanel(
        condition = "input.tabsetPanel === 'Bar Chart'",
        ns = ns,
        checkboxGroupInput(
          ns("variables"), 
          "Select Variable", 
          choices = unique(irrigation_drought_flood_protection$`Irrigation, drought or flood protection measure`)
        )
      ),
      conditionalPanel(
        condition = "input.tabsetPanel === 'Data Table'",
        ns = ns,
        radioButtons(
          ns("table_data"),
          "Select Data to Display",
          choices = c("Bar Chart Data" = "chart_data"),
          selected = "chart_data"
        )
      )
    ),
    mainPanel(
      id = ns("mainpanel"),
      width = 9,
      tabsetPanel(
        id = ns("tabsetPanel"),
        tabPanel("Bar Chart", barChartUI(ns("bar_chart"))),
        tabPanel("Data Table", 
                 DTOutput(ns("data_table")),
                 tags$div(
                   style = "margin-top: 20px;",
                   downloadButton(ns("download_data"), "Download Data"),
                   generateCensusTableFooter()
                 )
        )
      )
    )
  )
  
}

irrigationfloodServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    
    chart_data <- reactive({
      irrigation_drought_flood_protection %>%
        rename(Variable = `Irrigation, drought or flood protection measure`, Value = `Responses`) %>%
        filter(Variable %in% input$variables)
    })
    
    
    barChartServer(
      id = "bar_chart",
      chart_data = chart_data,
      title = paste("Irrigation, drought or flood protection measure", census_year),
      yAxisTitle = "Number of responses",
      xAxisTitle = "",
      unit = "",
      footer = census_footer,
      x_col = "Variable",
      y_col = "Value",
      tooltip_unit= reactive("")
    )
    
    # Data Table with Scrollable X-Axis
    output$data_table <- renderDT({
      datatable(
        chart_data()  %>%
          mutate(across(where(is.numeric))),
        options = list(
          scrollX = TRUE,
          pageLength = 20
        )
      )
    })
    
    # Download Handler
    output$download_data <- downloadHandler(
      filename = function() {
        paste("irrigation_drought_flood_protection", Sys.Date(), ".csv", sep = "")
      },
      content = function(file) {
        write.csv(chart_data(), file, row.names = FALSE)
      }
    )
  })
}

content_demo <- function() {
  ui <- fluidPage(irrigationfloodUI("irrigation_flood_test"))
  server <- function(input, output, session) {
    irrigationfloodServer("irrigation_flood_test")
  }
  shinyApp(ui, server)
}
content_demo()
