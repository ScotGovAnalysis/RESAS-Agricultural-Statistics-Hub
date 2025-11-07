irrigationmethodsUI <- function(id) {
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
          choices = unique(irrigation_methods$`Irrigation practice`),
          selected = c("Drip", "Sprinkler", "Surface/Flood", "Furrow", "Micro-irrigation", "Other")
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
                   downloadButton(ns("download_data"), "Download Data")
                   )
                 )
        ),
      generate2025ModuleTableFooter()  # Place the footer outside the tabsetPanel, but still inside the mainPanel
    )
  )
}

irrigationmethodsServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    

chart_data <- reactive({
  data <- irrigation_methods %>%
    rename(Variable = `Irrigation practice`, Value = `Responses`)
  
  if (!is.null(input$variables) && length(input$variables) > 0) {
    data <- data %>% filter(Variable %in% input$variables)
  }
  
  data
})

    
    barChartServer(
      id = "bar_chart",
      chart_data = chart_data,
      title = paste("Irrigation practices used in 2025"),
      yAxisTitle = "Number of responses",
      xAxisTitle = "",
      unit = "",
      footer = NULL,
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
        paste("irrigation_methods", Sys.Date(), ".csv", sep = "")
      },
      content = function(file) {
        write.csv(chart_data(), file, row.names = FALSE)
      }
    )
  })
}
    
content_demo <- function() {
  ui <- fluidPage(irrigationmethodsUI("irrigation_methods_test"))
  server <- function(input, output, session) {
    irrigationmethodsServer("irrigation_methods_test")
  }
  shinyApp(ui, server)
}
content_demo()
