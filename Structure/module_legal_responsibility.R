# File: module_legal_responsibility.R

legalResponsibilityUI <- function(id) {
  ns <- NS(id)
  tagList(
    sidebarLayout(
      sidebarPanel(
        width = 3,
        radioButtons(ns("data_type"), "Data Type", choices = c("Holdings" = "holdings", "Area" = "area"), selected = "holdings"),
        checkboxGroupInput(ns("selected_variables"), "Select Variables", choices = NULL, selected = NULL)
      ),
      mainPanel(
        id = ns("mainpanel"),
        width = 9,
        tabsetPanel(
          id = ns("tabs"),
          tabPanel("Bar Chart", barChartUI(ns("bar_chart")), value = ns("bar")),
          tabPanel("Data Table",
                   DTOutput(ns("data_table")),
                   downloadButton(ns("downloadData"), "Download Data"),
                   generateCensusTableFooter(),
                   value = ns("data"))
        )
      )
    )
  )
}
legalResponsibilityServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Chart data remains unformatted for proper rendering
    chart_data <- reactive({
      data <- legal_responsibility %>%
        filter(`Legal responsibility` != "Total")
      data
    })
    
    # Table data with formatted values for better readability
    table_data <- reactive({
      chart_data() %>%
        mutate(across(where(is.numeric), comma))  # Format numeric columns with commas
    })
    
    initial_order <- reactive({
      data <- chart_data() %>%
        arrange(desc(Holdings))
      data$`Legal responsibility`
    })
    
    observe({
      choices <- unique(chart_data()$`Legal responsibility`)
      selected <- choices
      updateCheckboxGroupInput(session, "selected_variables", choices = choices, selected = selected)
    })
    
    y_col <- reactive({
      if (input$data_type == "holdings") {
        "Holdings"
      } else {
        "Area of holdings"
      }
    })
    
    yAxisTitle <- reactive({
      if (input$data_type == "holdings") {
        "Number of Holdings (1,000)"
      } else {
        "Area of Holdings (1,000 hectares)"
      }
    })
    
    
    tooltip_unit <- reactive({
      if (input$data_type == "holdings") {
        "holdings"
      } else {
        "hectares"
      }
    })

    barChartServer(
      id = "bar_chart",
      chart_data = reactive({
        data <- chart_data() %>%
          filter(`Legal responsibility` %in% input$selected_variables) %>%
          mutate(`Legal responsibility` = factor(`Legal responsibility`, levels = c(
            "Unknown", "One person", "Two or more related", "Two or more not related","An organisation"
            ))) %>%
          arrange(`Legal responsibility`)
        data
      }),
      title = paste("Legal responsibility of holdings in Scotland", census_year),
      yAxisTitle = yAxisTitle,
      xAxisTitle = "Legal responsibility",
      footer = census_footer,
      x_col = "Legal responsibility",
      y_col = y_col,
      tooltip_unit = tooltip_unit,
      maintain_order = TRUE
    )
    
    # Render the data table with formatted values
    output$data_table <- renderDT({
      datatable(
        table_data(),
        options = list(
          scrollX = TRUE,  # Enable horizontal scrolling
          pageLength = 20  # Show 20 entries by default
        )
      )
    })
    
    # Download handler with formatted values
    output$downloadData <- downloadHandler(
      filename = function() {
        paste("Legal_Responsibility_Data_", Sys.Date(), ".csv", sep = "")
      },
      content = function(file) {
        write.csv(table_data(), file, row.names = FALSE)
      }
    )
  })
}
