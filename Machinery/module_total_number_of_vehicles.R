totalnumberofvehiclesUI <- function(id) {
  ns <- NS(id)
  tagList(
    sidebarLayout(
      sidebarPanel(
        width = 3,
        conditionalPanel(
          condition = paste0("input['", ns("tabs"), "'] == '", ns("bar"), "'"),  # Show variable select only on Bar Chart tab
          uiOutput(ns("variable_select"))  # Variable select UI
        )
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
                   value = ns("data"))
        ),
        generate2024ModuleTableFooter()
      )
    )
  )
}

totalnumberofvehiclesServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Create reactive data excluding 'All' and depending only on data_type (for chart)
    filtered_data_chart <- reactive({
      total_number_vehicles_data %>%
        filter(`Agricultural machinery` != "All agricultural machinery")
    })
    
    # Create reactive data for the table with commas added to numeric values
    filtered_data_table <- reactive({
      filtered_data_chart() %>%
        mutate(across(where(is.numeric), comma))
    })
    
    # Get filtered data based on selected variables (only for bar chart)
    chart_data <- reactive({
      req(input$variables)
      data <- filtered_data_chart()
        if (input$tabs == ns("bar") && !is.null(input$variables)) {
        data <- data %>%
        filter(`Agricultural machinery` %in% input$variables)
      }
       data
    })
    
    # Select the appropriate column based on data_type
    # y_col <- reactive({
    #   switch(input$tabs,
    #          "All fuel types" = "Total number of vehicles"
    #   )
    #          })
    
    y_col <- "All fuel types"
    
    # yAxisTitle <- reactive({
    #   switch(input$tabs,
    #          "All fuel types" = "Total number of vehicles")
    # })
    
    yAxisTitle <- "Number of vehicles (1,000)"
    
    tooltip_unit <- reactive({
      switch(input$tabs,
             "All fuel types" = "Total number of vehicles: {point.y:.0f}")
    })
    
    # Render the data table based only on data_type selection with 20 entries by default
    # output$data_table <- renderDT({
    #   datatable(
    #     filtered_data_table() %>%
    #       select(`Agricultural machinery`, y_col()),
    #     colnames = c("Agricultural machinery", yAxisTitle()),
    #     options = list(pageLength = 20, scrollX = TRUE)  # Show 20 entries by default, enable horizontal scrolling
    #   )
    # })
    # 
    output$data_table <- renderDT({ 
      datatable( 
        filtered_data_table() %>% 
          select(`Agricultural machinery`, !!sym(y_col)), 
        colnames = c("Agricultural machinery", "Number of vehicles"), # Use the string directly 
        options = list(pageLength = 20, scrollX = TRUE) # Show 20 entries by default, enable horizontal scrolling
        ) 
      })
    # Create a download handler for the data
    # output$downloadData <- downloadHandler(
    #   filename = function() {
    #     paste("Agricultural_Machinery_", input$tabs, ".xlsx", sep = "")
    #   },
    #   content = function(file) {
    #     write.xlsx(filtered_data_table() %>% 
    #                  select(`Agricultural machinery`, y_col()), file, rowNames = FALSE)
    #   }
    # )
    # 
    output$downloadData <- downloadHandler( 
      filename = function() { 
        paste("Agricultural_machinery_", input$tabs, ".xlsx", sep = "") 
        }, 
      content = function(file) { 
        write.xlsx(filtered_data_table() %>% 
                     select(`Agricultural machinery`, !!sym(y_col)), file, rowNames = FALSE) } )
    # Render the variable selection UI dynamically
    output$variable_select <- renderUI({
      choices <- unique(total_number_vehicles_data$`Agricultural machinery`)
      selected <- setdiff(choices, "All agricultural machinery")
      selectizeInput(
        ns("variables"), 
        "Click within the box to select variables", 
        choices = choices, 
        selected = selected,
        multiple = TRUE,
        options = list(
          plugins = list('remove_button')
        )
      )
    })
    
    # Render the bar chart using the filtered data
    barChartServer(
      id = "bar_chart",
      chart_data = chart_data,
      title = paste("Total number of agricultural machinery in Scotland in 2024"),
      yAxisTitle = yAxisTitle,
      xAxisTitle = "Agricultural machinery",
      unit = input$tabs,
      footer = NULL,
      x_col = "Agricultural machinery",
      y_col = y_col,
      tooltip_unit = tooltip_unit,
      maintain_order = FALSE
    )
  })
}
# Testing function for the entire module
totalnumberofvehiclesDemo <- function() {
  ui <- fluidPage(totalnumberofvehiclesUI("total_number_of_vehicles_demo"))
  server <- function(input, output, session) {
    totalnumberofvehiclesServer("total_number_of_vehicles_demo")
  }
  shinyApp(ui, server)
}

totalnumberofvehiclesDemo()
