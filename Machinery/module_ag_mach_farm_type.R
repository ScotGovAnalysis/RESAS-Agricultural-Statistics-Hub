# file: module_ag_mach_farm_type

agmachfarmtypeUI <- function(id) {
  ns <- NS(id)
  tagList(
    sidebarLayout(
      sidebarPanel(
        width = 3,
        radioButtons(ns("data_type"), "Data Type", choices = c("All tractors" = "All tractors", "Combine harvesters" = "Combine harvesters", "Self-propelled sprayers" = "Self-propelled sprayers", "Telescopic material handlers" = "Telescopic material handlers",
                                                               "All-terrain vehicle/Quads" = "All-terrain vehicle/Quads", "Side-by-side utility vehicles" = "Side-by-side utility vehicles", "Other lifting equipment" = "Other lifting equipment" ), selected = "All tractors"),
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

agmachfarmtypeServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Create reactive data excluding 'All' and depending only on data_type (for chart)
    filtered_data_chart <- reactive({
      ag_mach_farm_type_data
    })
    
    # Create reactive data for the table with commas added to numeric values
    filtered_data_table <- reactive({
      filtered_data_chart() %>%
        mutate(across(where(is.numeric), comma))
    })
    
    # Get filtered data based on selected variables (only for bar chart)
    chart_data <- reactive({
      data <- filtered_data_chart()
      if (input$tabs == ns("bar") && !is.null(input$variables)) {
        data <- data %>%
          filter(`Main farm type` %in% input$variables)
      }
      data
    })
    
    # Select the appropriate column based on data_type
    y_col <- reactive({
      switch(input$data_type,
             "All tractors" = "All tractors",
             "Combine harvesters" = "Combine harvesters", 
             "Self-propelled sprayers" = "Self-propelled sprayers", 
             "Telescopic material handlers" = "Telescopic material handlers",
             "All-terrain vehicle/Quads" = "All-terrain vehicle/Quads", 
             "Side-by-side utility vehicles" = "Side-by-side utility vehicles", 
             "Other lifting equipment" = "Other lifting equipment"
      )
    })
    
    yAxisTitle <- reactive({
      switch(input$data_type,
             "All tractors" = "Number of tractors",
             "Combine harvesters" = "Number of combine harvesters",
             "Self-propelled sprayers" = "Number of self-propelled sprayers",
             "Telescopic material handlers" = "Number of telescopic material handlers",
             "All-terrain vehicle/Quads" = "Number of all-terrain vehicles/quads",
             "Side-by-side utility vehicles" = "Number of side-by-side utility vehicles",
             "Other lifting equipment" = "Number of other lifting equipment")
    })
    
    
    tooltip_unit <- reactive({
      # Your logic to determine the unit based on input or other conditions
      switch(input$data_type,
             "All tractors" = "Number",
             "Combine harvesters" = "Number", 
             "Self-propelled sprayers" = "Number", 
             "Telescopic material handlers" = "Number",
             "All-terrain vehicle/Quads" = "Number", 
             "Side-by-side utility vehicles" = "Number", 
             "Other lifting equipment" = "Number")
    })
    # 
    # unit_position <- reactive({
    #   if (input$data_type %in% c("holdings", "area")) {
    #     return("after");
    #   } else {
    #     return("before");
    #   }
    # })
    
    # Render the data table based only on data_type selection with 20 entries by default
    output$data_table <- renderDT({
      datatable(
        filtered_data_table() %>%
          select(`Main farm type`, y_col()),
        colnames = c("Main Farm Type", yAxisTitle()),
        options = list(pageLength = 20, scrollX = TRUE)  # Show 20 entries by default, enable horizontal scrolling
      )
    })
    
    # Create a download handler for the data
    output$downloadData <- downloadHandler(
      filename = function() {
        paste("Farm_Types_", input$data_type, ".xlsx", sep = "")
      },
      content = function(file) {
        write.xlsx(filtered_data_table() %>% 
                     select(`Main farm type`, y_col()), file, rowNames = FALSE)
      }
    )
    
    # Render the variable selection UI dynamically
    output$variable_select <- renderUI({
      choices <- unique(ag_mach_farm_type_data$`Main farm type`)
      selected <- setdiff(choices, "All")
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
      title = paste("Number of agricultural machinery by main farm types in Scotland in 2024"),
      yAxisTitle = yAxisTitle,
      xAxisTitle = "Main farm type",
      unit = input$data_type,
      footer = NULL,
      x_col = "Main farm type",
      y_col = y_col,
      tooltip_unit = tooltip_unit,
      #unit_position = unit_position,
      maintain_order = FALSE
    )
  })
}

