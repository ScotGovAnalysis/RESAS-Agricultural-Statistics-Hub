# file: module_ag_mach_fuel

agmachfuelUI <- function(id) {
  ns <- NS(id)
  tagList(
    sidebarLayout(
      sidebarPanel(
        width = 3,
        radioButtons(ns("data_type"), "Data Type", choices = c("All tractors" = "All tractors", "Combine harvesters" = "Combine harvesters", "Self-propelled sprayers" = "Self-propelled sprayers",
                                                               "Telescopic material handlers (such as telehandlers)" = "Telescopic material handlers (such as telehandlers)", "Other lifting equipment (such as wheeled loaders, diggers and fork-lifts)" = "Other lifting equipment (such as wheeled loaders, diggers and fork-lifts)",
                                                               "All-terrain vehicle (ATV)/Quads" = "All-terrain vehicle (ATV)/Quads", "Side-by-side utility vehicles" = "Side-by-side utility vehicles"), selected = "All tractors"),
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
                   generateCensusTableFooter(),
                   
                   value = ns("data"))
        )
      )
    )
  )
}

agmachfuelServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Create reactive data excluding 'All' and depending only on data_type (for chart)
    filtered_data_chart <- reactive({
      ag_mach_fuel_data
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
          filter(`Fuel type` %in% input$variables)
      }
      data
    })
    
    # Select the appropriate column based on data_type
    y_col <- reactive({
      switch(input$data_type,
             "All tractors" = "All tractors",
             "Combine harvesters" = "Combine harvesters", 
             "Self-propelled sprayers" = "Self-propelled sprayers", 
             "Telescopic material handlers (such as telehandlers)" = "Telescopic material handlers (such as telehandlers)",
             "All-terrain vehicle (ATV)/Quads" = "All-terrain vehicle (ATV)/Quads", 
             "Side-by-side utility vehicles" = "Side-by-side utility vehicles", 
             "Other lifting equipment (such as wheeled loaders, diggers and fork-lifts)" = "Other lifting equipment (such as wheeled loaders, diggers and fork-lifts)"
      )
    })
    
    yAxisTitle <- reactive({
      switch(input$data_type,
             "All tractors" = "Number of tractors",
             "Combine harvesters" = "Number of combine harvesters",
             "Self-propelled sprayers" = "Number of self-propelled sprayers",
             "Telescopic material handlers (such as telehandlers)" = "Number of telescopic material handlers",
             "All-terrain vehicle (ATV)/Quads" = "Number of All-terrain vehicle (ATV)/Quads",
             "Side-by-side utility vehicles" = "Number of side-by-side utility vehicles",
             "Other lifting equipment (such as wheeled loaders, diggers and fork-lifts)" = "Number of other lifting equipment")
    })
    
    
    tooltip_unit <- reactive({
      # Your logic to determine the unit based on input or other conditions
      switch(input$data_type,
             "All tractors" = "Number",
             "Combine harvesters" = "Number", 
             "Self-propelled sprayers" = "Number", 
             "Telescopic material handlers (such as telehandlers)" = "Number",
             "All-terrain vehicle (ATV)/Quads" = "Number", 
             "Side-by-side utility vehicles" = "Number", 
             "Other lifting equipment (such as wheeled loaders, diggers and fork-lifts)" = "Number")
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
          select(`Fuel type`, y_col()),
        colnames = c("Status", yAxisTitle()),
        options = list(pageLength = 20, scrollX = TRUE)  # Show 20 entries by default, enable horizontal scrolling
      )
    })
    
    # Create a download handler for the data
    output$downloadData <- downloadHandler(
      filename = function() {
        paste("Fuel_type_", input$data_type, ".xlsx", sep = "")
      },
      content = function(file) {
        write.xlsx(filtered_data_table() %>% 
                     select(`Fuel type`, y_col()), file, rowNames = FALSE)
      }
    )
    
    # Render the variable selection UI dynamically
    output$variable_select <- renderUI({
      choices <- unique(ag_mach_fuel_data$`Fuel type`)
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
      title = paste("Fuel type of agriculture machinery in Scotland in", census_year),
      yAxisTitle = yAxisTitle,
      xAxisTitle = "Status",
      unit = input$data_type,
      footer = census_footer,
      x_col = "Fuel type",
      y_col = y_col,
      tooltip_unit = tooltip_unit,
      #unit_position = unit_position,
      maintain_order = FALSE
    )
  })
}

