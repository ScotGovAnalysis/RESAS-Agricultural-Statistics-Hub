employeesMapUI <- function(id) {
  ns <- NS(id)
  sidebarLayout(
    sidebarPanel(
      width = 3,
      uiOutput(ns("sidebar_ui"))
    ),
    mainPanel(
      width = 9,
      tabsetPanel(
        id = ns("tabs"),
        tabPanel(
          "Maps",
          value = "map",
          conditionalPanel(
            condition = sprintf("input['%s'] == 'region'", ns("map_region")),
            mapUI(ns("map"))
          ),
          conditionalPanel(
            condition = sprintf("input['%s'] == 'la'", ns("map_region")),
            mapUnitaryUI(ns("map_uni"))
          )
        ),
        # tabPanel("Constituency Map", mapConstituenciesUI(ns("map_con")), value = "map_con"),
        tabPanel("Time Series", 
                 lineChartUI(ns("line_chart"), note_type = 2),  # Use note_type = 2 for the second note
                 value = "timeseries"),
        tabPanel("Data Table", 
                 DTOutput(ns("data_table")),
                 div(
                   style = "background-color: #f0f0f0; padding: 15px; border-radius: 5px; margin-top: 10px; font-style: bold;",
                   "Note: Migrant labour is not directly comparable to other employee measures as it is measured in person working days."
                 ),  # Separate note about migrant labour
                 downloadButton(ns("downloadData"), "Download Data"),   
                 generateCensusTableFooter(),  # Existing footer

                 value = "data_table")
      )
    )
  )
}


employeesMapServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Constituency map
    # employee_const_map <- reactive({
    #   workforce_constituency %>%
    #     mutate(across(everything(), as.character)) %>%
    #     pivot_longer(
    #       cols = -`workforce`,
    #       names_to = "constituency",
    #       values_to = "value"
    #     ) %>% 
    #     mutate(
    #       value = if_else(is.na(value), NA_integer_, as.integer(value))
    #     )
    # })
    
    # Maps
    output$sidebar_ui <- renderUI({
      req(input$tabs)
      if (input$tabs == "map") {
        tagList(
          selectInput(
            ns("map_region"),
            "Geography",
            choices = c(
              "Agricultural Region" = "region",
              "Local Authority" = "la"
            ),
            selected = "region"
          ),
          radioButtons(
            ns("map_variable"),
            "Select Variable",
            choices = c(
              "Regular full-time staff total",
              "Regular part-time staff total",
              "Total casual and seasonal staff",
              "Total workforce (including occupiers)")
          )
        )
      # } else if (input$tabs == "map_con") {
      #   radioButtons(ns("variable_con"), "Select Variable", choices = categories_con)
        
      # Data table
      } else if (input$tabs == "data_table") {
        radioButtons(ns("data_source"), "Choose data to show:", 
                     choices = c("Agricultural Region Data",
                                 "Local Authority Data",
                                 "Time Series Data" ))
        
      # Time series
      } else if (input$tabs == "timeseries") {
        selectizeInput(
          ns("variables"), 
          "Click within the box to add more variables", 
          choices = unique(employees_timeseries_chart$`Occupiers and employees by category`), 
          selected = c('Regular full-time staff total', 
                       'Regular part-time staff total', 
                       'Total casual and seasonal staff'), 
          multiple = TRUE, 
          options = list(plugins = list('remove_button'), placeholder = "Click to add more variables")
        )
      }
    })
    
    
    # Time series chart server
    lineChartServer(
      id = "line_chart",
      chart_data = reactive({
        req(input$variables)
        employees_timeseries_chart %>%
          filter(`Occupiers and employees by category` %in% input$variables)
      }),
      title = "Agricultural employees over time",
      yAxisTitle = "Employees (1,000)",
      xAxisTitle = "Year",
      unit = "employees",
      footer = '<div style="font-size: 16px; font-weight: bold;"><a href="https://www.gov.scot/publications/results-from-the-scottish-agricultural-census-june-2026/">Source: Scottish Agricultural Census: June 2026</a></div>',
      x_col = "Year",
      y_col = "Value"
    )
    
    # Agricultural region map server
    mapServer(
      id = "map",
      data = reactive({
        req(input$map_variable)
        employees_region_map %>%
          filter(`Occupiers and employees by category` == input$map_variable)
      }),
      unit = "employees",
      footer = '<div style="font-size: 16px; font-weight: bold;"><a href="https://www.gov.scot/publications/results-from-the-scottish-agricultural-census-june-2026/">Source: Scottish Agricultural Census: June 2026</a></div>',
      variable = reactive(input$map_variable),
      title = paste("Agricultural employees by region in Scotland in", census_year),
      legend_title = "Number of employees"
    )  
    
    # Constituency map
    # mapConstituenciesServer(
    #   id = "map_con",
    #   data = reactive({
    #     req(input$variable_con)
    #     employee_const_map() %>% filter(`workforce` == input$variable_con)
    #   }),
    #   unit = "employees",
    #   footer = census_footer,
    #   variable = reactive(input$variable_con),
    #   title = paste("Agricultural employees by 2026 Scottish Parliamentary Constituency"),
    #   legend_title = "Employees (Number)"
    # )
    
    # Local authority map server
    mapUnitaryServer(
      id = "map_uni",
      data = reactive({
        req(input$map_variable)
        employees_unitauth_map %>% 
          filter(workforce == input$map_variable)
      }),
      unit = "employees",
      footer = census_footer,
      variable = reactive(input$map_variable),
      title = paste("Agricultural employees by local authority in", census_year),
      legend_title = "Employees (Number)"
    )
    
    # Data table
    output$data_table <- renderDT({
      req(input$data_source)
      
      if (input$data_source == "Time Series Data") {
        employees_timeseries_table %>% 
          mutate(across(where(is.numeric), comma)) %>% 
          datatable(., options = list(scrollX = TRUE, pageLength = 26
        ))
        
      } else if (input$data_source == "Agricultural Region Data") {
        employees_region_table %>% 
          mutate(across(where(is.numeric), comma)) %>% 
          datatable(., options = list(scrollX = TRUE, pageLength = 10
        ))
        
      # } else if (input$data_source == "Constituency Data") {
      #   con_data() %>% 
      #     mutate(across(where(is.numeric) & !contains("Year"), comma)) %>% 
      #     datatable(., options = list(scrollX = TRUE, pageLength = 20
      #   ))

      } else if (input$data_source == "Local Authority Data") {
        employees_unitauth_table %>%
          mutate(across(where(is.numeric), comma)) %>%
          datatable(, options = list(scrollX = TRUE, pageLength = 20
        ))
      }
    })
    
    # ---------------------
    # DOWNLOAD HANDLER
    # ---------------------
    output$downloadData <- downloadHandler(
      filename = function() {
        if (input$data_source == "Time Series Data") {
          "Scottish Agricultural Employees Time Series Data - 2016 to 2026.csv"
        } else if (input$data_source == "Agricultural Region Data") {
          "Scottish Agricultural Employees Agricultural Regional Data - 2026.csv"
        # } else if (input$data_source == "Constituency Data") {
        #   "Scottish Agricultural Employees Constituency Data - 2026.csv"
        } else if (input$data_source == "Local Authority Data") {
          "Scottish Agricultural Employees Local Authority Data - 2026.csv"
        }
      },
      
      content = function(file) {
        
        if (input$data_source == "Time Series Data") {
          write.csv(employees_timeseries_table, file, row.names = FALSE)
          
        } else if (input$data_source == "Agricultural Region Data") {
          write.csv(employees_region_table, file, row.names = FALSE)
          
        # } else if (input$data_source == "Constituency Data") {
        #   write.csv(fmt(con_data()), file, row.names = FALSE)
          
        } else if (input$data_source == "Local Authority Data") {
          write.csv(employees_unitauth_table, file, row.names = FALSE)
        }
      }
    )
  }
  )
}


# Testing module
content_demo <- function() {
  ui <- fluidPage(employeesMapUI("employees_map_test"))
  server <- function(input, output, session) {
    employeesMapServer("employees_map_test")
  }
  shinyApp(ui, server)
}

content_demo()
