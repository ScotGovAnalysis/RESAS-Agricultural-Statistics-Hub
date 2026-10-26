# File: module_cereals.R

cerealsUI <- function(id) {
  ns <- NS(id)
  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      
      # Maps
      conditionalPanel(
        condition = "input.tabsetPanel === 'Maps'",
        ns = ns,
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
          choices = unique(cereals_region_map$`Land use by category`)
        )
      ),
      
      # Constituency map
      # conditionalPanel(
      #   condition = "input.tabsetPanel === 'Constituency Map'",
      #   ns = ns,
      #   radioButtons(
      #     ns("variable_con"), 
      #     "Select Variable", 
      #     choices = c(
      #       "Wheat" = "Wheat",
      #       "Winter barley" = "Winter barley",
      #       "Spring barley" = "Spring barley",
      #       "Oats and Mixed Grain" = "Oats and mixed grain"
      #     )
      #   )
      # ),

      # Time series
      conditionalPanel(
        condition = "input.tabsetPanel === 'Time Series'",
        ns = ns,
        
        radioButtons(
          ns("measure"),
          "Select Measure:",
          choices = unique(cereals_combined_long$Measure),
          selected = "Area"
        ),
        
        # Single checkboxGroupInput for all measures
        checkboxGroupInput(
          ns("timeseries_variables"),
          "Select crops to display:",
          choices =  c("Total cereals", "Main cereals (barley, oats and wheat)",
                        "Spring barley", "Winter barley", "Barley total",
                        "Spring oats", "Winter oats", "Oats total", 
                        "Rye", "Triticale", "Wheat"),
          selected = c("Wheat", "Barley total", "Oats total")
        )
      ),
      
      # ===================== DATA TABLE =====================
      conditionalPanel(
        condition = "input.tabsetPanel === 'Data Table'",
        ns = ns,
        radioButtons(
          ns("table_data"),
          "Select Data to Display",
          choices = c("Agricultural Region Data" = "map",
                      "Local Authority Data" = "map_uni",
                      "Time Series Data" = "timeseries"
                      #"Constituency Data" = "map_con",
                     ),
          selected = "timeseries"
        )
      ),
    
    ## Hidden summary tab for now until fully developed or decide to remove
    
      # Summary
      # conditionalPanel(
      #   condition = "input.tabsetPanel === 'Cereals Summary'",
      #   ns = ns,
      #   div("Adjust the sliders to compare data from different years.",
      #       style = "font-size: 14px; font-weight: bold; margin-bottom: 10px;"),
      #   sliderInput(ns("summary_current_year_cereals"), "Year of interest",
      #               min = 2012, max = census_year, value = census_year, step = 1, sep = ""),
      #   sliderInput(ns("summary_comparison_year_cereals"), "Comparison year",
      #               min = 2012, max = census_year, value = census_year-1, step = 1, sep = "")
      # )
    ),
    
    # Main panel
    mainPanel(
      width = 9,
      tabsetPanel(
        id = ns("tabsetPanel"),
        tabPanel(
          "Maps",
          conditionalPanel(
            condition = sprintf("input['%s'] == 'region'", ns("map_region")),
            mapUI(ns("map"))
          ),
          conditionalPanel(
            condition = sprintf("input['%s'] == 'la'", ns("map_region")),
            mapUnitaryUI(ns("map_uni"))
          )
        ),
        # tabPanel("Constituency Map", mapConstituenciesUI(ns("map_con"))),
        tabPanel("Time Series", lineChartUI(ns("line"), note_type = 2)),
        tabPanel("Data Table", 
                 DTOutput(ns("table")),
                 downloadButton(ns("downloadData"), "Download Data"),
                 generateCerealandoilseedTableFooter()
        )
        
        ## Hidden summary tab for now until fully developed or decide to remove
        
        # tabPanel("Summary",
        #          fluidRow(
        #            column(width = 6, h3("Cereals summary section")),
        #            column(width = 3,
        #                   selectInput(ns("summary_variable"),
        #                               "Select Variable",
        #                               choices = unique(cereals_data_census$`Crop/land use`),
        #                               selected = "Total cereals"))
        #          ),
        #          fluidRow(
        #            column(width = 6, p("This content is under development.")),
        #            column(width = 6, valueBoxUI(ns("summaryValueBox")),
        #                   style = "padding-right: 0; padding-left: 0; padding-bottom: 10px;")
        #          )
        # )
      )
    )
  ) # end sidebarLayout
}


cerealsServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Agricultural region map
    mapServer(
      id = "map",
      data = reactive({
        req(input$map_variable)
        cereals_region_map %>% 
          filter(`Land use by category` == input$map_variable)
      }),
      unit = "hectares",
      footer = census_footer,
      variable = reactive(input$map_variable),
      title = paste("Cereals distribution by region in Scotland in", census_year),
      legend_title = "Area (hectares)"
    )
    
    # Constitency map
    # cereal_const_map <- reactive({
    #   cereals_constituency %>%        
    #     mutate(across(everything(), as.character)) %>%
    #     pivot_longer(
    #       cols = -`crop`,
    #       names_to = "constituency",
    #       values_to = "value"
    #     ) %>% 
    #     mutate(
    #       value = if_else(is.na(value), NA_real_, as.numeric(value))
    #     )
    # })
    # 
    # mapConstituenciesServer(
    #   id = "map_con",
    #   data = reactive({
    #     req(input$variable_con)
    #     cereal_const_map() %>% filter(`crop` == input$variable_con)
    #   }),
    #   unit = "hectares",
    #   footer = census_footer,
    #   variable = reactive(input$variable_con),
    #   title = paste("Cereals distribution by 2026 Scottish Parliamentary Constituency"),
    #   legend_title = "Area (hectares)"
    # )
    
    # Local authority map
    mapUnitaryServer(
       id = "map_uni",
       data = reactive({
         req(input$map_variable)
         cereals_unitauth_map %>% 
           filter(`crop` == input$map_variable)
       }),
       unit = "hectares",
       footer = census_footer,
       variable = reactive(input$map_variable),
       title = paste("Cereals distribution by local authority in", census_year),
       legend_title = "Area (hectares)"
     )

    # Time series
    observeEvent(input$measure, {
      if (input$measure == "Area") {
        choices <- c("Total cereals", "Main cereals (barley, oats and wheat)",
                   "Spring barley", "Winter barley", "Barley total",
                   "Spring oats", "Winter oats", "Oats total", 
                   "Rye", "Triticale", "Wheat")
        selected <- c("Wheat", "Barley total", "Oats total")
      } else {
        choices <- c("Main cereals (barley, oats and wheat)",
                     "Spring barley", "Winter barley", "Barley total",
                     "Oats total", "Wheat")
        selected <- c("Barley total","Oats total","Wheat")
      }
      
      updateCheckboxGroupInput(
        session,
        "timeseries_variables",
        choices = choices,
        selected = selected
      )
    })
    
    # Reactive data for line chart
    line_chart_data <- reactive({
      req(input$timeseries_variables, input$measure)
      
      if (input$measure == "Area") {
        df <- cereals_area_chart %>%
          filter(`Crop/land use` %in% input$timeseries_variables)
        
      } else {
        df <- cereals_tiff_data_long %>%
          filter(
            `Crop/land use` %in% input$timeseries_variables,
            Measure == input$measure
          ) %>%
          rename(value = Value) %>%
          mutate(Year = as.numeric(Year))
        
        # Keep only last 10 years
        max_year <- max(df$Year, na.rm = TRUE)
        df <- df %>% filter(Year > max_year - 10)
      }
      
      validate(
        need(nrow(df) > 0, "No data available for the selected options.")
      )
      
      df
    })
    
    # Chart settings based on measure
    line_chart_settings <- reactive({
      req(input$measure)
      
      switch(input$measure,
             "Area" = list(
               title = "Area used to grow cereals over time",
               yAxisTitle = "Area of cereals (1,000 hectares)",
               unit = "hectares",
               footer = census_footer
             ),
             "Production" = list(
               title = "Production of cereals over time",
               yAxisTitle = "Production of cereals (1,000 tonnes)",
               unit = "tonnes",
               footer = cereal_oilseed_footer
             ),
             "Yield" = list(
               title = "Yield of cereals over time",
               yAxisTitle = "Yield of cereals (tonnes/hectare)",
               unit = "tonnes/hectare",
               footer = cereal_oilseed_footer
             )
      )
    })
    
    observeEvent(input$measure, {
      settings <- line_chart_settings()
      
      lineChartServer(
        id = "line",
        chart_data = line_chart_data,
        title = settings$title,
        yAxisTitle = settings$yAxisTitle,
        xAxisTitle = "Year",
        unit = settings$unit,
        footer = settings$footer,   # use the footer from settings
        x_col = "Year",
        y_col = "value"
      )
    })
    
    # Data table
    output$table <- renderDT({
      req(input$tabsetPanel == "Data Table")
      
      # Agricultural region data
      if (input$table_data == "map") {
        req(input$map_variable)
        
        cereals_region_table %>%
          mutate(across(where(is.numeric), comma)) %>%
          datatable(
            options = list(
              scrollX = TRUE,
              pageLength = 20,
              autoWidth = TRUE,
              columnDefs = list(
                list(width = '100px', targets = 1)
              )
            )
          )
        
      # Time series data
      } else if (input$table_data == "timeseries") {
        
        ts_data <- cereals_timeseries_table %>% 
          mutate(across(where(is.numeric), ~ case_when(
            Measure == "Yield (tonnes per hectare)" ~ as.character(round(.x, 1)),
            TRUE ~ comma(round(.x, 0)))))
        
        left_cols  <- c("Crop/land use", "Measure")
        right_cols <- setdiff(names(ts_data), left_cols)
        
        datatable(
          ts_data,
          options = list(
            scrollX = TRUE,
            pageLength = 20,
            autoWidth = TRUE
          )
        ) %>%
          formatStyle(left_cols,  `text-align` = "left") %>%
          formatStyle(right_cols, `text-align` = "right")
        
      # Constituency data
      # } else if (input$table_data == "map_con") {
      #   
      #   cereals_constituency %>%
      #     rename(`Crop/land use` = crop) %>%
      #     mutate(across(where(is.numeric), comma)) %>%
      #     datatable(
      #       options = list(
      #         scrollX = TRUE,
      #         pageLength = 20,
      #         autoWidth = TRUE
      #       )
      #     )
      
        # Local authority data
      } else if (input$table_data == "map_uni") {

        cereals_unitauth_table %>%
          mutate(across(where(is.numeric), comma)) %>%
          datatable(
            options = list(
              scrollX = TRUE,
              pageLength = 20,
              autoWidth = TRUE
            )
          )

       }
    })
    
    output$downloadData <- downloadHandler(
      filename = function() {
        
        if (input$table_data == "map") {
          paste("Cereals_Map_Data_", Sys.Date(), ".csv", sep = "")
          
        } else if (input$table_data == "timeseries") {
          paste("Cereals_Timeseries_Data_", Sys.Date(), ".csv", sep = "")
          
        # } else if (input$table_data == "map_con") {
        #   paste("Cereals_Constituency_Data_", Sys.Date(), ".csv", sep = "")
           
        } else if (input$table_data == "table4") {
          paste("Cereals_Local_Authority_Data_", Sys.Date(), ".csv", sep = "")
        }
        
      },
      
      content = function(file) {
        
        # Agricultural region
        data <- if (input$table_data == "map") {

          cereals_region_table %>%
            filter(`Land use by category` == input$map_variable)
          
        # Time series
        } else if (input$table_data == "timeseries") {

          cereals_timeseries_table 
          
        # Constituency
        # } else if (input$table_data == "map_con") {
          
        #   cereals_constituency 
          
        # Local authority
        } else if (input$table_data == "map_uni") {
           
          cereals_unitauth_table 
           
        }
        
        # Write CSV (no formatting so numbers stay numeric)
        write.csv(data, file, row.names = FALSE)
      }
    )
    
    
    # Reactive expression for the selected variable and years
    summary_data <- reactive({
      cereals_data_census %>%
        filter(`Crop/land use` == input$summary_variable) %>%
        pivot_longer(cols = -`Crop/land use`, names_to = "Year", values_to = "Value") %>%
        mutate(Year = as.numeric(Year))
    })
    
    current_year <- reactive({ input$summary_current_year_cereals })
    comparison_year <- reactive({ input$summary_comparison_year_cereals })
    
    # Value box for the selected variable
    valueBoxServer("summaryValueBox", summary_data, "Crop/land use", reactive(input$summary_variable), current_year, comparison_year, "ha")
  })
}

# 
# Testing module
cereals_demo <- function() {
  ui <- fluidPage(cerealsUI("cereals_test"))
  server <- function(input, output, session) {
    cerealsServer("cereals_test")
  }
  shinyApp(ui, server)
}

cereals_demo()

