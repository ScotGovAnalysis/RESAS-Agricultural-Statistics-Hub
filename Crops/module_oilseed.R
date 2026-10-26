# File: module_oilseed.R

oilseedUI <- function(id) {
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
          choices = unique(oilseed_region_map$`Land use by category`)
        )
      ),
      
      # Constituency map
      # conditionalPanel(
      #   condition = "input.tabsetPanel === 'Constituency Map'",
      #   ns = ns,
      #   radioButtons(
      #     ns("variable_con"), 
      #     "Select Variable", 
      #     choices = unique(oilseeds_constituency$crop)
      #   )
      # ),
      
      # Time series
      conditionalPanel(
        condition = "input.tabsetPanel === 'Time Series'",
        ns = ns,
        radioButtons(
          ns("measure"),
          "Select Measure:",
          choices = unique(oilseed_combined_long$Measure),
          selected = "Area"
        ),
        
        # Single checkboxGroupInput for all measures
        checkboxGroupInput(
          ns("timeseries_variables"),
          "Select crops to display:",
          choices = c("Total oilseeds", "Linseed", "Winter oilseed rape","Spring oilseed rape"),
          selected = c("Winter oilseed rape","Spring oilseed rape")
        )
      ),

      # Data table
      conditionalPanel(
        condition = "input.tabsetPanel === 'Data Table'",
        ns = ns,
        radioButtons(
          ns("table_data"),
          "Select Data to Display",
          choices = c("Agricultural Region Data" = "map", 
                      "Local Authority Data" = "map_uni",
                      "Time Series Data" = "timeseries"
                      # "Constituency Data" = "map_con",
                      ),
          selected = "timeseries"
        )
      )
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
        tabPanel("Time Series", lineChartUI(ns("line"))),
        tabPanel("Data Table", 
                 DTOutput(ns("table")),
                 downloadButton(ns("downloadData"), "Download Data"),
                 generateCerealandoilseedTableFooter()

        )
      )
    )
  )
}

oilseedServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Agricultural region map
    mapServer(
      id = "map",
      data = reactive({
        req(input$map_variable)
        oilseed_region_map %>% 
          filter(`Land use by category` == input$map_variable)
      }),
      unit = "hectares",
      footer = census_footer,
      variable = reactive(input$map_variable),
      title = paste("Oilseed distribution by region in Scotland in", census_year),
      legend_title = "Area (hectares)"
    )
    
    # Constituency map
    # oilseed_const_map <- reactive({
    #   oilseeds_constituency %>%  
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
    #     oilseed_const_map() %>% filter(`crop` == input$variable_con)
    #   }),
    #   unit = "hectares",
    #   footer = census_footer,
    #   variable = reactive(input$variable_con),
    #   title = paste("Oilseed distribution by 2026 Scottish Parliamentary Constituency"),
    #   legend_title = "Area (hectares)"
    # )
    
    # Local authority map
    mapUnitaryServer(
      id = "map_uni",
      data = reactive({
        req(input$map_variable)
        oilseed_unitauth_map %>% 
          filter(`crop` == input$map_variable)
      }),
      unit = "hectares",
      footer = census_footer,
      variable = reactive(input$map_variable),
      title = paste("Oilseed distribution by local authority in", census_year),
      legend_title = "Area (hectares)"
    )
    
    # Time series
    observeEvent(input$measure, {
      if (input$measure == "Area") {
        choices <- c("Total oilseeds", "Linseed", "Winter oilseed rape","Spring oilseed rape")
        selected <- c("Winter oilseed rape", "Spring oilseed rape")
      } else {
        choices <- unique(oilseed_tiff_data_long$`Crop/land use`)
        selected <- c("Oilseed Rape")
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
        df <- oilseed_area_chart %>%
          filter(`Crop/land use` %in% input$timeseries_variables)
        
      } else {
        df <- oilseed_tiff_data_long %>%
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
    
    line_chart_settings <- reactive({
      req(input$measure)
      
      switch(input$measure,
             "Area" = list(
               title = "Area of Oilseeds Planted",
               yAxisTitle = "Area of Oilseed (1,000 hectares)",
               unit = "hectares",
               footer = census_footer
             ),
             "Production" = list(
               title = "Production of Oilseed Rape",
               yAxisTitle = "Production of Oilseed Rape (1,000 tonnes)",
               unit = "tonnes",
               footer = cereal_oilseed_footer
             ),
             "Yield" = list(
               title = "Yield of Oilseed Rape",
               yAxisTitle = "Yield of Oilseed Rape (tonnes/hectare)",
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
        footer = settings$footer,   # use footer from settings
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
        
        oilseed_region_table %>%
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
        
        ts_data <- oilseed_combined_long %>%
          pivot_wider(
            id_cols = c(`Crop/land use`, Measure),
            names_from = Year,
            values_from = Value) %>%
          mutate(
            Measure = recode(
              Measure,
              "Area"       = "Area (hectares)",
              "Production" = "Production (tonnes)",
              "Yield"      = "Yield (tonnes per hectare)")) %>%
          arrange(Measure) %>% 
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
      #   oilseeds_constituency %>%
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

        oilseed_unitauth_table %>% 
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
          paste("Oilseed_Map_Data_", Sys.Date(), ".csv", sep = "")
          
        } else if (input$table_data == "timeseries") {
          paste("Oilseed_Timeseries_Data_", Sys.Date(), ".csv", sep = "")
          
        # } else if (input$table_data == "map_con") {
        #   paste("Oilseed_Constituency_Data_", Sys.Date(), ".csv", sep = "")
        
        } else if (input$table_data == "table4") {
          paste("Oilseed_Local_Authority_Data_", Sys.Date(), ".csv", sep = "")
        }
        
      },
      
      content = function(file) {
        
        # Agricultural region data
        data <- if (input$table_data == "map") {
          oilseed_region_table %>%
            filter(`Land use by category` == input$variable)
         
        # Time series data
        } else if (input$table_data == "timeseries") {
          oilseed_timeseries_table
          
        # Constituency
        # } else if (input$table_data == "map_con") {
        #   oilseeds_constituency 
        
        # Local authority data
        } else if (input$table_data == "map_uni") {
          oilseed_unitauth_table
        }
        
        # Write CSV (no formatting so numbers stay numeric)
        write.csv(data, file, row.names = FALSE)
      }
    )
    
  })
}
    

# Testing module
oilseed_demo <- function() {
  ui <- fluidPage(oilseedUI("oilseed_test"))
  server <- function(input, output, session) {
    oilseedServer("oilseed_test")
  }
  shinyApp(ui, server)
}

oilseed_demo()
