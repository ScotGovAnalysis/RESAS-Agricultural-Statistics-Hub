# File: module_stockfeeding.R

stockfeedingUI <- function(id) {
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
          choices = unique(stockfeeding_region_map$`Land use by category`)
        )
      ),
      
      # Constituency map
      # conditionalPanel(
      #   condition = "input.tabsetPanel === 'Constituency Map'",
      #   ns = ns,
      #   radioButtons(
      #     ns("variable_con"), 
      #     "Select Variable", 
      #     choices = unique(stockfeeding_constituency$crop)
      #   )
      # ),     
      # 
      
      # Time series
      conditionalPanel(
        condition = "input.tabsetPanel === 'Time Series'",
        ns = ns,
        checkboxGroupInput(
          ns("timeseries_variables"),
          "Select Time Series Variables",
          choices = unique(stockfeeding_timeseries_chart$`Crop/land use`),
          selected = c(
            "Turnips/swedes",
            "Kale/cabbage",
            "Maize",
            "Rape",
            "Fodder beet",
            "Lupins"
          )
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
          selected = "map"
        )
      )
    ),
    
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
                 downloadButton(ns("download_data"), "Download Data"),
                 generateCensusTableFooter()

        )
      )
    )
  )
}

stockfeedingServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Agricultural region map
    mapServer(
      id = "map",
      data = reactive({
        req(input$map_variable)
        stockfeeding_region_map %>% 
          filter(`Land use by category` == input$map_variable)
      }),
      unit = "hectares",
      footer = census_footer,
      variable = reactive(input$map_variable),
      title = paste("Stockfeeding crops distribution by region in Scotland in", census_year),
      legend_title = "Area (hectares)"
    )
    
    # Constituency map
    # stock_const_map <- reactive({
    #   stockfeeding_constituency %>%        
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
    
    # mapConstituenciesServer(
    #   id = "map_con",
    #   data = reactive({
    #     req(input$variable_con)
    #     stock_const_map() %>% filter(`crop` == input$variable_con)
    #   }),
    #   unit = "hectares",
    #   footer = census_footer,
    #   variable = reactive(input$variable_con),
    #   title = paste("Stockfeeding crops distribution by 2026 Scottish Parliamentary Constituency"),
    #   legend_title = "Area (hectares)"
    # )
     
    # Local authority map
    mapUnitaryServer(
      id = "map_uni",
      data = reactive({
        req(input$map_variable)
        stockfeeding_unitauth_map %>% 
          filter(`crop` == input$map_variable)
      }),
      unit = "hectares",
      footer = census_footer,
      variable = reactive(input$map_variable),
      title = paste("Stockfeeding crops distribution by local authority in", census_year),
      legend_title = "Area (hectares)"
    )
    
    # Time series chart
    chart_data <- reactive({
      req(input$timeseries_variables)
      filtered_data <- stockfeeding_timeseries_chart %>%
        filter(`Crop/land use` %in% input$timeseries_variables)
      filtered_data
    })
    
    lineChartServer(
      id = "line",
      chart_data = chart_data,
      title = "Area used to grow crops for stockfeeding over time",
      yAxisTitle = "Area of crops (1,000 hectares)",
      xAxisTitle = "Year",
      unit = "hectares",
      footer = census_footer,
      x_col = "year",
      y_col = "value"
    )
    
    # Data table
    output$table <- renderDT({
      req(input$tabsetPanel == "Data Table")
      
      # -------------------------
      # Select which dataset to show
      # -------------------------
      data <- switch(input$table_data,
                     
                     # -------------------
                     # 1. Agricultural Region data
                     # -------------------
                     "map" = {
                       stockfeeding_region_table %>%
                         mutate(across(where(is.numeric), comma))
                     },
                     
                     # -------------------
                     # 2. Timeseries data
                     # -------------------
                     "timeseries" = {
                       stockfeeding_timeseries_table %>%
                         mutate(across(where(is.numeric), comma))
                     },
                     
                     # -------------------
                     # # 3. Constituency Table
                     # # -------------------
                     # "map_con" = {
                     #   stockfeeding_constituency %>%
                     #     rename(`Land use by category` = `crop`) %>%
                     #     mutate(across(where(is.numeric), comma))
                     # },
                      
                     # -------------------
                     # 4. Local authority table
                     # -------------------
                     "map_uni" = {
                       stockfeeding_unitauth_table %>%
                         mutate(across(where(is.numeric), comma))
                     },
      )
      
      # -------------------------
      # Render the chosen table
      # -------------------------
      datatable(
        data,
        options = list(
          scrollX = TRUE,
          autoWidth = TRUE,
          pageLength = 20,
          columnDefs = list(
            list(width = '200px', targets = 1)
          )
        )
      )
    })
    
    # Data Download Handler
    output$download_data <- downloadHandler(
      
      # ---- Dynamic filename depending on selected table ----
      filename = function() {
        
        switch(input$table_data,
               "map" = paste0("Stockfeeding_Agricultural_Region_Map_Data_", Sys.Date(), ".csv"),
               "timeseries" = paste0("Stockfeeding_Timeseries_Data_", Sys.Date(), ".csv"),
               # "map_con" = paste0("Stockfeeding_Constituency_Data_", Sys.Date(), ".csv"),
               "map_uni" = paste0("Stockfeeding_Local_Authority_Data_", Sys.Date(), ".csv"),
               
               # fallback
               paste0("Downloaded_Data_", Sys.Date(), ".csv")
        )
      },
      
      # ---- Write selected dataset to disk ----
      content = function(file) {
        
        data <- switch(input$table_data,
                       
                       # ---- Agricultural region map ----
                       "map" = {
                         stockfeeding_subregion %>%
                           filter(`Land use by category` == input$map_variable)
                       },
                       
                       # ---- Timeseries ----
                       "timeseries" = {
                         stockfeeding_timeseries_table
                       },
                       
                       # ---- Constituency ----
                       # "map_con" = {
                       #   stockfeeding_constituency
                       # },
                       
                       # ---- Local authority ----
                       "map_uni" = {
                         stockfeeding_unitauth_table
                       }
        )
        
        write.csv(data, file, row.names = FALSE)
      }
    )
  }
  )
}

stockfeeding_demo <- function() {
  ui <- fluidPage(stockfeedingUI("stockfeeding_test"))
  server <- function(input, output, session) {
    stockfeedingServer("stockfeeding_test")
  }
  shinyApp(ui, server)
}
stockfeeding_demo()