# File: module_cattle.R
cattleUI <- function(id) {
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
            "Local Authority" = "la"),
          selected = "region"
        ),
        
        radioButtons(
          ns("map_variable"),
          "Select Variable",
          choices = c(
            "Total cattle" = "Total cattle",
            "Total female dairy cattle" = "Total female dairy cattle",
            "Total female beef cattle" = "Total female beef cattle",
            "Total male cattle" = "Total male cattle",
            "Total calves" = "Total calves"
          )
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
      #       "Total cattle" = "Total cattle",
      #       "Total female dairy cattle" = "Total female dairy cattle",
      #       "Total female beef cattle" = "Total female beef cattle",
      #       "Total male cattle" = "Total male cattle",
      #       "Total calves" = "Total calves"
      #     )
      #   )
      # ),
      
      # Time series 
      conditionalPanel(
        condition = "input.tabsetPanel === 'Time Series'",
        ns = ns,
        selectizeInput(
          ns("timeseries_variables"),
          "Click within the box to select variables (not all shown)",
          choices = unique(cattle_timeseries_chart$`Cattle by category`),
          selected = c(
            "Total female dairy cattle",
            "Total female beef cattle",
            "Total male cattle",
            "Total calves"
          ),
          multiple = TRUE,
          options = list(
            plugins = list('remove_button')
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
                   #   "Constituency Data" = "map_con",
                      
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
        #tabPanel("Constituency Map", mapConstituenciesUI(ns("map_con"))),
        tabPanel("Time Series", lineChartUI(ns("line"), note_type = 2)),
        tabPanel("Data Table", 
                 DTOutput(ns("table")),
                 downloadButton(ns("downloadData"), "Download Data"),
                 generateCensusTableFooter()
        )
      )
    )
  )
}


cattleServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Agricultural region map server
    mapServer(
      id = "map",
      data = reactive({
        req(input$map_variable)
        cattle_region_map %>% filter(`Livestock by category` == input$map_variable)
      }),
      footer = census_footer,
      variable = reactive(input$map_variable),
      title = paste("Cattle distribution by region in Scotland in", census_year),
      legend_title = "Number of cattle"
    )

    # Local authority map server
    mapUnitaryServer(
      id = "map_uni",
      data = reactive({
        req(input$map_variable)
        cattle_unitauth_map %>% filter(`livestock` == input$map_variable)
      }),
      unit = "number",
      footer = census_footer,
      variable = reactive(input$map_variable),
      title = paste("Cattle distribution by local authority in", census_year),
      legend_title = "Cattle (number)"
    )
    
    # Constituency map server
    # cattle_const_map <- reactive({
    #   cattle_constituency %>%         
    #     mutate(across(everything(), as.character)) %>%
    #     pivot_longer(
    #       cols = -`livestock`,
    #       names_to = "constituency",
    #       values_to = "value"
    #     ) %>% 
    #     mutate(
    #       value = if_else(is.na(value), NA_integer_, as.integer(value))
    #     )
    # })
    
    # mapConstituenciesServer(
    #   id = "map_con",
    #   data = reactive({
    #     req(input$variable_con)
    #     cattle_const_map() %>% filter(`livestock` == input$variable_con)
    #   }),
    #   unit = "number",
    #   footer = census_footer,
    #   variable = reactive(input$variable_con),
    #   title = paste("Cattle distribution by 2026 Scottish Parliamentary Constituency"),
    #   legend_title = "Cattle (number)"
    # )
    
    # Processing data for time series
    chart_data <- reactive({
      req(input$timeseries_variables)
      filtered_data <- cattle_timeseries_chart %>% 
        filter(`Cattle by category` %in% input$timeseries_variables)
      filtered_data
    })
    
    # Line Chart Server
    lineChartServer(
      id = "line",
      chart_data = chart_data,
      title = "Number of cattle by category across time",
      yAxisTitle = "Number of cattle (1,000)",
      xAxisTitle = "Year",
      footer = census_footer,
      x_col = "year",
      y_col = "value"
    )
    
    # ===================== DATA TABLE =====================
    
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
                       cattle_region_table %>% 
                         mutate(across(where(is.numeric), comma))
                     },
                     
                     # -------------------
                     # 2. Timeseries data
                     # -------------------
                     "timeseries" = {
                       cattle_timeseries_table %>% 
                         mutate(across(where(is.numeric), comma))
                     },
                     
                     # -------------------
                     # 3. Constituency Table
                     # -------------------
                     # "map_con" = {
                     #   cattle_constituency %>%
                     #     rename(`Livestock by category` = `livestock`) %>%
                     #     mutate(across(where(is.numeric), comma))
                     # },
                      
                     # -------------------
                     # 4. Local authority table
                     # -------------------
                     "map_uni" = {
                       cattle_unitauth_table %>% 
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
    output$downloadData <- downloadHandler(
      
      # ---- Dynamic filename depending on selected table ----
      filename = function() {
        
        switch(input$table_data,
               "map" = paste0("Cattle_Agricultural_Region_Data_", Sys.Date(), ".csv"),
               "timeseries" = paste0("Cattle_Timeseries_Data_", Sys.Date(), ".csv"),
               # "map_con" = paste0("Cattle_Constituency_Data_", Sys.Date(), ".csv"),
               "map_uni" = paste0("Cattle_Local_Authority_Data_", Sys.Date(), ".csv"),
               
               # fallback
               paste0("Downloaded_Data_", Sys.Date(), ".csv")
        )
      },
      
      # ---- Write selected dataset to disk ----
      content = function(file) {
        
        data <- switch(input$table_data,
                       
                       # ---- Agricultural region map ----
                       "map" = {
                         cattle_region_table %>%
                           filter(`Livestock by category` == input$map_variable)
                       },
                       
                       # ---- Timeseries ----
                       "timeseries" = {
                         cattle_timeseries_table
                       },
                       
                       # ---- Constituency ----
                       # "map_con" = {
                       #   cattle_constituency
                       # },
                        
                       # ---- Local authority ----
                       "map_uni" = {
                         cattle_unitauth_table
                       }
        )
        
        write.csv(data, file, row.names = FALSE)
      }
    )
  }
  )
}


cattle_demo <- function() {
  ui <- fluidPage(cattleUI("cattle_test"))
  server <- function(input, output, session) {
    cattleServer("cattle_test")
  }
  shinyApp(ui, server)
}

cattle_demo()
