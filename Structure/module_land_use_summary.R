landUseSummaryUI <- function(id) {
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
          choices = unique(land_use_region_table$`Land use by category`)
        )
      ),
      
      # Constituency map
      # conditionalPanel(
      #   condition = "input.tabsetPanel === 'Constituency Map'",
      #   ns = ns,
      #   radioButtons(
      #     ns("variable_con"),
      #     "Select Variable",
      #     choices = unique(land_use_constituency$`land use`)
      #   )
      # ),

      # Summary
      conditionalPanel(
        condition = "input.tabsetPanel === 'Summary'",
        ns = ns,
        div(
          style = "font-size: 24px; font-weight: bold;",
          " "
        )
      ), 

      # Time series
      conditionalPanel(
        condition = "input.tabsetPanel === 'Time Series'",
        ns = ns,
        checkboxGroupInput(
          ns("timeseries_variables"),
          "Select Time Series Variables",
          choices = unique(land_use_timeseries_table$`Crop/land use`),
          selected = unique(land_use_timeseries_table$`Crop/land use`)
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
      id = ns("mainpanel"),
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
                 tags$div(
                   style = "margin-top: 20px;",
                   downloadButton(ns("download_data"), "Download Data"),
                   generateCensusTableFooter()
                 )
        )
      )
    )
  )
}


landUseSummaryServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Agricultural region map server
    mapServer(
      id = "map",
      data = reactive({
        req(input$map_variable)
        land_use_region_map %>% filter(`Land use by category` == input$map_variable)
      }),
      unit = "hectares",
      footer = census_footer,
      variable = reactive(input$map_variable),
      title = paste("Land use by region in Scotland in", census_year),
      legend_title = "Area (hectares)"
    )
    
    
    # Constituency map
    # land_use_const_map <- reactive({
    #   land_use_constituency %>%   
    #     mutate(across(everything(), as.character)) %>%
    #     pivot_longer(
    #       cols = -`land use`,
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
    #     land_use_const_map() %>% filter(`land use` == input$variable_con)
    #   }),
    #   unit = "hectares",
    #   footer = census_footer,
    #   variable = reactive(input$variable_con),
    #   title = paste("Land use by 2026 Scottish Parliamentary Constituency"),
    #   legend_title = "Area (hectares)"
    # )
    
    # Local authority map server
    mapUnitaryServer(
      id = "map_uni",
      data = reactive({
        req(input$map_variable)
        land_use_unitauth_map %>% filter(`land use` == input$map_variable)
      }),
      unit = "hectares",
      footer = census_footer,
      variable = reactive(input$map_variable),
      title = paste("Land use by local authority in", census_year),
      legend_title = "Area (hectares)"
    )
    
    # Time series chart server
    timeseries_data <- reactive({
      req(input$timeseries_variables)
      land_use_timeseries_chart %>%
        filter(`Crop/land use` %in% input$timeseries_variables)
    })

    lineChartServer(
      id = "line",
      chart_data = timeseries_data,
      title = "Land use over time",
      yAxisTitle = "Area of land use (1,000 hectares)",
      xAxisTitle = "Year",
      unit = "hectares",
      footer = census_footer,
      x_col = "year",
      y_col = "value"
    )
    
    
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
                       land_use_region_table %>%
                         mutate(across(where(is.numeric), comma))
                     },
                     
                     # -------------------
                     # 2. Timeseries data
                     # -------------------
                     "timeseries" = {
                       land_use_timeseries_table %>%
                         mutate(across(where(is.numeric), comma))
                     },
                     
                     # -------------------
                     # 3. Constituency Table
                     # -------------------
                     # "map_con" = {
                     #   land_use_constituency %>%
                     #     rename(`Land use by category` = `land use`) %>%
                     #     mutate(across(where(is.numeric), comma))
                     # },

                     # -------------------
                     # 4. Local authority table
                     # -------------------
                     "map_uni" = {
                       land_use_unitauth_table %>%
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
               "map" = paste0("Land_Use_Agricultural_Region_Map_Data_", Sys.Date(), ".csv"),
               "timeseries" = paste0("Land_Use_Timeseries_Data_", Sys.Date(), ".csv"),
               # "map_con" = paste0("Land_Use_Constituency_Data_", Sys.Date(), ".csv"),
               "map_uni" = paste0("Land_Use_Local_Authority_Data_", Sys.Date(), ".csv"),
               
               # fallback
               paste0("Downloaded_Data_", Sys.Date(), ".csv")
        )
      },
      
      # ---- Write selected dataset to disk ----
      content = function(file) {
        
        data <- switch(input$table_data,
                       
                       # ---- Agricultural region map ----
                       "map" = {
                         land_use_region_table %>%
                           filter(`Land use by category` == input$map_variable)
                       },
                       
                       # ---- Time series ----
                       "timeseries" = {
                         land_use_timeseries_table
                       },
                       
                       # ---- Constituency ----
                       # "map_con" = {
                       #   land_use_constituency
                       # },
                        
                       # ---- Local authority ----
                        "map_uni" = {
                          land_use_unitauth_table
                        }
        )
        
        write.csv(data, file, row.names = FALSE)
      }
    )
  }
  )
}


land_use_demo <- function() {
  ui <- fluidPage(landUseSummaryUI("land_use_test"))
  server <- function(input, output, session) {
    landUseSummaryServer("land_use_test")
  }
  shinyApp(ui, server)
}

land_use_demo()
