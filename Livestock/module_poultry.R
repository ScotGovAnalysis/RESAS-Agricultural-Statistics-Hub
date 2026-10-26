# File: module_poultry.R
poultryUI <- function(id) {
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
          choices = c(
            "Total poultry" = "Total poultry",
            "Fowls for producing eggs" = "Fowls for producing eggs",
            "Fowls for breeding" = "Fowls for breeding",
            "Broilers, other table fowls and other poultry" =
              "Broilers, other table fowls and other poultry"
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
      #       "Total poultry" = "Total poultry",
      #       "Fowls for producing eggs" = "Fowls for producing eggs",
      #       "Fowls for breeding" = "Fowls for breeding",
      #       "Broilers and other table fowls and other poultry" = "Broilers and other table fowls and other poultry"
      #     )
      #   )
      # ),

      # Time series
      conditionalPanel(
        condition = "input.tabsetPanel === 'Time Series'",
        ns = ns,
        selectizeInput(
          ns("timeseries_variables"),
          "Click within the box to select variables",
          choices = unique(poultry_timeseries_chart$`Poultry by category`),
          selected = c(
            "Total fowls for producing eggs",
            "Total fowls for breeding",
            "Broilers and other table birds",
            "Total poultry"
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
                      #"Constituency Data" = "map_con",
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
        tabPanel("Time Series", lineChartUI(ns("line"), note_type = 2)),
        tabPanel("Data Table", 
                 DTOutput(ns("table")),
                 downloadButton(ns("downloadData"), "Download Data"),
                 generateCensusTableFooter()
        )
      ),
      div(
        style = "margin-top: 20px; padding: 10px; border-top: 1px solid #ddd;",
        HTML("<strong>Note:</strong> Poultry estimates for 2023 are not comparable to previous years due to methodological improvements.")
      )
    )
  )
}


poultryServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Agricultural region map server
    mapServer(
      id = "map",
      data = reactive({
        req(input$map_variable)
        poultry_region_map %>% filter(`Livestock by category` == input$map_variable)
      }),
      footer = census_footer,
      variable = reactive(input$map_variable),
      title = paste("Poultry distribution by region in Scotland in", census_year),
      legend_title = "Number of poultry"
    )
    
    # poultry_const_map <- reactive({
    #   poultry_constituency %>%         # <— your constituency land use table
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
    # 
    # mapConstituenciesServer(
    #   id = "map_con",
    #   data = reactive({
    #     req(input$variable_con)
    #     poultry_const_map() %>% filter(`livestock` == input$variable_con)
    #   }),
    #   unit = "number",
    #   footer = census_footer,
    #   variable = reactive(input$variable_con),
    #   title = paste("Poultry distribution by 2026 Scottish Parliamentary Constituency"),
    #   legend_title = "Poultry (number)"
    # )

    # Local authority map server
    mapUnitaryServer(
      id = "map_uni",
      data = reactive({
        req(input$map_variable)
        poultry_unitauth_map %>% filter(`livestock` == input$map_variable)
      }),
      unit = "number",
      footer = census_footer,
      variable = reactive(input$map_variable),
      title = paste("Poultry distribution by Local authority in", census_year),
      legend_title = "Poultry (number)"
    )
    
    # Time series chart server
    chart_data <- reactive({
      req(input$timeseries_variables)
      filtered_data <- poultry_timeseries_chart %>%
        filter(`Poultry by category` %in% input$timeseries_variables)
      filtered_data
    })
    
    lineChartServer(
      id = "line",
      chart_data = chart_data,
      title = "Number of poultry by category across time",
      yAxisTitle = "Number of poultry (1,000)",
      xAxisTitle = "Year",
      footer = '<div style="font-size: 16px; font-weight: bold;">* Estimates for 2023 are not comparable to previous years due to methodological improvements.<br/><a href="https://www.gov.scot/publications/results-scottish-agricultural-census-june-2025/documents/">Source: Scottish Agricultural Census: June 2025</a></div>',
      x_col = "year",
      y_col = "value"
    )
    
    # Data Table output
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
                       poultry_region_table %>%
                         mutate(across(where(is.numeric), comma))
                     },
                     
                     # -------------------
                     # 2. Timeseries data
                     # -------------------
                     "timeseries" = {
                       poultry_timeseries_table %>%
                         mutate(across(where(is.numeric), comma))
                     },
                     
                     # -------------------
                     # 3. Constituency Table
                     # -------------------
                     # "map_con" = {
                     #   poultry_constituency %>%
                     #     rename(`Poultry by category` = `livestock`) %>%
                     #     mutate(across(where(is.numeric), comma))
                     # },
                     
                     # -------------------
                     # 4. Local authority table
                     # -------------------
                     "map_uni" = {
                       poultry_unitauth_table %>%
                         mutate(across(where(is.numeric), comma))
                     }
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
               "map" = paste0("Poultry_Map_Data_", Sys.Date(), ".csv"),
               "timeseries" = paste0("Poultry_Timeseries_Data_", Sys.Date(), ".csv"),
               # "map_con" = paste0("Poultry_Constituency_Data_", Sys.Date(), ".csv"),
               "map_uni" = paste0("Poultry_Local_Authority_Data_", Sys.Date(), ".csv"),
               
               # fallback
               paste0("Downloaded_Data_", Sys.Date(), ".csv")
        )
      },
      
      # ---- Write selected dataset to disk ----
      content = function(file) {
        
        data <- switch(input$table_data,
                       
                       # ---- Agricultural region map ----
                       "map" = {
                         poultry_region_table %>%
                           filter(`Livestock by category` == input$map_variable)
                       },
                       
                       # ---- Timeseries ----
                       "timeseries" = {
                         poultry_timeseries_table
                       },
                       
                       # # ---- Constituency ----
                       # "map_con" = {
                       #   poultry_constituency
                       # },
                       
                       # ---- Local authority ----
                       "map_uni" = {
                         poultry_unitauth_table
                       }
        )
        
        write.csv(data, file, row.names = FALSE)
      }
    )
  }
  )
}

# Testing module
poultry_demo <- function() {
  ui <- fluidPage(poultryUI("poultry_test"))
  server <- function(input, output, session) {
    poultryServer("poultry_test")
  }
  shinyApp(ui, server)
}

poultry_demo()