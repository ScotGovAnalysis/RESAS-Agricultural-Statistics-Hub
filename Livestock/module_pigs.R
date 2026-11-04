# File: module_pigs.R

pigsUI <- function(id) {
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
            "Total pigs" = "Total pigs",
            "Female pigs breeding herd" = "Female pigs breeding herd",
            "All other non-breeding pigs" = "All other non-breeding pigs"
          )
        )
      ),

      # conditionalPanel(
      #   condition = "input.tabsetPanel === 'Constituency Map'",
      #   ns = ns,
      #   radioButtons(
      #     ns("variable_con"), 
      #     "Select Variable", 
      #     choices = c(
      #       "Total pigs" = "Total pigs",
      #       "Female pigs breeding herd" = "Female pigs breeding herd",
      #       "All other non-breeding pigs" = "All other non-breeding pigs"
      #     ))
      # ),

      # Time series
      conditionalPanel(
        condition = "input.tabsetPanel === 'Time Series'",
        ns = ns,
        selectizeInput(
          ns("timeseries_variables"),
          "Click within the box to select variables (not all shown)",
          choices = unique(pigs_timeseries_chart$`Pigs by category`),
          selected = c(
            "Total breeding herd",
            "80kg liveweight and over",
            "50kg and under 80kg liveweight",
            "Under 50kg liveweight"
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

pigsServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Agricultural region map server
    mapServer(
      id = "map",
      data = reactive({
        req(input$map_variable)
        pigs_region_map %>% 
          filter(`Livestock by category` == input$map_variable)
      }),
      footer = census_footer,
      variable = reactive(input$map_variable),
      title = paste("Pig distribution by region in Scotland in", census_year),
      legend_title = "Number of pigs"
    )
    
    # pig_const_map <- reactive({
    #   pigs_constituency %>%   
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
    #     pig_const_map() %>% filter(`livestock` == input$variable_con)
    #   }),
    #   unit = "number",
    #   footer = census_footer,
    #   variable = reactive(input$variable_con),
    #   title = paste("Pig distribution by 2026 Scottish Parliamentary Constituency"),
    #   legend_title = "Pigs (number)"
    # )
    
    # Local authority map server
    mapUnitaryServer(
      id = "map_uni",
      data = reactive({
        req(input$map_variable)
        pigs_unitauth_map %>% 
          filter(`livestock` == input$map_variable)
      }),
      unit = "number",
      footer = census_footer,
      variable = reactive(input$map_variable),
      title = paste("Pig distribution by Local Authority in", census_year),
      legend_title = "Pigs (number)"
    )

    chart_data <- reactive({
      req(input$timeseries_variables)
      filtered_data <- pigs_timeseries_chart %>%
        filter(`Pigs by category` %in% input$timeseries_variables)
      filtered_data
    })
    
    lineChartServer(
      id = "line",
      chart_data = chart_data,
      title = "Number of pigs by category over time",
      yAxisTitle = "Number of pigs (1,000)",
      xAxisTitle = "Year",
      footer = census_footer,
      x_col = "year",
      y_col = "value"
    )
    
    output$table <- renderDT({
      req(input$tabsetPanel == "Data Table")
      
      # Choose which dataset to display
      data <- switch(input$table_data,
                     
                     # ---- Agricultural Region Table ----
                     "map" = {
                       req(input$map_variable)
                       pigs_region_table %>%
                         mutate(across(where(is.numeric), comma))
                     },
                     
                     # ---- Timeseries Table ----
                     "timeseries" = {
                       pigs_timeseries_table %>%
                         mutate(across(where(is.numeric), comma))
                     },
                     
                     # # ---- Constituency Table ----
                     # "map_con" = {
                     #   pigs_constituency %>%
                     #     rename(`Pigs by category` = `livestock`) %>%
                     #     mutate(across(where(is.numeric), comma))
                     # },
                     
                     # ---- Local authority table ----
                     "map_uni" = {
                       pigs_unitauth_table %>%
                         mutate(across(where(is.numeric), comma))
                     }
      )
      
      # Render the chosen table
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
               "map" = paste0("Pigs_Map_Data_", Sys.Date(), ".csv"),
               "timeseries" = paste0("Pigs_Timeseries_Data_", Sys.Date(), ".csv"),
               # "map_con" = paste0("Pigs_Constituency_Data_", Sys.Date(), ".csv"),
               "map_uni" = paste0("Pigs_Local_Authority_Data_", Sys.Date(), ".csv"),
               
               # fallback
               paste0("Downloaded_Data_", Sys.Date(), ".csv")
        )
      },
      
      # ---- Write selected dataset to disk ----
      content = function(file) {
        
        data <- switch(input$table_data,
                       
                       # ---- Agricultural region map ----
                       "map" = {
                         pigs_region_table %>%
                           filter(`Livestock by category` == input$map_variable)
                       },
                       
                       # ---- Timeseries ----
                       "timeseries" = {
                         pigs_timeseries_table
                       },
                       
                       # # ---- Constituency map ----
                       # "map_con" = {
                       #    pigs_constituency
                       # },
                       
                       # ---- Local authority map ----
                       "map_uni" = {
                         pigs_unitauth_table
                       }
        )
        
        write.csv(data, file, row.names = FALSE)
      }
    )
  }
  )
}


#Testing module
pigs_demo <- function() {
  ui <- fluidPage(pigsUI("pigs_test"))
  server <- function(input, output, session) {
    pigsServer("pigs_test")
  }
  shinyApp(ui, server)
}

pigs_demo()
