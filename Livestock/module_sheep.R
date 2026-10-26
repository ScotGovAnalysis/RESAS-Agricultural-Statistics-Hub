# File: module_sheep.R
sheepUI <- function(id) {
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
            "Total sheep" = "Total sheep",
            "Ewes used for breeding in previous season" = "Ewes used for breeding in previous season",
            "Other sheep for breeding aged 1 year and over" = "Other sheep for breeding aged 1 year and over",
            "Rams to be used for service" = "Rams to be used for service",
            "Lambs" = "Lambs"
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
      #       "Total sheep" = "Total sheep",
      #       "Ewes used for breeding in previous season" = "Ewes used for breeding in previous season",
      #       "Other sheep for breeding aged 1 year and over" = "Other sheep for breeding aged 1 year and over",
      #       "Rams to be used for service" = "Rams to be used for service",
      #       "Lambs" = "Lambs"
      #     )
      #   )
      # )
      # ,
      
      # Time series
      conditionalPanel(
        condition = "input.tabsetPanel === 'Time Series'",
        ns = ns,
        checkboxGroupInput(
          ns("timeseries_variables"),
          "Select Time Series Variables",
          choices = unique(sheep_timeseries_chart$`Sheep by category`),
          selected = c(
            "Ewes used for breeding in previous season",
            "Other sheep for breeding aged 1 year and over",
            "Rams to be used for service",
            "Total other sheep 1 year and over",
            "Lambs"
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
        tabPanel("Maps",
          conditionalPanel(
            condition = sprintf("input['%s'] == 'region'", ns("map_region")),
            mapUI(ns("map"))),
          conditionalPanel(
            condition = sprintf("input['%s'] == 'la'", ns("map_region")),
            mapUnitaryUI(ns("map_uni")))
        ),
        # tabPanel("Constituency Map", mapConstituenciesUI(ns("map_con"))),
        tabPanel("Time Series", lineChartUI(ns("line"))),
        tabPanel("Data Table", 
                 DTOutput(ns("table")),
                 downloadButton(ns("downloadData"), "Download Data"),
                 generateCensusTableFooter()

        )
      )
    )
  )
}

sheepServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    mapServer(
      id = "map",
      data = reactive({
        req(input$map_variable)
        sheep_region_map %>% filter(`Livestock by category` == input$map_variable)
      }),
      footer = census_footer,
      variable = reactive(input$map_variable),
      title = paste("Sheep distribution by region in Scotland in", census_year),
      legend_title = "Number of sheep"
    )
    
    # # Constituency map
    # sheep_const_map <- reactive({
    #   sheep_constituency %>%       
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
    #     sheep_const_map() %>% filter(`livestock` == input$variable_con)
    #   }),
    #   unit = "number",
    #   footer = census_footer,
    #   variable = reactive(input$variable_con),
    #   title = paste("Sheep distribution by 2026 Scottish Parliamentary Constituency"),
    #   legend_title = "Sheep (number)"
    # )

    mapUnitaryServer(
      id = "map_uni",
      data = reactive({
        req(input$map_variable)
        sheep_unitauth_map %>% filter(`livestock` == input$map_variable)
      }),
      unit = "number",
      footer = census_footer,
      variable = reactive(input$map_variable),
      title = paste("Sheep distribution by local authority", census_year),
      legend_title = "Sheep (number)"
    )
    
    # Time series
    chart_data <- reactive({
      req(input$timeseries_variables)
      filtered_data <- sheep_timeseries_chart %>% 
        filter(`Sheep by category` %in% input$timeseries_variables)
      filtered_data
    })
    
    lineChartServer(
      id = "line",
      chart_data = chart_data,
      title = "Number of sheep by category across time",
      yAxisTitle = "Number of sheep (1,000)",
      xAxisTitle = "Year",
      footer = census_footer,
      x_col = "year",
      y_col = "value"
    )
    
    output$table <- renderDT({
      req(input$tabsetPanel == "Data Table")
      
      # Choose which dataset to display
      data <- switch(input$table_data,
                    
                    # Agricultural region table
                    "map" = {
                      req(input$map_variable)
                      sheep_region_table %>% 
                        mutate(across(where(is.numeric), comma))
                    },
                    
                    # Timeseries table
                    "timeseries" = {
                      sheep_timeseries_table %>% 
                        mutate(across(where(is.numeric), comma))
                    },
                    
                    # Constituency Table
                    # "map_con" = {
                    #   sheep_constituency %>%
                    #     rename(`Sheep by category` = `livestock`) %>%
                    #     mutate(across(where(is.numeric), comma))
                    # },

                    # Local authority table
                    "map_uni" = {
                      sheep_unitauth_table %>% 
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
      
      # Dynamic filename depending on selected table
      filename = function() {
        switch(input$table_data,
               "map" = paste0("Sheep_Map_Data_", Sys.Date(), ".csv"),
               "timeseries" = paste0("Sheep_Timeseries_Data_", Sys.Date(), ".csv"),
               # "map_con" = paste0("Sheep_Constituency_Data_", Sys.Date(), ".csv"),
               "map_uni" = paste0("Sheep_Local_Authority_Data_", Sys.Date(), ".csv"),
               
               # fallback
               paste0("Downloaded_Data_", Sys.Date(), ".csv")
        )
      },
      
      # ---- Write selected dataset to disk ----
      content = function(file) {
        
        data <- switch(input$table_data,
                       
                       # ---- Agricultural region  ----
                       "map" = {
                         sheep_region_table %>%
                           filter(`Livestock by category` == input$map_variable)
                       },
                       
                       # ---- Timeseries ----
                       "timeseries" = {
                         sheep_timeseries_table
                       },
                       
                       # ---- Constituency ----
                       # "map_con" = {
                       #   sheep_constituency
                       # },
                        
                       # ---- Local authority ----
                       "map_uni" = {
                         sheep_unitauth_table
                       }
        )
        
        write.csv(data, file, row.names = FALSE)
      }
    )
  }
)
}


sheep_demo <- function() {
  ui <- fluidPage(sheepUI("sheep_test"))
  server <- function(input, output, session) {
    sheepServer("sheep_test")
  }
  shinyApp(ui, server)
}

sheep_demo()
