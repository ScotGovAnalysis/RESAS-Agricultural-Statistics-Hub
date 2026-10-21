# File: module_fruit.R
fruit_data <- fruit_data %>%
  mutate(across(c(`2023`, `2024`, `2026`), ~ round(.x, 0)))
fruitUI <- function(id) {
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
          choices = unique(fruit_subregion$`Land use by category`)
        )
      ),
      
      # Constituency map
      # conditionalPanel(
      #   condition = "input.tabsetPanel === 'Constituency Map'",
      #   ns = ns,
      #   radioButtons(
      #     ns("variable_con"), 
      #     "Select Variable", 
      #     choices = unique(fruit_constituency$crop)
      #   )
      # ),    

      # Time series
      conditionalPanel(
        condition = "input.tabsetPanel === 'Time Series'",
        ns = ns,
        selectizeInput(
          ns("timeseries_variables"),
          "Click within the box to select variables",
          choices = unique(fruit_data$`Vegetables and fruits for human consumption`),
          selected = c(
            "Strawberries grown in open/under cover",
            "Raspberries grown in open/under cover",
            "Blackcurrants grown in open/under cover",
            "Blueberries grown in open/under cover",
            "Tomatoes grown in open/under cover",
            "Other Fruit grown in open/under cover"
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
                 downloadButton(ns("download_data"), "Download Data"),
                 generateCensusTableFooter()

        )
      )
    )
  )
}

fruitServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    fruit_map <- fruit_subregion %>%
      select(-`Scotland total`) %>%
      mutate(across(everything(), as.character)) %>%
      pivot_longer(cols = -`Land use by category`, names_to = "sub_region", values_to = "value") %>%
      mutate(value = as.numeric(value))
    
    mapServer(
      id = "map",
      data = reactive({
        req(input$map_variable)
        fruit_map %>% filter(`Land use by category` == input$map_variable)
      }),
      unit = "hectares",
      footer = census_footer,
      variable = reactive(input$map_variable),
      title = paste("Fruit distribution by region in Scotland in", census_year),
      legend_title = "Area (hectares)"
    )
    
    
    # ===================== CONSTITUENCY MAP =====================
    # fruit_const_map <- reactive({
    #   fruit_constituency %>%        
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
    #     fruit_const_map() %>% filter(`crop` == input$variable_con)
    #   }),
    #   unit = "hectares",
    #   footer = census_footer,
    #   variable = reactive(input$variable_con),
    #   title = paste("Fruit crops distribution by 2026 Scottish Parliamentary Constituency"),
    #   legend_title = "Area (hectares)"
    # )
    
    # ===================== LOCAL AUTHORITY MAP =====================
    fruit_uni_map <- reactive({
      fruit_unitauth %>%
        mutate(across(everything(), as.character)) %>%
        pivot_longer(
          cols = -`crop`,
          names_to = "unitauth",
          values_to = "value"
        ) %>%
        mutate(
          value = if_else(is.na(value), NA_real_, as.numeric(value))
        )
    })

    mapUnitaryServer(
      id = "map_uni",
      data = reactive({
        req(input$map_variable)
        fruit_uni_map() %>% filter(`crop` == input$map_variable)
      }),
      unit = "hectares",
      footer = census_footer,
      variable = reactive(input$map_variable),
      title = paste("Fruit crops distribution by local authority in", census_year),
      legend_title = "Area (hectares)"
    )
    
    # ===================== TIME SERIES =====================
    chart_data <- reactive({
      req(input$timeseries_variables)
      filtered_data <- fruit_data %>%
        filter(`Vegetables and fruits for human consumption` %in% input$timeseries_variables) %>%
        pivot_longer(cols = -`Vegetables and fruits for human consumption`, names_to = "year", values_to = "value") %>%
        mutate(year = as.numeric(year))  # Ensure year is numeric
      filtered_data
    })
    
    lineChartServer(
      id = "line",
      chart_data = chart_data,
      title = "Area used to grow fruit across time",
      yAxisTitle = "Area of fruit (hectares)",
      xAxisTitle = "Year",
      unit = "hectares",
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
                       fruit_map %>%
                         pivot_wider(names_from = sub_region, values_from = value) %>%
                         mutate(across(where(is.numeric), comma))
                     },
                     
                     # -------------------
                     # 2. Timeseries data
                     # -------------------
                     "timeseries" = {
                       fruit_data %>%
                         pivot_longer(cols = -`Vegetables and fruits for human consumption`,
                                      names_to = "year",
                                      values_to = "value") %>%
                         pivot_wider(names_from = year, values_from = value) %>%
                         mutate(across(where(is.numeric) & !contains("Year"), comma))
                     },
                     
                     # -------------------
                     # # 3. Constituency Table
                     # # -------------------
                     # "map_con" = {
                     #   fruit_constituency %>%
                     #     rename(`Land use by category` = `crop`) %>%
                     #     mutate(across(where(is.numeric), comma))
                     # },
                     # 
                     
                     # -------------------
                     # 4. Local authority table
                     # -------------------
                     "map_uni" = {
                       fruit_unitauth %>%
                         rename(`Land use by category` = `crop`) %>%
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
               "map" = paste0("Fruit_Agricultural_Region_Data_", Sys.Date(), ".csv"),
               "timeseries" = paste0("Fruit_Timeseries_Data_", Sys.Date(), ".csv"),
               # "map_con" = paste0("Fruit_Constituency_Data_", Sys.Date(), ".csv"),
               "map_uni" = paste0("Fruit_Local_Authority_Data_", Sys.Date(), ".csv"),
               
               # fallback
               paste0("Downloaded_Data_", Sys.Date(), ".csv")
        )
      },
      
      # ---- Write selected dataset to disk ----
      content = function(file) {
        
        data <- switch(input$table_data,
                       
                       # ---- Agricultural region map ----
                       "map" = {
                         fruit_subregion %>%
                           filter(`Land use by category` == input$map_variable) # %>%
                        #   pivot_wider(names_from = sub_region, values_from = value)
                       },
                       
                       # ---- Timeseries ----
                       "timeseries" = {
                         fruit_data %>%
                           pivot_longer(
                             cols = -`Vegetables and fruits for human consumption`,
                             names_to = "year",
                             values_to = "value"
                           ) #%>%
                          # pivot_wider(names_from = year, values_from = value)
                       },
                       
                       # # ---- Constituency ----
                       # "map_con" = {
                       #   fruit_constituency
                       # },
                       # 
                       # ---- Local authority ----
                       "map_uni" = {
                         fruit_unitauth
                       }
        )
        
        write.csv(data, file, row.names = FALSE)
      }
    )
  }
  )
}

fruit_demo <- function() {
  ui <- fluidPage(fruitUI("fruit_test"))
  server <- function(input, output, session) {
    fruitServer("fruit_test")
  }
  shinyApp(ui, server)
}

fruit_demo()