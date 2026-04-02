# File: module_fruit.R

fruitUI <- function(id) {
  ns <- NS(id)
  sidebarLayout(
    sidebarPanel(
      width = 3,
      conditionalPanel(
        condition = "input.tabsetPanel === 'Agricultural Region Map'",
        ns = ns,
        radioButtons(
          ns("variable_region"), 
          "Select Variable", 
          choices = unique(fruit_subregion$`Land use by category`)
        )
      ),
      
      conditionalPanel(
        condition = "input.tabsetPanel === 'Constituency Map'",
        ns = ns,
        radioButtons(
          ns("variable_con"), 
          "Select Variable", 
          choices = unique(fruit_constituency$crop)
        )
      ),    
      
      conditionalPanel(
        condition = "input.tabsetPanel === 'Local Authority Map'",
        ns = ns,
        radioButtons(
          ns("variable_uni"), 
          "Select Variable", 
          choices = unique(fruit_constituency$crop)
        )
      ),   
      
      conditionalPanel(
        condition = "input.tabsetPanel === 'Time Series' || input.tabsetPanel === 'Area Chart'",
        ns = ns,
        selectizeInput(
          ns("timeseries_variables"),
          "Click within the box to select variables",
          choices = unique(fruit_data$`Vegetables and fruits for human consumption`),
          selected = c(
            "Strawberries Grown In Open/Under Cover",
            "Raspberries Grown In Open/Under Cover",
            "Blackcurrants Grown In Open/Under Cover",
            "Blueberries Grown In Open/Under Cover",
            "Tomatoes Grown In Open/Under Cover",
            "Other Fruit Grown In Open/Under Cover"
          ),
          multiple = TRUE,
          options = list(
            plugins = list('remove_button')
          )
        )
      ),
      conditionalPanel(
        condition = "input.tabsetPanel === 'Data Table'",
        ns = ns,
        radioButtons(
          ns("table_data"),
          "Select Data to Display",
          choices = c("Agricultural Region Data" = "map",
                      "Time Series Data" = "timeseries",
                      "Constituency Data" = "map_con",
                      "Local Authority Data" = "map_uni"),
          selected = "map"
        )
      )
    ),
    mainPanel(
      width = 9,
      tabsetPanel(
        id = ns("tabsetPanel"),
        tabPanel("Agricultural Region Map", mapUI(ns("map"))),
        tabPanel("Constituency Map", mapConstituenciesUI(ns("map_con"))),
        tabPanel("Local Authority Map", mapUnitaryUI(ns("map_uni"))),
        tabPanel("Time Series", lineChartUI(ns("line"), note_type = 2)),
        tabPanel("Area Chart", areaChartUI(ns("area"), note_type = 2)),
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
        req(input$variable_region)
        fruit_map %>% filter(`Land use by category` == input$variable_region)
      }),
      unit = "hectares",
      footer = census_footer,
      variable = reactive(input$variable_region),
      title = paste("Fruit distribution by region in Scotland in", census_year),
      legend_title = "Area (hectares)"
    )
    
    
    # ===================== CONSTITUENCY MAP =====================
    fruit_const_map <- reactive({
      fruit_constituency %>%        
        mutate(across(everything(), as.character)) %>%
        pivot_longer(
          cols = -`crop`,
          names_to = "constituency",
          values_to = "value"
        ) %>% 
        mutate(
          value = if_else(is.na(value), NA_real_, as.numeric(value))
        )
    })
    
    mapConstituenciesServer(
      id = "map_con",
      data = reactive({
        req(input$variable_con)
        fruit_const_map() %>% filter(`crop` == input$variable_con)
      }),
      unit = "hectares",
      footer = census_footer,
      variable = reactive(input$variable_con),
      title = paste("Fruit crops distribution by 2026 Scottish Parliamentary Constituency"),
      legend_title = "Area (hectares)"
    )
    
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
        req(input$variable_uni)
        fruit_uni_map() %>% filter(`crop` == input$variable_uni)
      }),
      unit = "hectares",
      footer = census_footer,
      variable = reactive(input$variable_uni),
      title = paste("Fruit crops distribution by local authority in", census_year),
      legend_title = "Area (hectares)"
    )
    
    # ===================== AREA CHART =====================
    chart_data <- reactive({
      req(input$timeseries_variables)
      filtered_data <- fruit_data %>%
        filter(`Vegetables and fruits for human consumption` %in% input$timeseries_variables) %>%
        pivot_longer(cols = -`Vegetables and fruits for human consumption`, names_to = "year", values_to = "value") %>%
        mutate(year = as.numeric(year))  # Ensure year is numeric
      filtered_data
    })
    
    areaChartServer(
      id = "area",
      chart_data = chart_data,
      title = "Area used to grow fruit across time",
      yAxisTitle = "Area of fruit (hectares)",
      xAxisTitle = "Year",
      unit = "hectares",
      footer = census_footer,
      x_col = "year",
      y_col = "value"
    )
    
    # ===================== LINE CHART =====================
    
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
                         mutate(across(where(is.numeric) & !contains("Year"), comma))
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
                     # 3. Constituency Table
                     # -------------------
                     
                     "map_con" = {
                       fruit_constituency %>%
                         rename(`Land use by category` = `crop`) %>%
                         mutate(across(
                           where(is.character),
                           ~ ifelse(grepl("^\\d+$", .x), scales::comma(as.numeric(.x)), .x)
                         )) %>%
                  
                         mutate(
                           across(
                             where(is.numeric),
                             ~ round(.x, 0)
                           )
                         ) %>%
                         
                         
                         mutate(
                           across(
                             where(is.character),
                             ~ ifelse(
                               grepl("^\\d+(\\.\\d+)?$", .x),   # matches integers or decimals
                               as.character(round(as.numeric(.x), 0)),
                               .x
                             )
                           )
                         )
                       
                     },
                     
                     
                     # -------------------
                     # 4. Local authority table
                     # -------------------
                     "map_uni" = {
                       fruit_unitauth %>%
                         rename(`Land use by category` = `crop`) %>%
                         mutate(across(
                           where(is.character),
                           ~ ifelse(grepl("^\\d+$", .x), scales::comma(as.numeric(.x)), .x)
                         )) %>%
                         
                         mutate(
                           across(
                             where(is.numeric),
                             ~ round(.x, 0)
                           )
                         ) %>%
                         
                         
                         mutate(
                           across(
                             where(is.character),
                             ~ ifelse(
                               grepl("^\\d+(\\.\\d+)?$", .x),   # matches integers or decimals
                               as.character(round(as.numeric(.x), 0)),
                               .x
                             )
                           )
                         )
                       
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
               
               "map_con" = paste0("Fruit_Constituency_Data_", Sys.Date(), ".csv"),
               
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
                           filter(`Land use by category` == input$variable_region) # %>%
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
                       
                       # ---- Constituency ----
                       "map_con" = {
                         fruit_constituency
                       },
                       
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