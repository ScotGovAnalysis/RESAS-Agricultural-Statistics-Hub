# File: module_potatoes.R

potatoesUI <- function(id) {
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
          choices = unique(potatoes_subregion$`Land use by category`)
        )
      ),
      # ===================== CONSTITUENCY MAP =====================
      conditionalPanel(
        condition = "input.tabsetPanel === 'Constituency Map'",
        ns = ns,
        radioButtons(
          ns("variable_con"), 
          "Select Variable", 
          choices = unique(potatoes_constituency$crop)
        )
      ),
      
      # ===================== LOCAL AUTHORITY MAP =====================
      conditionalPanel(
        condition = "input.tabsetPanel === 'Local Authority Map'",
        ns = ns,
        radioButtons(
          ns("variable_uni"), 
          "Select Variable", 
          choices = unique(potatoes_unitauth$crop)
        )
      ),
      
      conditionalPanel(
        condition = "input.tabsetPanel === 'Time Series' || input.tabsetPanel === 'Area Chart'",
        ns = ns,
        checkboxGroupInput(
          ns("timeseries_variables"),
          "Select Time Series Variables",
          choices = unique(potatoes_data$`Crop/Land use`),
          selected = c(
            "Ware Potatoes",
            "Seed Potatoes"
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
        tabPanel("Time Series", lineChartUI(ns("line"))),
        tabPanel("Area Chart", areaChartUI(ns("area"))),
        tabPanel("Data Table", 
                 DTOutput(ns("table")),
                 downloadButton(ns("download_data"), "Download Data"),
                  generateCensusTableFooter()
        )
      )
    )
  )
}

potatoesServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    potatoes_map <- potatoes_subregion %>%
      select(-`Scotland total`) %>%
      mutate(across(everything(), as.character)) %>%
      pivot_longer(cols = -`Land use by category`, names_to = "sub_region", values_to = "value") %>%
      mutate(value = as.numeric(value))
    
    mapServer(
      id = "map",
      data = reactive({
        req(input$variable_region)
        potatoes_map %>% filter(`Land use by category` == input$variable_region)
      }),
      unit = "hectares",
      footer = census_footer,
      variable = reactive(input$variable_region),
      title = paste("Potatoes distribution by region in Scotland in", census_year),
      legend_title = "Area (hectares)"
    )
    
    # ===================== CONSTITUENCY MAP =====================
    potato_const_map <- reactive({
      potatoes_constituency %>%       
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
        potato_const_map() %>% filter(`crop` == input$variable_con)
      }),
      unit = "hectares",
      footer = census_footer,
      variable = reactive(input$variable_con),
      title = paste("Potato distribution by 2026 Scottish Parliamentary Constituency"),
      legend_title = "Area (hectares)"
    )
    
    # ===================== LOCAL AUTHORITY MAP =====================
    potato_uni_map <- reactive({
      potatoes_unitauth %>%     
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
        potato_uni_map() %>% filter(`crop` == input$variable_uni)
      }),
      unit = "hectares",
      footer = census_footer,
      variable = reactive(input$variable_uni),
      title = paste("Potato distribution by local authority in", census_year),
      legend_title = "Area (hectares)"
    )
    
    
    chart_data <- reactive({
      req(input$timeseries_variables)
      filtered_data <- potatoes_data %>%
        filter(`Crop/Land use` %in% input$timeseries_variables) %>%
        pivot_longer(cols = -`Crop/Land use`, names_to = "year", values_to = "value") %>%
        mutate(year = as.numeric(year))  # Ensure year is numeric
      filtered_data
    })
    
    areaChartServer(
      id = "area",
      chart_data = chart_data,
      title = "Area used to grow potatoes over time",
      yAxisTitle = "Area of potatoes (1,000 hectares)",
      xAxisTitle = "Year",
      unit = "hectares",
      footer = census_footer,
      x_col = "year",
      y_col = "value"
    )
    
    lineChartServer(
      id = "line",
      chart_data = chart_data,
      title = "Area used to grow potatoes over time",
      yAxisTitle = "Area of potatoes (1,000 hectares)",
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
                       potatoes_map %>%
                         pivot_wider(names_from = sub_region, values_from = value) %>%
                         mutate(across(where(is.numeric), comma))
                     },
                     
                     # -------------------
                     # 2. Timeseries data
                     # -------------------
                     "timeseries" = {
                       potatoes_data %>%
                         pivot_longer(cols = -`Crop/Land use`,
                                      names_to = "year",
                                      values_to = "value") %>%
                         pivot_wider(names_from = year, values_from = value) %>%
                         mutate(across(where(is.numeric) & !contains("Year"), comma))
                     },
                     
                     # -------------------
                     # 3. Constituency Table
                     # -------------------
                     "map_con" = {
                       potatoes_constituency %>%
                         rename(`Land use by category` = `crop`) %>%
                         mutate(across(where(is.numeric), comma))
                     },
                     
                     # -------------------
                     # 4. Local authority table
                     # -------------------
                     "map_uni" = {
                       potatoes_unitauth %>%
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
               "map" = paste0("Potatoes_Agricultural_Region_Map_Data_", Sys.Date(), ".csv"),
               "timeseries" = paste0("Potatoes_Timeseries_Data_", Sys.Date(), ".csv"),
               "map_con" = paste0("Potatoes_Constituency_Data_", Sys.Date(), ".csv"),
               "map_uni" = paste0("Potatoes_Local_Authority_Data_", Sys.Date(), ".csv"),
               
               # fallback
               paste0("Downloaded_Data_", Sys.Date(), ".csv")
        )
      },
      
      # ---- Write selected dataset to disk ----
      content = function(file) {
        
        data <- switch(input$table_data,
                       
                       # ---- Agricultural region map ----
                       "map" = {
                         potatoes_subregion %>%
                           filter(`Land use by category` == input$variable_region)# %>%
                         #  pivot_wider(names_from = sub_region, values_from = value)
                       },
                       
                       # ---- Timeseries ----
                       "timeseries" = {
                         potatoes_data %>%
                           pivot_longer(
                             cols = -`Crop/Land use`,
                             names_to = "year",
                             values_to = "value"
                           ) #%>%
                          # pivot_wider(names_from = year, values_from = value)
                       },
                       
                       # ---- Constituency ----
                       "map_con" = {
                         potatoes_constituency
                       },
                       
                       # ---- Local authority ----
                       "map_uni" = {
                         potatoes_unitauth
                       }
        )
        
        write.csv(data, file, row.names = FALSE)
      }
    )
  }
  )
}

potatoes_demo <- function() {
  ui <- fluidPage(potatoesUI("potatoes_test"))
  server <- function(input, output, session) {
    potatoesServer("potatoes_test")
  }
  shinyApp(ui, server)
}

potatoes_demo()
