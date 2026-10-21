# File: module_human_vegetables.R

humanVegetablesUI <- function(id) {
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
          choices = unique(human_vegetables_subregion$`Land use by category`)
        )
      ),
      
      # Constituency map
      # conditionalPanel(
      #   condition = "input.tabsetPanel === 'Constituency Map'",
      #   ns = ns,
      #   radioButtons(
      #     ns("variable_con"), 
      #     "Select Variable", 
      #     choices = unique(vegetables_constituency$crop)
      #   )
      # ),     

      # Time series
      conditionalPanel(
        condition = "input.tabsetPanel === 'Time Series'",
        ns = ns,
        checkboxGroupInput(
          ns("timeseries_variables"),
          "Select Time Series Variables",
          choices = unique(human_vegetables_data$`Vegetables and fruits for human consumption`),
          selected = c(
            "Peas for canning, freezing or drying",
            "Beans for canning, freezing or drying",
            "Turnips/swedes",
            "Calabrese",
            "Cauliflower",
            "Carrots"
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

humanVegetablesServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    human_vegetables_map <- human_vegetables_subregion %>%
      select(-`Scotland total`) %>%
      mutate(across(everything(), as.character)) %>%
      pivot_longer(cols = -`Land use by category`, names_to = "sub_region", values_to = "value") %>%
      mutate(value = as.numeric(value))
    
    mapServer(
      id = "map",
      data = reactive({
        req(input$map_variable)
        human_vegetables_map %>% filter(`Land use by category` == input$map_variable)
      }),
      unit = "hectares",
      footer = census_footer,
      variable = reactive(input$map_variable),
      title = paste("Vegetables for human consumption distribution by region in Scotland in", census_year),
      legend_title = "Area (hectares)"
    )
    
    # ===================== CONSTITUENCY MAP =====================
    # veg_const_map <- reactive({
    #   vegetables_constituency %>%       
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
    #     veg_const_map() %>% filter(`crop` == input$variable_con)
    #   }),
    #   unit = "hectares",
    #   footer = census_footer,
    #   variable = reactive(input$variable_con),
    #   title = paste("Vegetables for human consumption crops distribution by 2026 Scottish Parliamentary Constituency"),
    #   legend_title = "Area (hectares)"
    # )
    
    # ===================== LOCAL AUTHORITY MAP =====================
    veg_uni_map <- reactive({
      vegetables_unitauth %>%
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
        veg_uni_map() %>% filter(`crop` == input$map_variable)
      }),
      unit = "hectares",
      footer = census_footer,
      variable = reactive(input$map_variable),
      title = paste("Vegetables for human consumption crops distribution by local authority in", census_year),
      legend_title = "Area (hectares)"
    )

    chart_data <- reactive({
      req(input$timeseries_variables)
      filtered_data <- human_vegetables_data %>%
        filter(`Vegetables and fruits for human consumption` %in% input$timeseries_variables) %>%
        pivot_longer(cols = -`Vegetables and fruits for human consumption`, names_to = "year", values_to = "value") %>%
        mutate(year = as.numeric(year))  # Ensure year is numeric
      filtered_data
    })
    
    lineChartServer(
      id = "line",
      chart_data = chart_data,
      title = "Area used to grow vegetables for human consumption across time",
      yAxisTitle = "Area of vegetables (1,000 hectares)",
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
                       human_vegetables_map %>%
                         pivot_wider(names_from = sub_region, values_from = value) %>%
                         mutate(across(where(is.numeric), comma))
                     },
                     
                     # -------------------
                     # 2. Timeseries data
                     # -------------------
                     "timeseries" = {
                       human_vegetables_data %>%
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
                     #   vegetables_constituency %>%
                     #     rename(`Land use by category` = `crop`) %>%
                     #     mutate(across(where(is.numeric), comma))
                     # },
                     
                     # -------------------
                     # 4. Local authority table
                     # -------------------
                     "map_uni" = {
                       vegetables_unitauth %>%
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
               "map" = paste0("Vegetables_Agricultural_Region_Data_", Sys.Date(), ".csv"),
               "timeseries" = paste0("Vegetables_Timeseries_Data_", Sys.Date(), ".csv"),
               # "map_con" = paste0("Vegetables_Constituency_Data_", Sys.Date(), ".csv"),
               "map_uni" = paste0("Vegetables_Local_Authority_Data_", Sys.Date(), ".csv"),
               
               # fallback
               paste0("Downloaded_Data_", Sys.Date(), ".csv")
        )
      },
      
      # ---- Write selected dataset to disk ----
      content = function(file) {
        
        data <- switch(input$table_data,
                       
                       # ---- Agricultural region map ----
                       "map" = {
                         human_vegetables_map %>%
                           filter(`Land use by category` == input$map_variable) %>%
                           pivot_wider(names_from = sub_region, values_from = value)
                       },
                       
                       # ---- Timeseries ----
                       "timeseries" = {
                         human_vegetables_data %>%
                           pivot_longer(
                             cols = -`Vegetables and fruits for human consumption`,
                             names_to = "year",
                             values_to = "value"
                           ) %>%
                           pivot_wider(names_from = year, values_from = value)
                       },
                       
                       # ---- Constituency ----
                       # "map_con" = {
                       #   vegetables_constituency
                       # },
                       # 
                       # ---- Local authority ----
                       "map_uni" = {
                         vegetables_unitauth
                       }
        )
        
        write.csv(data, file, row.names = FALSE)
      }
    )
  }
  )
}


human_vegetables_demo <- function() {
  ui <- fluidPage(humanVegetablesUI("human_vegetables_test"))
  server <- function(input, output, session) {
    humanVegetablesServer("human_vegetables_test")
  }
  shinyApp(ui, server)
}
human_vegetables_demo()