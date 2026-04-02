landUseSummaryUI <- function(id) {
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
          choices = unique(land_use_subregion$`Land use by category`)
        )
      ),
      
      # Only show for Constituency Map tab
      conditionalPanel(
        condition = "input.tabsetPanel === 'Constituency Map'",
        ns = ns,
        radioButtons(
          ns("variable_con"),
          "Select Variable",
          choices = unique(land_use_constituency$`land use`)
        )
      ),
      
      # Unitary Authority Map tab
      conditionalPanel(
        condition = "input.tabsetPanel === 'Local Authority Map'",
        ns = ns,
        radioButtons(
          ns("variable_uni"),
          "Select Variable",
          choices = unique(land_use_unitauth$`land use`)
        )
      ),
    
      conditionalPanel(
        condition = "input.tabsetPanel === 'Summary'",
        ns = ns,
        div(
          style = "font-size: 24px; font-weight: bold;",
          " "
        )
      ), 
      conditionalPanel(
        condition = "input.tabsetPanel === 'Bar Chart'",
        ns = ns,
        checkboxGroupInput(
          ns("variables"), 
          "Choose variables to add to chart", 
          choices = c("Total Crops, Fallow, And Set-Aside", "Total Grass", "Rough Grazing", 
                      "Total Sole Right Agricultural Area", "Common Grazings"),
          selected = c("Total Crops, Fallow, And Set-Aside", "Total Grass", "Rough Grazing", 
                       "Total Sole Right Agricultural Area", "Common Grazings")
        )
      ),
      conditionalPanel(
        condition = "input.tabsetPanel === 'Time Series'",
        ns = ns,
        checkboxGroupInput(
          ns("timeseries_variables"),
          "Select Time Series Variables",
          choices = unique(land_use_data$`Crop/Land use`),
          selected = c(
            "Common Grazings",
            "Rough Grazing",
            "Total Crops, Fallow, And Set-Aside",
            "Total Grass",
            "Cauliflower",
            "Total Sole Right Agricultural Area"
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
      id = ns("mainpanel"),
      width = 9,
      tabsetPanel(
        id = ns("tabsetPanel"),
        tabPanel("Agricultural Region Map", mapUI(ns("map"))),
        tabPanel("Constituency Map", mapConstituenciesUI(ns("map_con"))),
        tabPanel("Local Authority Map", mapUnitaryUI(ns("map_uni"))),
        tabPanel("Bar Chart", barChartUI(ns("bar_chart"))),
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
    
    # Agricultural region map
    # Map data remains unformatted for proper rendering
    land_use_map <- reactive({
      land_use_subregion %>%
        select(-`Scotland total`) %>%
        mutate(across(everything(), as.character)) %>%
        pivot_longer(cols = -`Land use by category`, names_to = "sub_region", values_to = "value") %>%
        mutate(value = as.numeric(value))
    })
    
    # Constituency map
    land_use_const_map <- reactive({
      land_use_constituency %>%   
        mutate(across(everything(), as.character)) %>%
        pivot_longer(
          cols = -`land use`,
          names_to = "constituency",
          values_to = "value"
        ) %>% 
        mutate(
          value = if_else(is.na(value), NA_real_, as.numeric(value))
        )
    })
    
    # Local Authority Map
    land_use_unitauth_map <- reactive({
      land_use_unitauth %>%        
        mutate(across(everything(), as.character)) %>%
        pivot_longer(
          cols = -`land use`,
          names_to = "unitauth",
          values_to = "value"
        ) %>% 
        mutate(
          value = if_else(is.na(value), NA_real_, as.numeric(value))
        )
    })
    
    # Table data with formatted values for better readability
    # table_data <- reactive({
    #   if (input$table_data == "map") {
    #     land_use_subregion %>%
    #       mutate(across(where(is.numeric), comma))  # Format numeric columns with commas
    #   } else {
    #     land_use_data %>%
    #       mutate(across(where(is.numeric), comma))  # Format numeric columns with commas
    #   }
    # })
    
    
    table_data <- reactive({
      req(input$table_data)
      
      # Pick the raw dataset
      raw_data <- switch(
        input$table_data,
        "map"        = land_use_subregion,
        "timeseries" = land_use_data,
        "map_con"     = land_use_constituency,
        "map_uni"   = land_use_unitauth
      )
      
      
      if ("land use" %in% names(raw_data)) {
        raw_data <- raw_data %>% rename(`Crop/Land use` = `land use`)
      }
      
      
      # Apply numeric formatting once
      
      
      raw_data %>%
        mutate(
          across(
            everything(),
            ~ ifelse(
              # test: is it numeric once coerced?
              !is.na(suppressWarnings(as.numeric(.x))),
              
              # yes → round + comma format
              scales::comma(round(as.numeric(.x))),
              
              # no → return original value (e.g., "c")
              .x
            )
          )
        )
      
    })
    
    
    # 
    # con_data <- reactive({land_use_constituency %>%
    #     rename(`Crop/Land use` = `land use`)})
    # 
    # uni_data <- reactive({land_use_unitauth %>%
    #     rename(`Crop/Land use` = `land use`)})
    
    mapServer(
      id = "map",
      data = reactive({
        req(input$variable_region)
        land_use_map() %>% filter(`Land use by category` == input$variable_region)
      }),
      unit = "hectares",
      footer = census_footer,
      variable = reactive(input$variable_region),
      title = paste("Land use by region in Scotland in", census_year),
      legend_title = "Area (hectares)"
    )
    
    mapConstituenciesServer(
      id = "map_con",
      data = reactive({
        req(input$variable_con)
        land_use_const_map() %>% filter(`land use` == input$variable_con)
      }),
      unit = "hectares",
      footer = census_footer,
      variable = reactive(input$variable_con),
      title = paste("Land use by 2026 Scottish Parliamentry Constituency"),
      legend_title = "Area (hectares)"
    )
    
    mapUnitaryServer(
      id = "map_uni",
      data = reactive({
        req(input$variable_uni)
        land_use_unitauth_map() %>% filter(`land use` == input$variable_uni)
      }),
      unit = "hectares",
      footer = census_footer,
      variable = reactive(input$variable_uni),
      title = paste("Land use by local authority in", census_year),
      legend_title = "Area (hectares)"
    )
    

    chart_data <- reactive({
      agricultural_area_hectares %>%
        filter(`Crop/Land use` %in% input$variables) %>%
        select(`Crop/Land use`, `2025 Area`) %>%
        rename(Variable = `Crop/Land use`, Value = `2025 Area`)
    })
    
    timeseries_data <- reactive({
      req(input$timeseries_variables)
      land_use_data %>%
        filter(`Crop/Land use` %in% input$timeseries_variables) %>%
        pivot_longer(cols = -`Crop/Land use`, names_to = "year", values_to = "value") %>%
        mutate(year = as.numeric(year))  # Ensure year is numeric
    })
    

    
    barChartServer(
      id = "bar_chart",
      chart_data = chart_data,
      title = paste("Agricultural area by land use type in", census_year),
      yAxisTitle = "Area (1,000 hectares)",
      xAxisTitle = "Land use type",
      unit = "hectares",
      footer = census_footer,
      x_col = "Variable",
      y_col = "Value",
      tooltip_unit= reactive("hectares")
    )
    
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
                       land_use_map() %>%
                         pivot_wider(names_from = `sub_region`, values_from = value) %>%
                         mutate(across(where(is.numeric) & !contains("Year"), comma))
                     },
                     
                     # -------------------
                     # 2. Timeseries data
                     # -------------------
                     "timeseries" = {
                       land_use_data %>%
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
                       land_use_constituency %>%
                         rename(`Land use by category` = `land use`) %>%
                         
                         mutate(across(
                           everything(),
                           ~ {
                             x <- as.character(.x)
                             
                             # extract numbers (gives NA for "c")
                             nums <- readr::parse_number(x)
                             
                             # round + comma format where numeric exists
                             formatted <- ifelse(
                               is.na(nums),
                               x,   # keep original ("c", NA, etc.)
                               scales::comma(round(nums, 0))
                             )
                             
                             formatted
                           }
                         ))
                     },
                     
                     
                     
                     
                     # -------------------
                     # 4. Local authority table
                     # -------------------
                     "map_uni" = {
                       land_use_unitauth %>%
                         rename(`Land use by category` = `land use`) %>%
                         mutate(across(
                           everything(),
                           ~ {
                             x <- as.character(.x)
                             
                             # extract numbers (gives NA for "c")
                             nums <- readr::parse_number(x)
                             
                             # round + comma format where numeric exists
                             formatted <- ifelse(
                               is.na(nums),
                               x,   # keep original ("c", NA, etc.)
                               scales::comma(round(nums, 0))
                             )
                             
                             formatted
                           }
                         ))
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
               
               "map_con" = paste0("Land_Use_Constituency_Data_", Sys.Date(), ".csv"),
               
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
                         land_use_subregion %>%
                           filter(`Land use by category` == input$variable_region)# %>%
                      #     pivot_wider(names_from = sub_region, values_from = value)
                       },
                       
                       # ---- Timeseries ----
                       "timeseries" = {
                         land_use_data %>%
                           pivot_longer(
                             cols = -`Crop/Land use`,
                             names_to = "year",
                             values_to = "value"
                           ) %>%
                           pivot_wider(names_from = year, values_from = value)
                       },
                       
                       # ---- Constituency ----
                       "map_con" = {
                         land_use_constituency
                       },
                       
                       # ---- Local authority ----
                       "map_uni" = {
                         land_use_unitauth
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



