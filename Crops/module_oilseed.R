# File: module_oilseed.R

oilseedUI <- function(id) {
  ns <- NS(id)
  sidebarLayout(
    sidebarPanel(
      width = 3,
      
      # ===================== MAP =====================
      conditionalPanel(
        condition = "input.tabsetPanel === 'Agricultural Region Map'",
        ns = ns,
        radioButtons(
          ns("variable_region"), 
          "Select Variable", 
          choices = unique(oilseed_subregion$`Land use by category`)
        )
      ),
      
      
      # ===================== CONSTITUENCY MAP =====================
      conditionalPanel(
        condition = "input.tabsetPanel === 'Constituency Map'",
        ns = ns,
        radioButtons(
          ns("variable_con"), 
          "Select Variable", 
          choices = unique(oilseeds_constituency$crop)
        )
      ),
      
      
      # ===================== TIME SERIES =====================
      conditionalPanel(
        condition = "input.tabsetPanel === 'Time Series'",
        ns = ns,
        
        radioButtons(
          ns("measure"),
          "Select Measure:",
          choices = unique(oilseed_combined_long$Measure),
          selected = "Area"
        ),
        
        # Single checkboxGroupInput for all measures
        checkboxGroupInput(
          ns("timeseries_variables"),
          "Select crops to display:",
          choices = c("Total Oilseeds", "Linseed", "Winter Oilseed Rape","Spring Oilseed Rape"),
          selected = c("Winter Oilseed Rape","Spring Oilseed Rape")
        )
      ),
      
      # ===================== AREA CHART =====================
      conditionalPanel(
        condition = "input.tabsetPanel === 'Area Chart'",
        ns = ns,
        selectizeInput(
          ns("area_variables"),   
          "Click within the box to select variables",
          choices = unique(oilseed_data$`Crop/Land use`),
          selected  = c("Winter Oilseed Rape","Spring Oilseed Rape"),
          multiple = TRUE,
          options = list(plugins = list('remove_button'))
        )
      ),
      # ===================== DATA TABLE =====================
      conditionalPanel(
        condition = "input.tabsetPanel === 'Data Table'",
        ns = ns,
        radioButtons(
          ns("table_data"),
          "Select Data to Display",
          choices = c("Map Data" = "map", "Time Series Data" = "timeseries"),
          selected = "timeseries"
        )
      )
    ),
    # ===================== MAIN PANEL =====================
    mainPanel(
      width = 9,
      tabsetPanel(
        id = ns("tabsetPanel"),
        tabPanel("Agricultural Region Map", mapUI(ns("map"))),
        tabPanel("Constituency Map", mapConstituenciesUI(ns("map_con"))),
        tabPanel("Time Series", lineChartUI(ns("line"))),
        tabPanel("Area Chart", areaChartUI(ns("area"))),
        tabPanel("Data Table", 
                 DTOutput(ns("table")),
                 downloadButton(ns("downloadData"), "Download Data"),
                 generateCerealandoilseedTableFooter()

        )
      )
    )
  )
}

oilseedServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    # ===================== MAP =====================
    oilseed_map <- oilseed_subregion %>%
      select(-`Scotland total`) %>%
      mutate(across(everything(), as.character)) %>%
      pivot_longer(cols = -`Land use by category`, names_to = "sub_region", values_to = "value") %>%
      mutate(value = as.numeric(value))
    
    mapServer(
      id = "map",
      data = reactive({
        req(input$variable_region)
        oilseed_map %>% filter(`Land use by category` == input$variable_region)
      }),
      unit = "hectares",
      footer = census_footer,
      variable = reactive(input$variable_region),
      title = paste("Oilseed distribution by region in Scotland in", census_year),
      legend_title = "Area (hectares)"
    )
    
    # ===================== CONSTITUENCY MAP =====================
    oilseed_const_map <- reactive({
      oilseeds_constituency %>%         # <— your constituency land use table
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
        oilseed_const_map() %>% filter(`crop` == input$variable_con)
      }),
      unit = "hectares",
      footer = census_footer,
      variable = reactive(input$variable_con),
      title = paste("Oilseed distribution by Scottish Parliamentary Constituency in", census_year),
      legend_title = "Area (hectares)"
    )
    
    # ===================== AREA CHART =====================
    area_chart_data <- reactive({
      req(input$area_variables)   
      oilseed_data %>%
        filter(`Crop/Land use` %in% input$area_variables) %>%
        pivot_longer(
          cols = -`Crop/Land use`,
          names_to = "year",
          values_to = "value"
        ) %>%
        mutate(year = as.numeric(year)) %>%   
        arrange(year)                         
    })
    
    areaChartServer(
      id = "area",
      chart_data = area_chart_data,
      title = "Area used to grow oilseed in Scotland over time",
      yAxisTitle = "Area of oilseed (1,000 hectares)",
      xAxisTitle = "Year",
      unit = "hectares",
      footer = census_footer,
      x_col = "year",
      y_col = "value"
    )
    
    # ===================== TIME SERIES =====================
    # Update crop selection when measure changes
    observeEvent(input$measure, {
      if (input$measure == "Area") {
        choices <- c("Total Oilseeds", "Linseed", "Winter Oilseed Rape","Spring Oilseed Rape")
        selected <- c("Winter Oilseed Rape", "Spring Oilseed Rape")
      } else {
        choices <- unique(oilseed_tiff_data_long$`Crop/Land use`)
        selected <- c("Oilseed Rape")
      }
      
      updateCheckboxGroupInput(
        session,
        "timeseries_variables",
        choices = choices,
        selected = selected
      )
    })
    
    # Reactive data for line chart
    line_chart_data <- reactive({
      req(input$timeseries_variables, input$measure)
      
      if (input$measure == "Area") {
        df <- oilseed_data %>%
          filter(`Crop/Land use` %in% input$timeseries_variables) %>%
          pivot_longer(
            cols = -`Crop/Land use`,
            names_to = "Year",
            values_to = "value"
          ) %>%
          mutate(
            Year = as.numeric(Year),
            Measure = "Area"
          )
        # Keep only last 10 years
        max_year <- max(df$Year, na.rm = TRUE)
        df <- df %>% filter(Year > max_year - 10)
        
      } else {
        df <- oilseed_tiff_data_long %>%
          filter(
            `Crop/Land use` %in% input$timeseries_variables,
            Measure == input$measure
          ) %>%
          rename(value = Value) %>%
          mutate(Year = as.numeric(Year))
        
        # Keep only last 10 years
        max_year <- max(df$Year, na.rm = TRUE)
        df <- df %>% filter(Year > max_year - 10)
      }
      
      validate(
        need(nrow(df) > 0, "No data available for the selected options.")
      )
      
      df
    })
    
    line_chart_settings <- reactive({
      req(input$measure)
      
      switch(input$measure,
             "Area" = list(
               title = "Area of Oilseeds Planted",
               yAxisTitle = "Area of Oilseed (1,000 hectares)",
               unit = "hectares",
               footer = census_footer
             ),
             "Production" = list(
               title = "Production of Oilseed Rape",
               yAxisTitle = "Production of Oilseed Rape (1,000 tonnes)",
               unit = "tonnes",
               footer = cereal_oilseed_footer
             ),
             "Yield" = list(
               title = "Yield of Oilseed Rape",
               yAxisTitle = "Yield of Oilseed Rape (tonnes/hectare)",
               unit = "tonnes/hectare",
               footer = cereal_oilseed_footer
             )
      )
    })
    
    observeEvent(input$measure, {
      settings <- line_chart_settings()
      
      lineChartServer(
        id = "line",
        chart_data = line_chart_data,
        title = settings$title,
        yAxisTitle = settings$yAxisTitle,
        xAxisTitle = "Year",
        unit = settings$unit,
        footer = settings$footer,   # use footer from settings
        x_col = "Year",
        y_col = "value"
      )
    })
    
    
    # ===================== DATA TABLE =====================
    output$table <- renderDT({
      req(input$tabsetPanel == "Data Table")
      if (input$table_data == "map") {
        req(input$variable)
        oilseed_map %>%
          filter(`Land use by category` == input$variable) %>%
          pivot_wider(names_from = sub_region, values_from = value) %>%
          mutate(across(where(is.numeric) & !contains("Year"), comma)) %>% 
          datatable(
            options = list(
              scrollX = TRUE,  # Enable horizontal scrolling
              pageLength = 20  # Show 20 entries by default
            )
          )
      } else {
        
        ts_data <- oilseed_combined_long %>%
          pivot_wider(
            id_cols = c(`Crop/Land use`, Measure),
            names_from = Year,
            values_from = Value
          ) %>%
          # Relabel Measure values with units
          mutate(
            Measure = recode(
              Measure,
              "Area"       = "Area (hectares)",
              "Production" = "Production (tonnes)",
              "Yield"      = "Yield (tonnes per hectare)"
            )
          ) %>%
          select(
            `Crop/Land use`, Measure,
            sort(
              as.numeric(colnames(.)[!(colnames(.) %in% c("Crop/Land use", "Measure"))]),
              decreasing = FALSE
            ) %>% 
              as.character()
          ) %>%
          mutate(across(
            where(is.numeric),
            ~ case_when(
              Measure == "Yield (tonnes per hectare)" ~ comma(round(.x, 1), accuracy = 0.1),
              TRUE                                   ~ comma(round(.x, 0), accuracy = 1)
            )
          )) %>% 
          arrange(Measure)
        
        # Columns for alignment
        left_cols  <- c("Crop/Land use", "Measure")
        right_cols <- setdiff(names(ts_data), left_cols)
        
        datatable(
          ts_data,
          options = list(
            scrollX = TRUE,
            pageLength = 20
          )
        ) %>%
          formatStyle(left_cols,  `text-align` = "left") %>%
          formatStyle(right_cols, `text-align` = "right")
        
      }
    })
    
    output$downloadData <- downloadHandler(
      filename = function() {
        if (input$table_data == "map") {
          paste("Oilseed_Map_Data_", Sys.Date(), ".csv", sep = "")
        } else {
          paste("Oilseed_Timeseries_Data_", Sys.Date(), ".csv", sep = "")
        }
      },
      content = function(file) {
        data <- if (input$table_data == "map") {
          oilseed_map %>%
            filter(`Land use by category` == input$variable) %>%
            pivot_wider(names_from = sub_region, values_from = value) %>%
            mutate(across(where(is.numeric) & !contains("Year"), comma))
        } else {
          oilseed_combined_long %>%
            pivot_wider(
              id_cols = c(`Crop/Land use`, Measure),
              names_from = Year,
              values_from = Value
            ) %>%
            # Relabel Measure values with units
            mutate(
              Measure = recode(
                Measure,
                "Area"       = "Area (hectares)",
                "Production" = "Production (tonnes)",
                "Yield"      = "Yield (tonnes per hectare)"
              )
            ) %>%
            select(
              `Crop/Land use`, Measure,
              sort(as.numeric(colnames(.)[!(colnames(.) %in% c("Crop/Land use", "Measure"))]), decreasing = FALSE) %>% 
                as.character()
            ) %>%
            mutate(across(
              where(is.numeric),
              ~ case_when(
                Measure == "Yield (tonnes per hectare)" ~ comma(round(.x, 1), accuracy = 0.1),  # 1 dp with commas
                TRUE                                   ~ comma(round(.x, 0), accuracy = 1)      # 0 dp with commas
              )
            ))  %>% 
            arrange(Measure)
        }
        write.csv(data, file, row.names = FALSE)
      }
    )
  })
}
    

# # Testing module
# oilseed_demo <- function() {
#   ui <- fluidPage(oilseedUI("oilseed_test"))
#   server <- function(input, output, session) {
#     oilseedServer("oilseed_test")
#   }
#   shinyApp(ui, server)
# }
# 
# oilseed_demo()
