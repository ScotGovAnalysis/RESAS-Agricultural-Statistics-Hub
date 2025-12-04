# File: module_cereals.R

cerealsUI <- function(id) {
  ns <- NS(id)
  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      
      # ===================== MAP =====================
      conditionalPanel(
        condition = "input.tabsetPanel === 'Map'",
        ns = ns,
        radioButtons(
          ns("variable"), 
          "Select Variable", 
          choices = unique(cereals_subregion$`Land use by category`)
        )
      ),
      
      # ===================== TIME SERIES =====================
      conditionalPanel(
        condition = "input.tabsetPanel === 'Time Series'",
        ns = ns,
        
        radioButtons(
          ns("measure"),
          "Select Measure:",
          choices = unique(cereals_combined_long$Measure),
          selected = "Area"
        ),
        
        # Single checkboxGroupInput for all measures
        checkboxGroupInput(
          ns("timeseries_variables"),
          "Select crops to display:",
          choices =  c("Total Cereals", "Main Cereals (Barley, Oats and Wheat)",
                        "Spring Barley", "Winter Barley", "Total Barley",
                        "Maize",
                        "Spring Oats", "Winter Oats", "Total Oats", 
                        "Rye", "Triticale", "Wheat"),
          selected = c("Wheat", "Total Barley", "Total Oats")
        )
      ),
      
      
      # ===================== AREA CHART =====================
      conditionalPanel(
        condition = "input.tabsetPanel === 'Area Chart'",
        ns = ns,
        selectizeInput(
          ns("area_variables"),
          "Click within the box to select variables",
          choices = unique(cereals_data_census$`Crop/Land use`),
          selected = c("Wheat", "Total Barley", "Total Oats"),
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
          selected = "map"
        )
      ),
      
      # ===================== SUMMARY =====================
      conditionalPanel(
        condition = "input.tabsetPanel === 'Cereals Summary'",
        ns = ns,
        div("Adjust the sliders to compare data from different years.", 
            style = "font-size: 14px; font-weight: bold; margin-bottom: 10px;"),
        sliderInput(ns("summary_current_year_cereals"), "Year of interest",
                    min = 2012, max = census_year, value = census_year, step = 1, sep = ""),
        sliderInput(ns("summary_comparison_year_cereals"), "Comparison year",
                    min = 2012, max = census_year, value = census_year-1, step = 1, sep = "")
      )
    ),
    
    # ===================== MAIN PANEL =====================
    mainPanel(
      width = 9,
      tabsetPanel(
        id = ns("tabsetPanel"),
        tabPanel("Map", mapUI(ns("map"))),
        tabPanel("Time Series", lineChartUI(ns("line"), note_type = 2)),
        tabPanel("Area Chart", areaChartUI(ns("area"), note_type = 2)),
        tabPanel("Data Table", 
                 DTOutput(ns("table")),
                 downloadButton(ns("downloadData"), "Download Data"),
                 generateCensusTableFooter()
        ),
        tabPanel("Summary",
                 fluidRow(
                   column(width = 6, h3("Cereals summary section")),
                   column(width = 3,
                          selectInput(ns("summary_variable"),
                                      "Select Variable",
                                      choices = unique(cereals_data_census$`Crop/Land use`),
                                      selected = "Total cereals"))
                 ),
                 fluidRow(
                   column(width = 6, p("This content is under development.")),
                   column(width = 6, valueBoxUI(ns("summaryValueBox")),
                          style = "padding-right: 0; padding-left: 0; padding-bottom: 10px;")
                 )
        )
      )
    )
    
  ) # end sidebarLayout
}


cerealsServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    # ===================== MAP =====================
    cereals_map <- cereals_subregion %>%
      select(-`Scotland total`) %>%
      mutate(across(everything(), as.character)) %>%
      pivot_longer(cols = -`Land use by category`, names_to = "sub_region", values_to = "value") %>%
      mutate(value = as.numeric(value))
    
    mapServer(
      id = "map",
      data = reactive({
        req(input$variable)
        cereals_map %>% filter(`Land use by category` == input$variable)
      }),
      unit = "hectares",
      footer = census_footer,
      variable = reactive(input$variable),
      title = paste("Cereals distribution by region in Scotland in", census_year),
      legend_title = "Area (hectares)"
    )
    # ===================== AREA CHART =====================
    area_chart_data <- reactive({
      req(input$timeseries_variables)
      filtered_data <- cereals_data_census %>%
        filter(`Crop/Land use` %in% input$timeseries_variables) %>%
        pivot_longer(cols = -`Crop/Land use`, names_to = "year", values_to = "value") %>%
        mutate(year = as.numeric(year))  # Ensure year is numeric
      filtered_data
    })
    
    areaChartServer(
      id = "area",
      chart_data = area_chart_data,
      title = "Area used to grow cereals over time",
      yAxisTitle = "Area of cereals (1,000 hectares)",
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
        choices <- c("Total Cereals", "Main Cereals (Barley, Oats and Wheat)",
                   "Spring Barley", "Winter Barley", "Total Barley",
                   "Maize",
                   "Spring Oats", "Winter Oats", "Total Oats", 
                   "Rye", "Triticale", "Wheat")
        selected <- c("Wheat", "Total Barley", "Total Oats")
      } else {
        choices <- c("Main Cereals (Barley, Oats and Wheat)",
                     "Spring Barley", "Winter Barley", "Total Barley",
                     "Total Oats", "Wheat")
        selected <- c("Total Barley","Total Oats","Wheat")
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
        df <- cereals_data_census %>%
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
      } else {
        df <- cereals_tiff_data_long %>%
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
    
    # Chart settings based on measure
    line_chart_settings <- reactive({
      req(input$measure)
      
      switch(input$measure,
             "Area" = list(
               title = "Area used to grow cereals over time",
               yAxisTitle = "Area of cereals (1,000 hectares)",
               unit = "hectares",
               footer = census_footer
             ),
             "Production" = list(
               title = "Production of cereals over time",
               yAxisTitle = "Production of cereals (1,000 tonnes)",
               unit = "tonnes",
               footer = cereal_oilseed_footer
             ),
             "Yield" = list(
               title = "Yield of cereals over time",
               yAxisTitle = "Yield of cereals (tonnes/hectare)",
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
        footer = settings$footer,   # use the footer from settings
        x_col = "Year",
        y_col = "value"
      )
    })
    
    # ===================== DATA TABLE =====================
    output$table <- renderDT({
      req(input$tabsetPanel == "Data Table")
      if (input$table_data == "map") {
        req(input$variable)
        cereals_map %>%
          pivot_wider(names_from = sub_region, values_from = value) %>%
          mutate(across(where(is.numeric) & !contains("Year"), comma)) %>%
          datatable(
            options = list(
              scrollX = TRUE,  # Enable horizontal scrolling
              pageLength = 20,  # Show 20 entries by default
              autoWidth = TRUE, # Apply column widths
              columnDefs = list(
                list(width = '100px', targets = 1)
              )
            )
          )
      } else {
        cereals_combined_long %>%
          pivot_wider(
            id_cols = c(`Crop/Land use`, Measure),  
            names_from = Year,                      
            values_from = Value                     
          ) %>%
          select(
            `Crop/Land use`, Measure,
            sort(as.numeric(colnames(.)[!(colnames(.) %in% c("Crop/Land use", "Measure"))]), decreasing = TRUE) %>% 
              as.character()
          ) %>%
          mutate(across(
            where(is.numeric),
            ~ case_when(
              Measure == "Yield" ~ comma(round(.x, 2), accuracy = 0.01),  # 2 dp with commas
              Measure != "Yield" ~ comma(round(.x, 0), accuracy = 1)      # 0 dp with commas
            )
          )) %>%
          datatable(
            options = list(
              scrollX = TRUE,  # Enable horizontal scrolling
              pageLength = 20  # Show 20 entries by default
            )
          )
      }
    })
    
    output$downloadData <- downloadHandler(
      filename = function() {
        if (input$table_data == "map") {
          paste("Cereals_Map_Data_", Sys.Date(), ".csv", sep = "")
        } else {
          paste("Cereals_Timeseries_Data_", Sys.Date(), ".csv", sep = "")
        }
      },
      content = function(file) {
        data <- if (input$table_data == "map") {
          cereals_map %>%
            filter(`Land use by category` == input$variable) %>%
            pivot_wider(names_from = sub_region, values_from = value)
        } else {
          cereals_data_census %>%
            pivot_longer(cols = -`Crop/Land use`, names_to = "year", values_to = "value") %>%
            pivot_wider(names_from = year, values_from = value)
        }
        write.csv(data, file, row.names = FALSE)
      }
    )
    
    # Reactive expression for the selected variable and years
    summary_data <- reactive({
      cereals_data_census %>%
        filter(`Crop/Land use` == input$summary_variable) %>%
        pivot_longer(cols = -`Crop/Land use`, names_to = "Year", values_to = "Value") %>%
        mutate(Year = as.numeric(Year))
    })
    
    current_year <- reactive({ input$summary_current_year_cereals })
    comparison_year <- reactive({ input$summary_comparison_year_cereals })
    
    # Value box for the selected variable
    valueBoxServer("summaryValueBox", summary_data, "Crop/Land use", reactive(input$summary_variable), current_year, comparison_year, "ha")
  })
}

# Testing module
cereals_demo <- function() {
  ui <- fluidPage(cerealsUI("cereals_test"))
  server <- function(input, output, session) {
    cerealsServer("cereals_test")
  }
  shinyApp(ui, server)
}

cereals_demo()

