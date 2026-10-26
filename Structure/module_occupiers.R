occupiersUI <- function(id) {
  ns <- NS(id)
  sidebarLayout(
    sidebarPanel(
      width = 3,
      uiOutput(ns("sidebar_ui"))
    ),
    mainPanel(
      width = 9,
      tabsetPanel(
        id = ns("tabs"),
        tabPanel(
          "Maps",
          value = "map",
          conditionalPanel(
            condition = sprintf("input['%s'] == 'region'", ns("map_region")),
            mapUI(ns("map"))
          ),
          conditionalPanel(
            condition = sprintf("input['%s'] == 'la'", ns("map_region")),
            mapUnitaryUI(ns("map_uni"))
          )
        ),
        # tabPanel("Constituency Map", mapConstituenciesUI(ns("map_con")), value = "map_con"),
        tabPanel("Population Pyramid", 
                 div(
                   htmlOutput(ns("pyramid_title")),
                   highchartOutput(ns("pyramid_chart"), height = "500px"),
                   htmlOutput(ns("pyramid_footer")),
                   div(
                     class = "note",
                     style = "margin-top: 20px; padding: 10px; border-top: 1px solid #ddd;",
                     HTML(
                       "<strong>Note:</strong><ul>
                          <li>To add or remove a series from the chart, select/deselect the variable from the sidebar menu.</li>
                          <li>You can see data values for a specific variable by hovering your mouse over the bar.</li>
                        </ul>"
                     )
                   )
                 ), 
                 value = "bar_chart"),
        tabPanel("Time Series", lineChartUI(ns("line_chart")), value = "timeseries"),
        tabPanel("Data Table", 
                 DTOutput(ns("data_table")),
                 downloadButton(ns("downloadData"), "Download Data"), 
                 generateCensusTableFooter(),

                 value = "data_table")
      )
    )
  )
}


occupiersServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Constituency map
    # occupier_const_map <- reactive({
    #   occupiers_constituency %>%         # <— your constituency land use table
    #     mutate(across(everything(), as.character)) %>%
    #     pivot_longer(
    #       cols = -`occupier`,
    #       names_to = "constituency",
    #       values_to = "value"
    #     ) %>% 
    #     mutate(
    #       value = if_else(is.na(value), NA_integer_, as.integer(value))
    #     )
    # })

    # Data Processing for Constituency Map
    # con_data <- reactive({
    #   occupiers_constituency %>%
    #     mutate(across(everything(), as.character)) %>%
    #     pivot_longer(
    #       cols = -`occupier`,
    #       names_to = "constituency",
    #       values_to = "value"
    #     ) %>% 
    #     mutate(
    #       value = if_else(is.na(value), NA_integer_, as.integer(value))) %>%
    #     filter(`occupier` %in% c(
    #           "Total working occupiers", 
    #           "Occupiers not working on the holding"
    #         ))
    # })
    
    # Sidebar UI
    output$sidebar_ui <- renderUI({
      req(input$tabs)
      if (input$tabs == "map") {
        tagList(
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
              "Total working occupiers",
              "Occupiers not working on the holding"
            )
          )
          
        )
      # } else if (input$tabs == "map_con") {
      #   radioButtons(ns("variable_con"), "Select Variable", choices = c(
      #     "Total working occupiers",
      #     "Occupiers not working on the holding"
      #   ))
      } else if (input$tabs == "data_table") {
        radioButtons(ns("data_source"), "Choose data to show:", 
                     choices = c("Agricultural Region Data", 
                                 "Local Authority Data",
                                 "Population Pyramid Data",  
                                 "Time Series Data"))
        
      } else if (input$tabs == "timeseries") {
        checkboxGroupInput(
          ns("variables"), 
          "Choose variables to add to chart", 
          choices = unique(occupiers_timeseries_table$`Occupiers and employees by category`), 
          selected = c('Occupiers - full time', 
                       'Total working occupiers', 
                       'Occupiers not working on the holding'))
        
      } else {
        checkboxGroupInput(ns("variables_bar"), 
                           "Choose age groups to display", 
                           choices = unique(occupiers_pyramid_chart$Age), 
                           selected = unique(occupiers_pyramid_chart$Age))
      }
    })
    
    # Bar Chart - Filtered Data
    filtered_data <- reactive({
      req(input$variables_bar)
      occupiers_pyramid_chart %>%
        filter(Age %in% input$variables_bar) %>% 
        mutate(Count = ifelse(Gender == "Female", -Count, Count))
    })
    
    # Bar Chart Title with Census Year
    output$pyramid_title <- renderUI({
      HTML(paste0("<div style='font-size: 20px; font-weight: bold;'>Breakdown of occupiers by age and gender in ", census_year, "</div>"))
    })
    
    # Bar Chart Footer
    output$pyramid_footer <- renderUI({
      HTML(census_footer)
    })
    
    # Bar Chart - Output
    output$pyramid_chart <- renderHighchart({
      data <- filtered_data()
      female_data <- data %>% filter(Gender == "Female") %>% select(y = Count) %>% pull()
      male_data <- data %>% filter(Gender == "Male") %>% select(y = Count) %>% pull()
      categories <- data %>% filter(Gender == "Male") %>% select(Age) %>% pull()
      max_count <- max(abs(c(male_data, female_data)), na.rm = TRUE)
      
      highchart() %>%
        hc_chart(type = "bar") %>%
        hc_xAxis(categories = categories, reversed = FALSE) %>%
        hc_yAxis(
          min = -max_count,
          max = max_count,
          title = list(text = "Number of working occupiers"), 
          labels = list(
            formatter = JS("function () {
          return Math.abs(this.value).toLocaleString();
        }")
          )
        ) %>%
        hc_plotOptions(bar = list(stacking = "normal")) %>%
        hc_add_series(name = "Female", data = as.list(female_data), color = "#002d54", 
           tooltip = list(pointFormatter = JS("function() { return 'Female: ' + Math.abs(this.y).toLocaleString() + ' occupiers'; }"))) %>%
        hc_add_series(name = "Male", data = as.list(male_data), color = "#2b9c93", 
           tooltip = list(pointFormatter = JS("function() { return 'Male: ' + this.y.toLocaleString() + ' occupiers'; }"))) %>%
        hc_tooltip(shared = FALSE) %>%
        hc_legend(align = "center") %>%
        hc_xAxis(title = list(text = "Age group")) %>%
        hc_yAxis(title = list(text = "Number of working occupiers"),
                 labels = list(formatter = JS("function () {
               return Math.abs(this.value).toLocaleString();
             }")))
    })
    
    # Timeseries Chart - Using the modular line chart
    filtered_timeseries_data <- reactive({
      req(input$variables)
      occupiers_timeseries_chart %>% 
        filter(`Occupiers and employees by category` %in% input$variables)
    })
    
    lineChartServer(
      id = "line_chart",
      chart_data = filtered_timeseries_data,
      title = "Agricultural occupiers over time",
      yAxisTitle = "Occupiers (1,000)",
      xAxisTitle = "Year",
      footer = census_footer,
      x_col = "Year",
      y_col = "Value"
    )
    
    # Agricultural region map server
    mapServer(
      id = "map",
      data = reactive({
        req(input$map_variable)
        occupiers_region_map %>% 
          filter(`occupier` == input$map_variable)
      }),
      variable = reactive(input$map_variable),
      footer = census_footer,
      title = paste("Occupiers by region in Scotland in", census_year),
      legend_title = "Number of occupiers"   
      )
    
    # mapConstituenciesServer(
    #   id = "map_con",
    #   data = reactive({
    #     req(input$variable_con)
    #     occupier_const_map() %>% filter(`occupier` == input$variable_con)
    #   }),
    #   unit = "occupiers",
    #   footer = census_footer,
    #   variable = reactive(input$variable_con),
    #   title = paste("Occupiers by 2026 Scottish Parliamentary Constituency"),
    #   legend_title = "Occupiers (Number)"
    # )

    # Local authority map server
    mapUnitaryServer(
      id = "map_uni",
      data = reactive({
        req(input$map_variable)
        occupiers_unitauth_map %>% 
          filter(`occupier` == input$map_variable)
      }),
      unit = "occupiers",
      footer = census_footer,
      variable = reactive(input$map_variable),
      title = paste("Occupiers by 2026 Scottish Parliamentary Constituency"),
      legend_title = "Occupiers (Number)"
    )

    # Data table
    output$data_table <- renderDT({
      req(input$data_source)
      
      if (input$data_source == "Population Pyramid Data") {
        
        datatable(
          occupiers_pyramid_table %>%
            mutate(across(where(is.numeric), comma)),
          options = list(scrollX = TRUE, pageLength = 26)
        )
        
      } else if (input$data_source == "Agricultural Region Data") {
        
        datatable(
          occupiers_region_table %>%
            mutate(across(where(is.numeric), comma)),
          options = list(scrollX = TRUE)
        )
        
      } else if (input$data_source == "Time Series Data") {
        
        datatable(
          occupiers_timeseries_table %>%
            mutate(across(where(is.numeric), comma)),
          options = list(scrollX = TRUE)
        )
        
      # } else if (input$data_source == "Constituency Data") {
      #   
      #   datatable(
      #     con_data() %>%
      #       mutate(across(where(is.numeric), comma)),
      #     options = list(scrollX = TRUE)
      #   )

      } else if (input$data_source == "Local Authority Data") {

        datatable(
          occupiers_unitauth_table %>%
            mutate(across(where(is.numeric), comma)),
          options = list(scrollX = TRUE)
        )
      }
    })
  }
  )
}

  

# Testing module
content_demo <- function() {
  ui <- fluidPage(occupiersUI("occupiers_map_test"))
  server <- function(input, output, session) {
    occupiersServer("occupiers_map_test")
  }
  shinyApp(ui, server)
}

content_demo()
