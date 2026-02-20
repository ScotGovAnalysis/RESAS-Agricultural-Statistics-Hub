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
        tabPanel("Map", mapUI(ns("map")), value = "map"),
        tabPanel("Constituency Map", mapConstituenciesUI(ns("map_con")), value = "map_con"),
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
    
    occupier_const_map <- reactive({
      occupiers_constituency %>%         # <— your constituency land use table
        mutate(across(everything(), as.character)) %>%
        pivot_longer(
          cols = -`occupier`,
          names_to = "constituency",
          values_to = "value"
        ) %>% 
        mutate(
          value = if_else(is.na(value), NA_integer_, as.integer(value))
        )
    })
    
    # Data Processing for Bar Chart
    chart_data <- reactive({
      occupiers_age_gender %>%
        filter(`Occupier working time` == "Total working occupiers") %>%
        select(-`Male total`, -`Female total`) %>%
        pivot_longer(cols = -`Occupier working time`, names_to = "Gender_Age", values_to = "Count") %>%
        separate(Gender_Age, into = c("Gender", "Age"), sep = " ", extra = "merge") %>%
        mutate(Count = as.numeric(Count))
    })
    
    
    # Data Processing for Map
    regions_data <- reactive({
      occupiers_employees_subregion %>%
        select(-`Scotland total`) %>%
        mutate(across(everything(), as.character)) %>%  # Ensure all columns are characters
        pivot_longer(cols = -`Occupiers and employees by category`, names_to = "sub_region", values_to = "value") %>%
        mutate(value = ifelse(is.na(as.numeric(value)), NA, as.numeric(value))) %>%
        filter(`Occupiers and employees by category` %in% c(
          "Total Working Occupiers", 
          "Occupiers Not Working On The Holding"
        ))
    })
    
    # Data Processing for Constituency Map
    con_data <- reactive({
      occupiers_constituency %>%
        mutate(across(everything(), as.character)) %>%
        pivot_longer(
          cols = -`occupiers`,
          names_to = "constituency",
          values_to = "value"
        ) %>% 
        mutate(
          value = if_else(is.na(value), NA_integer_, as.integer(value))) %>%
        filter(`workforce` %in% c(
              "Total Working Occupiers (Number)", 
              "Occupiers Not Working On The Holding (Number)"
            ))
    })
    
    # Data Processing for Timeseries
    occupiers_employees <- occupiers_employees %>%
      mutate(across(starts_with("20"), as.numeric))
    
    occupiers_timeseries_data <- reactive({
      occupiers_employees %>%
        pivot_longer(cols = -`Occupiers and employees by category`, names_to = "Year", values_to = "Value") %>%
        mutate(Year = as.numeric(Year)) %>%
        filter(grepl("occupiers", `Occupiers and employees by category`, ignore.case = TRUE))
    })
    
    # Sidebar UI
    output$sidebar_ui <- renderUI({
      req(input$tabs)
      if (input$tabs == "map") {
        radioButtons(ns("variable_region"), "Select Variable", choices = c(
          "Total Working Occupiers", 
          "Occupiers Not Working On The Holding"
        ))
      } else if (input$tabs == "map_con") {
        radioButtons(ns("variable_con"), "Select Variable", choices = c(
          "Total Working Occupiers (Number)",
          "Occupiers Not Working On The Holding (Number)"
        ))
      } else if (input$tabs == "data_table") {
        radioButtons(ns("data_source"), "Choose data to show:", choices = c("Map Data", "Population Pyramid Data",  "Time Series Data"))
      } else if (input$tabs == "timeseries") {
        checkboxGroupInput(
          ns("variables"), 
          "Choose variables to add to chart", 
          choices = unique(occupiers_timeseries_data()$`Occupiers and employees by category`), 
          selected = c('Occupiers - Full Time', 'Total Working Occupiers', 'Occupiers Not Working On The Holding')
        )
      } else {
        checkboxGroupInput(ns("variables_bar"), "Choose age groups to display", choices = unique(chart_data()$Age), selected = unique(chart_data()$Age))
      }
    })
    
    # Bar Chart - Filtered Data
    filtered_data <- reactive({
      req(input$variables_bar)
      chart_data() %>%
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
        hc_add_series(name = "Female", data = as.list(female_data), color = "#002d54", tooltip = list(pointFormatter = JS("function() { return 'Female: ' + Math.abs(this.y).toLocaleString() + ' occupiers'; }"))) %>%
        hc_add_series(name = "Male", data = as.list(male_data), color = "#2b9c93", tooltip = list(pointFormatter = JS("function() { return 'Male: ' + this.y.toLocaleString() + ' occupiers'; }"))) %>%
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
      data <- occupiers_timeseries_data()
      data %>% filter(`Occupiers and employees by category` %in% input$variables)
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
    
    # Map - Use the new map module
    mapServer(
      id = "map",
      data = regions_data,
      variable = reactive(input$variable_region),
      footer = census_footer,
      title = paste("Occupiers by region in Scotland in", census_year),
      legend_title = "Number of occupiers"   
      )
    
    mapConstituenciesServer(
      id = "map_con",
      data = reactive({
        req(input$variable_con)
        occupier_const_map() %>% filter(`occupier` == input$variable_con)
      }),
      unit = "occupiers",
      footer = census_footer,
      variable = reactive(input$variable_con),
      title = paste("Workforce by constituency in Scotland in", census_year),
      legend_title = "Occupiers (Number)"
    )
    
    # Pivoting Chart Data Wider for Data Table View
    pivoted_chart_data <- reactive({
      chart_data() %>%
        pivot_wider(names_from = Age, values_from = Count)%>%
        mutate(across(where(is.numeric) & !contains("Year"), comma))
    })
    
    # Pivoting Map Data Wider for Data Table View
    pivoted_regions_data <- reactive({
      regions_data() %>%
        pivot_wider(names_from = sub_region, values_from = value)%>%
        mutate(across(where(is.numeric) & !contains("Year"), comma))
    })
    
    # Pivoting Timeseries Data Wider for Data Table View
    pivoted_timeseries_data <- reactive({
      occupiers_timeseries_data() %>%
        pivot_wider(names_from = Year, values_from = Value)%>%
        mutate(across(where(is.numeric) & !contains("Year"), comma))
    })
    
    # Data Table - Output
    output$data_table <- renderDT({
      req(input$data_source)
      if (input$data_source == "Population Pyramid Data") {
        datatable(pivoted_chart_data(), options = list(
          scrollX = TRUE,
          pageLength = 26  # Adjust this to show all entries
        ))
      } else if (input$data_source == "Map Data") {
        datatable(pivoted_regions_data(), options = list(
          scrollX = TRUE
        ))
      } else if (input$data_source == "Time Series Data") {
        datatable(pivoted_timeseries_data(), options = list(
          scrollX = TRUE
        ))
      }
    })
    
    # Data Table - Download Handler
    output$downloadData <- downloadHandler(
      filename = function() {
        paste(input$data_source, Sys.Date(), ".csv", sep = "")
      },
      content = function(file) {
        if (input$data_source == "Population Pyramid Data") {
          write.csv(pivoted_chart_data(), file, row.names = FALSE)
        } else if (input$data_source == "Map Data") {
          write.csv(pivoted_regions_data(), file, row.names = FALSE)
        } else {
          write.csv(pivoted_timeseries_data(), file, row.names = FALSE)
        }
      }
    )
  })
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
