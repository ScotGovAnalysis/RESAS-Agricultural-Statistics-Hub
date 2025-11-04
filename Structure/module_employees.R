# Coerce all relevant columns to character before pivoting
occupiers_employees_subregion <- occupiers_employees_subregion %>%
  mutate(across(-`Occupiers and employees by category`, as.character))

# Transform the data
regions_data <- occupiers_employees_subregion %>% 
  select(-`Scotland total`) %>% 
  pivot_longer(cols = -`Occupiers and employees by category`, names_to = "sub_region", values_to = "value") %>%
  mutate(value = case_when(value == "c"~ NA,
                           .default = as.numeric(value)))

# Filter for the specific categories
categories <- c("Regular Full-Time Staff Total", 
                "Regular Part-Time Staff Total", 
                "Total Casual And Seasonal Staff", 
                "Total Workforce (including occupiers)")

filtered_regions_data <- regions_data %>%
  filter(`Occupiers and employees by category` %in% categories)

employeesMapUI <- function(id) {
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
        tabPanel("Time Series", 
                 lineChartUI(ns("line_chart"), note_type = 2),  # Use note_type = 2 for the second note
                 value = "timeseries"),
        tabPanel("Data Table", 
                 DTOutput(ns("data_table")),
                 div(
                   style = "background-color: #f0f0f0; padding: 15px; border-radius: 5px; margin-top: 10px; font-style: bold;",
                   "Note: Migrant labour is not directly comparable to other employee measures as it is measured in person working days."
                 ),  # Separate note about migrant labour
                 downloadButton(ns("downloadData"), "Download Data"),   
                 generateCensusTableFooter(),  # Existing footer

                 value = "data_table")
      )
    )
  )
}


employeesMapServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Data Processing for Timeseries
    occupiers_employees <- occupiers_employees %>%
      select(-`% Change 2025 to 2024`) %>%
      mutate(across(starts_with("20"), as.numeric))
    
    # Reactive data for the time series chart
    chart_data <- reactive({
      occupiers_employees %>%
        pivot_longer(cols = -`Occupiers and employees by category`, names_to = "Year", values_to = "Value") %>%
        mutate(Year = as.numeric(Year)) %>%
        filter(!grepl("occupiers", `Occupiers and employees by category`, ignore.case = TRUE))
    })
    
    output$sidebar_ui <- renderUI({
      req(input$tabs)
      if (input$tabs == "map") {
        radioButtons(ns("variable"), "Select Variable", choices = categories)
      } else if (input$tabs == "data_table") {
        radioButtons(ns("data_source"), "Choose data to show:", choices = c("Chart Data", "Map Data"))
      } else if (input$tabs == "timeseries") {
        selectizeInput(
          ns("variables"), 
          "Click within the box to add more variables", 
          choices = unique(chart_data()$`Occupiers and employees by category`), 
          selected = c('Regular Full-Time Staff Total', 'Regular Part-Time Staff Total', 'Total Casual and seasonal staff'), 
          multiple = TRUE, 
          options = list(plugins = list('remove_button'), placeholder = "Click to add more variables")
        )
      }
    })
    
    # Pivot the chart data wider for the data table view
    pivoted_chart_data <- reactive({
      chart_data() %>%
        pivot_wider(names_from = Year, values_from = Value) %>%
        mutate(across(where(is.numeric) & !contains("Year"), comma))
    })
    
    # Pivot the map data wider for the data table view
    pivoted_regions_data <- reactive({
      filtered_regions_data %>%
        pivot_wider(names_from = sub_region, values_from = value) %>%
        mutate(across(where(is.numeric) & !contains("Year"), comma))
    })
    
    # Time series chart rendering
    lineChartServer(
      id = "line_chart",
      chart_data = reactive({
        req(input$variables)
        chart_data() %>%
          filter(`Occupiers and employees by category` %in% input$variables)
      }),
      title = "Agricultural employees over time",
      yAxisTitle = "Employees (1,000)",
      xAxisTitle = "Year",
      unit = "employees",
      footer = '<div style="font-size: 16px; font-weight: bold;"><a href="https://www.gov.scot/publications/results-from-the-scottish-agricultural-census-june-2025/">Source: Scottish Agricultural Census: June 2025</a></div>',
      x_col = "Year",
      y_col = "Value"
    )
    
    # Map rendering
    mapServer(
      id = "map",
      data = reactive({
        req(input$variable)
        filtered_regions_data %>%
          filter(`Occupiers and employees by category` == input$variable)
      }),
      unit = "employees",
      footer = '<div style="font-size: 16px; font-weight: bold;"><a href="https://www.gov.scot/publications/results-from-the-scottish-agricultural-census-june-2025/">Source: Scottish Agricultural Census: June 2025</a></div>',
      variable = reactive(input$variable),
      title = paste("Agricultural employees by region in Scotland in", census_year),
      legend_title = "Number of employees"
    )  
    
    # Render the data table with scrollable options for both chart and map data
    output$data_table <- renderDT({
      req(input$data_source)
      if (input$data_source == "Chart Data") {
        datatable(pivoted_chart_data(), options = list(
          scrollX = TRUE,
          pageLength = 26  # Show all 26 entries on a single page
        ))
      } else if (input$data_source == "Map Data") {
        datatable(pivoted_regions_data(), options = list(
          scrollX = TRUE,
          pageLength = 10  # You can adjust this if needed for the map data
        ))
      }
    })
    
    # Create a download handler with appropriate naming
    output$downloadData <- downloadHandler(
      filename = function() {
        if (input$data_source == "Chart Data") {
          paste("Scottish Agricultural Employees Timeseries Data - 2013 to 2025.csv", sep = "")
        } else if (input$data_source == "Map Data") {
          paste("Scottish Agricultural Employees Regional Data - 2025.csv", sep = "")
        }
      },
      content = function(file) {
        if (input$data_source == "Chart Data") {
          write.csv(pivoted_chart_data(), file, row.names = FALSE)
        } else if (input$data_source == "Map Data") {
          write.csv(pivoted_regions_data(), file, row.names = FALSE)
        }
      }
    )
  })
}

# Testing module
content_demo <- function() {
  ui <- fluidPage(employeesMapUI("employees_map_test"))
  server <- function(input, output, session) {
    employeesMapServer("employees_map_test")
  }
  shinyApp(ui, server)
}

content_demo()
