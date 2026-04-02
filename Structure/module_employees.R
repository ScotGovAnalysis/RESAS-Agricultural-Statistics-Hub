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

categories_con <- c("Regular Full-Time Staff Total", 
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
        tabPanel("Agricultural Region Map", mapUI(ns("map")), value = "map"),
        tabPanel("Constituency Map", mapConstituenciesUI(ns("map_con")), value = "map_con"),
        tabPanel("Local Authority Map", mapUnitaryUI(ns("map_uni")), value = "map_uni"),
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
    ########## CONSTITUENCY MAP #############
    employee_const_map <- reactive({
      workforce_constituency %>%
        mutate(across(everything(), as.character)) %>%
        pivot_longer(
          cols = -`workforce`,
          names_to = "constituency",
          values_to = "value"
        ) %>% 
        mutate(
          value = if_else(is.na(value), NA_integer_, as.integer(value))
        )
    })
    ############# LOCAL AUTHORITY MAP #################
    employee_unitary_map <- reactive({
      workforce_unitauth %>%        
        mutate(across(everything(), as.character)) %>%
        pivot_longer(
          cols = -`workforce`,
          names_to = "unitauth",
          values_to = "value"
        ) %>% 
        mutate(
          value = if_else(is.na(value), NA_integer_, as.integer(value))
        )
    })
    
    ########### Timeseries ##############
    occupiers_employees <- occupiers_employees %>%
      mutate(across(starts_with("20"), as.numeric))
    
    
    # Reactive data for the time series chart
    chart_data <- reactive({
      occupiers_employees %>%
        pivot_longer(cols = -`Occupiers and employees by category`, names_to = "Year", values_to = "Value") %>%
        mutate(Year = as.numeric(Year)) %>%
        filter(!grepl("occupiers", `Occupiers and employees by category`, ignore.case = TRUE))
    })
    
    ######## RADIO BUTTONS #############
    output$sidebar_ui <- renderUI({
      req(input$tabs)
      if (input$tabs == "map") {
        radioButtons(ns("variable_region"), "Select Variable", choices = categories)
      } else if (input$tabs == "map_con") {
        radioButtons(ns("variable_con"), "Select Variable", choices = categories_con)
      } else if (input$tabs == "map_uni") {
        radioButtons(ns("variable_uni"), "Select Variable", choices = categories_con)
      } else if (input$tabs == "data_table") {
        radioButtons(ns("data_source"), "Choose data to show:", choices = c("Time Series Data", "Agricultural Region Data", "Constituency Data", "Local Authority Data"))
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
    
    ############# DATA TABLES #################
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
    
    con_data <- reactive({workforce_constituency %>%
        rename(`Occupiers and employees by category` = workforce)})
    
    uni_data <- reactive({workforce_unitauth %>%
        rename(`Occupiers and employees by category` = workforce)})
    
    ############# Time series chart ################
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
    
    ############ AGRICULTURAL REGION MAP ##############
    mapServer(
      id = "map",
      data = reactive({
        req(input$variable_region)
        filtered_regions_data %>%
          filter(`Occupiers and employees by category` == input$variable_region)
      }),
      unit = "employees",
      footer = '<div style="font-size: 16px; font-weight: bold;"><a href="https://www.gov.scot/publications/results-from-the-scottish-agricultural-census-june-2025/">Source: Scottish Agricultural Census: June 2025</a></div>',
      variable = reactive(input$variable_region),
      title = paste("Agricultural employees by region in Scotland in", census_year),
      legend_title = "Number of employees"
    )  
    
    
    ########### CONSTITUENCIES MAP ###########
    
    mapConstituenciesServer(
      id = "map_con",
      data = reactive({
        req(input$variable_con)
        employee_const_map() %>% filter(`workforce` == input$variable_con)
      }),
      unit = "employees",
      footer = census_footer,
      variable = reactive(input$variable_con),
      title = paste("Agricultural employees by 2026 Scottish Parliamentary Constituency"),
      legend_title = "Employees (Number)"
    )
    
    ############ LOCAL AUTHORITY MAP #############
    
    mapUnitaryServer(
      id = "map_uni",
      data = reactive({
        req(input$variable_uni)
        employee_unitary_map() %>% filter(`workforce` == input$variable_uni)
      }),
      unit = "employees",
      footer = census_footer,
      variable = reactive(input$variable_uni),
      title = paste("Agricultural employees by local authority in", census_year),
      legend_title = "Employees (Number)"
    )
    
    
    ########### DATA TABLES ##############
    output$data_table <- renderDT({
      req(input$data_source)
      
      # Inline formatting code (round + commas + keep "c")
      fmt <- function(df) {
        df %>%
          mutate(across(
            everything(),
            ~ {
              x <- as.character(.x)
              nums <- readr::parse_number(x)   # gets number or NA for "c"
              ifelse(
                is.na(nums),
                x,                              # keep "c" or NA
                scales::comma(round(nums, 0))   # round + commas
              )
            }
          ))
      }
      
      if (input$data_source == "Time Series Data") {
        datatable(fmt(pivoted_chart_data()), options = list(
          scrollX = TRUE,
          pageLength = 26
        ))
        
      } else if (input$data_source == "Agricultural Region Data") {
        datatable(fmt(pivoted_regions_data()), options = list(
          scrollX = TRUE,
          pageLength = 10
        ))
        
      } else if (input$data_source == "Constituency Data") {
        datatable(fmt(con_data()), options = list(
          scrollX = TRUE,
          pageLength = 20
        ))
        
      } else if (input$data_source == "Local Authority Data") {
        datatable(fmt(uni_data()), options = list(
          scrollX = TRUE,
          pageLength = 20
        ))
      }
    })
    
    # ---------------------
    # DOWNLOAD HANDLER
    # ---------------------
    
    output$downloadData <- downloadHandler(
      filename = function() {
        if (input$data_source == "Time Series Data") {
          "Scottish Agricultural Employees Time Series Data - 2013 to 2025.csv"
        } else if (input$data_source == "Agricultural Region Data") {
          "Scottish Agricultural Employees Agricultural Regional Data - 2025.csv"
        } else if (input$data_source == "Constituency Data") {
          "Scottish Agricultural Employees Constituency Data - 2025.csv"
        } else if (input$data_source == "Local Authority Data") {
          "Scottish Agricultural Employees Local Authority Data - 2025.csv"
        }
      },
      
      content = function(file) {
        
        # apply same formatting to the downloaded file
        fmt <- function(df) {
          df %>%
            mutate(across(
              everything(),
              ~ {
                x <- as.character(.x)
                nums <- readr::parse_number(x)
                ifelse(
                  is.na(nums),
                  x,
                  scales::comma(round(nums, 0))
                )
              }
            ))
        }
        
        if (input$data_source == "Time Series Data") {
          write.csv(fmt(pivoted_chart_data()), file, row.names = FALSE)
          
        } else if (input$data_source == "Agricultural Region Data") {
          write.csv(fmt(pivoted_regions_data()), file, row.names = FALSE)
          
        } else if (input$data_source == "Constituency Data") {
          write.csv(fmt(con_data()), file, row.names = FALSE)
          
        } else if (input$data_source == "Local Authority Data") {
          write.csv(fmt(uni_data()), file, row.names = FALSE)
        }
      }
    )
  }
  )
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
