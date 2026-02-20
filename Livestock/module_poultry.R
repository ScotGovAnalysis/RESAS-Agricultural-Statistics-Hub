# File: module_poultry.R
poultryUI <- function(id) {
  ns <- NS(id)
  sidebarLayout(
    sidebarPanel(
      width = 3,
      conditionalPanel(
        condition = "input.tabsetPanel === 'Map'",
        ns = ns,
        radioButtons(
          ns("variable_region"), 
          "Select Variable", 
          choices = c(           
            "Total Poultry" = "Total Poultry",
            "Fowls for producing eggs" = "Fowls for producing eggs",
            "Fowls for breeding" = "Fowls for breeding",
            "Broilers and other table fowls and other poultry" = "Broilers and other table fowls and other poultry"
          )
        )
      ),
      conditionalPanel(
        condition = "input.tabsetPanel === 'Constituency Map'",
        ns = ns,
        radioButtons(
          ns("variable_con"), 
          "Select Variable", 
          choices = c(           
            "Total Poultry" = "Total Poultry (Number)",
            "Fowls for producing eggs" = "Fowls for producing eggs (Number)",
            "Fowls for breeding" = "Fowls for breeding (Number)",
            "Broilers and other table fowls and other poultry" = "Broilers and other table fowls and other poultry (Number)"
          )
        )
      ),
      conditionalPanel(
        condition = "input.tabsetPanel === 'Time Series' || input.tabsetPanel === 'Area Chart'",
        ns = ns,
        selectizeInput(
          ns("timeseries_variables"),
          "Click within the box to select variables",
          choices = unique(number_of_poultry$`Poultry by category`),
          selected = c(
            "Total Fowls For Producing Eggs",
            "Total Fowls For Breeding",
            "Broilers And Other Table Birds",
            "Total Poultry"
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
          choices = c("Map Data" = "map", "Time Series Data" = "timeseries"),
          selected = "map"
        )
      )
    ),
    mainPanel(
      width = 9,
      tabsetPanel(
        id = ns("tabsetPanel"),
        tabPanel("Map", mapUI(ns("map"))),
        tabPanel("Constituency Map", mapConstituenciesUI(ns("map_con"))),
        tabPanel("Time Series", lineChartUI(ns("line"), note_type = 2)),
        tabPanel("Area Chart", areaChartUI(ns("area"), note_type = 2)),
        tabPanel("Data Table", 
                 DTOutput(ns("table")),
                 downloadButton(ns("downloadData"), "Download Data"),
                 generateCensusTableFooter()
        )
      ),
      div(
        style = "margin-top: 20px; padding: 10px; border-top: 1px solid #ddd;",
        HTML("<strong>Note:</strong> Poultry estimates for 2023 are not comparable to previous years due to methodological improvements.")
      )
    )
  )
}


poultryServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    poultry_data <- livestock_subregion %>%
      filter(`Livestock by category` %in% c(
        "Fowls for producing eggs",
        "Fowls for breeding",
        "Broilers and other table fowls and other poultry",
        "Total Poultry"
      )) %>%
      select(-`Scotland total`) %>%
      mutate(across(everything(), as.character)) %>%
      pivot_longer(cols = -`Livestock by category`, names_to = "sub_region", values_to = "value") %>%
      mutate(value = as.numeric(value))
    
    mapServer(
      id = "map",
      data = reactive({
        req(input$variable_region)
        poultry_data %>% filter(`Livestock by category` == input$variable_region)
      }),
      footer = census_footer,
      variable = reactive(input$variable_region),
      title = paste("Poultry distribution by region in Scotland in", census_year),
      legend_title = "Number of poultry"
    )
    
    poultry_const_map <- reactive({
      poultry_constituency %>%         # <— your constituency land use table
        mutate(across(everything(), as.character)) %>%
        pivot_longer(
          cols = -`livestock`,
          names_to = "constituency",
          values_to = "value"
        ) %>% 
        mutate(
          value = if_else(is.na(value), NA_integer_, as.integer(value))
        )
    })
    
    mapConstituenciesServer(
      id = "map_con",
      data = reactive({
        req(input$variable_con)
        poultry_const_map() %>% filter(`livestock` == input$variable_con)
      }),
      unit = "number",
      footer = census_footer,
      variable = reactive(input$variable_con),
      title = paste("Poultry by constituency in Scotland in", census_year),
      legend_title = "Poultry (number)"
    )
    
    chart_data <- reactive({
      req(input$timeseries_variables)
      filtered_data <- number_of_poultry %>%
        select(-`% Change 2025 to 2024`) %>% 
        filter(`Poultry by category` %in% input$timeseries_variables) %>%
        pivot_longer(cols = -`Poultry by category`, names_to = "year", values_to = "value") %>%
        mutate(value = as.numeric(value))
    })
    
    areaChartServer(
      id = "area",
      chart_data = chart_data,
      title = "Number of poultry by category across time",
      yAxisTitle = "Number of poultry (1,000)",
      xAxisTitle = "Year",
      footer = '<div style="font-size: 16px; font-weight: bold;">* Estimates for 2023 are not comparable to previous years due to methodological improvements.<br/><a href="https://www.gov.scot/publications/results-scottish-agricultural-census-june-2025/documents/">Source: Scottish Agricultural Census: June 2025</a></div>',
      x_col = "year",
      y_col = "value"
    )
    
    lineChartServer(
      id = "line",
      chart_data = chart_data,
      title = "Number of poultry by category across time",
      yAxisTitle = "Number of poultry (1,000)",
      xAxisTitle = "Year",
      footer = '<div style="font-size: 16px; font-weight: bold;">* Estimates for 2023 are not comparable to previous years due to methodological improvements.<br/><a href="https://www.gov.scot/publications/results-scottish-agricultural-census-june-2025/documents/">Source: Scottish Agricultural Census: June 2025</a></div>',
      x_col = "year",
      y_col = "value"
    )
    
    output$table <- renderDT({
      req(input$tabsetPanel == "Data Table")
      if (input$table_data == "map") {
        req(input$variable)
        poultry_data %>%
          pivot_wider(names_from = sub_region, values_from = value)  %>%
          mutate(across(where(is.numeric) & !contains("Year"), comma)) %>%
          datatable(
            options = list(
              scrollX = TRUE,  # Enable horizontal scrolling
              pageLength = 20,  # Show 20 entries by default
              autoWidth = TRUE, # Apply column widths
              columnDefs = list(
                list(width = '150px', targets = 1)
              )
            )
          )
      } else {
        number_of_poultry %>%
          select(-`% Change 2025 to 2024`) %>% 
          pivot_longer(cols = -`Poultry by category`, names_to = "year", values_to = "value") %>%
          pivot_wider(names_from = year, values_from = value)  %>%
          mutate(across(where(is.numeric) & !contains("Year"), comma)) %>%
          datatable(
            options = list(
              scrollX = TRUE,  # Enable horizontal scrolling
              pageLength = 20,  # Show 20 entries by default
              autoWidth = TRUE, # Apply column widths
              columnDefs = list(
                list(width = '150px', targets = 1)
              )
            )
          )
      }
    })
    
    output$downloadData <- downloadHandler(
      filename = function() {
        if (input$table_data == "map") {
          paste("Poultry_Map_Data_", Sys.Date(), ".csv", sep = "")
        } else {
          paste("Poultry_Timeseries_Data_", Sys.Date(), ".csv", sep = "")
        }
      },
      content = function(file) {
        data <- if (input$table_data == "map") {
          poultry_data %>%
            filter(`Livestock by category` == input$variable) %>%
            pivot_wider(names_from = sub_region, values_from = value)
        } else {
          number_of_poultry %>%
            pivot_longer(cols = -`Poultry by category`, names_to = "year", values_to = "value") %>%
            pivot_wider(names_from = year, values_from = value)
        }
        write.csv(data, file, row.names = FALSE)
      }
    )
  })
}

# Testing module
poultry_demo <- function() {
  ui <- fluidPage(poultryUI("poultry_test"))
  server <- function(input, output, session) {
    poultryServer("poultry_test")
  }
  shinyApp(ui, server)
}

poultry_demo()