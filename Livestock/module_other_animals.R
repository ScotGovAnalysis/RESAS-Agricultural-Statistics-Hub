# File: module_other_animals.R

otherAnimalsUI <- function(id) {
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
          choices = c(
            "Goats and kids" = "Goats and kids",
            "Deer" = "Deer",
            "Horses" = "Horses",
            "Donkeys" = "Donkeys",
            "Camelids" = "Camelids",
            "Beehives" = "Beehives"
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
            "Goats and kids" = "Goats and kids (Number)",
            "Deer" = "Deer (Number)",
            "Horses" = "Horses (Number)",
            "Donkeys" = "Donkeys (Number)",
            "Camelids" = "Camelids (Number)",
            "Beehives" = "Beehives (Number)"
          )
        )
      ),
      conditionalPanel(
        condition = "input.tabsetPanel === 'Time Series'",
        ns = ns,
        checkboxGroupInput(
          ns("timeseries_variables"),
          "Select Time Series Variables",
          choices = c(
            "Goats",
            "Deer",
            "Horses",
            "Donkeys",
            "Camelids",
            "Beehives"
          ),
          selected = c(
            "Goats",
            "Deer",
            "Horses",
            "Donkeys",
            "Camelids",
            "Beehives"
          )
        )
      ),
      conditionalPanel(
        condition = "input.tabsetPanel === 'Data Table'",
        ns = ns,
        radioButtons(
          ns("table_data"),
          "Select Data to Display",
          choices = c("Map Data" = "map", "Chart Data" = "timeseries"),
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
        tabPanel("Time Series", lineChartUI(ns("line"))),
        tabPanel("Data Table", 
                 DTOutput(ns("table")),
                 downloadButton(ns("downloadData"), "Download Data"),
                 generateCensusTableFooter()

                 
        )
      )
    )
  )
}

otherAnimalsServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    other_animals_data <- livestock_subregion %>%
      filter(`Livestock by category` %in% c(
        "Goats and kids",
        "Deer",
        "Horses",
        "Donkeys",
        "Camelids",
        "Beehives"
      )) %>%
      select(-`Scotland total`) %>%
      mutate(across(everything(), as.character)) %>%
      pivot_longer(cols = -`Livestock by category`, names_to = "sub_region", values_to = "value") %>%
      mutate(value = as.numeric(value))
    
    mapServer(
      id = "map",
      data = reactive({
        req(input$variable_region)
        other_animals_data %>% filter(`Livestock by category` == input$variable_region)
      }),
      footer = census_footer,
      variable = reactive(input$variable_region),
      title = paste("Other animals distribution by region in Scotland in", census_year),
      legend_title = "Number of animals"
    )
    
    other_const_map <- reactive({
      other_animals_constituency %>%         # <— your constituency land use table
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
        other_const_map() %>% filter(`livestock` == input$variable_con)
      }),
      unit = "number",
      footer = census_footer,
      variable = reactive(input$variable_con),
      title = paste("Other animals distribution by 2026 Scottish Parliamentary Constituency"),
      legend_title = "Animals (number)"
    )
    
    chart_data <- reactive({
      req(input$timeseries_variables)
      filtered_data <- number_of_other_livestock %>%
        mutate(across(-`Livestock by category`, as.numeric)) %>% 
        select(-last_col()) %>% 
        filter(`Livestock by category` %in% input$timeseries_variables) %>%
        pivot_longer(cols = -`Livestock by category`, names_to = "year", values_to = "value") %>%
        mutate(year = as.numeric(year))  # Ensure year is numeric
      filtered_data
    })
    
    lineChartServer(
      id = "line",
      chart_data = chart_data,
      title = "Number of other animals over time",
      yAxisTitle = "Number of animals (1,000)",
      xAxisTitle = "Year",
      footer = census_footer,
      x_col = "year",
      y_col = "value"
    )
    
    output$table <- renderDT({
      req(input$tabsetPanel == "Data Table")
      if (input$table_data == "map") {
        req(input$variable_region)
        other_animals_data %>%
          pivot_wider(names_from = sub_region, values_from = value)  %>%
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
        number_of_other_livestock %>%
          pivot_longer(cols = -`Livestock by category`, names_to = "year", values_to = "value") %>%
          pivot_wider(names_from = year, values_from = value)  %>%
          mutate(across(where(is.numeric) & !contains("Year"), comma)) %>%
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
          paste("Other_Animals_Map_Data_", Sys.Date(), ".csv", sep = "")
        } else {
          paste("Other_Animals_Timeseries_Data_", Sys.Date(), ".csv", sep = "")
        }
      },
      content = function(file) {
        data <- if (input$table_data == "map") {
          other_animals_data %>%
            filter(`Livestock by category` == input$variable) %>%
            pivot_wider(names_from = sub_region, values_from = value)
        } else {
          number_of_other_livestock %>%
            pivot_longer(cols = -`Livestock by category`, names_to = "year", values_to = "value") %>%
            pivot_wider(names_from = year, values_from = value)
        }
        write.csv(data, file, row.names = FALSE)
      }
    )
  })
}

# Testing module
other_animals_demo <- function() {
  ui <- fluidPage(otherAnimalsUI("other_animals_test"))
  server <- function(input, output, session) {
    otherAnimalsServer("other_animals_test")
  }
  shinyApp(ui, server)
}

other_animals_demo()
