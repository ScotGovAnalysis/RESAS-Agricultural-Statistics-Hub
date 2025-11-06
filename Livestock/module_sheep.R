# File: module_sheep.R


sheepUI <- function(id) {
  ns <- NS(id)
  sidebarLayout(
    sidebarPanel(
      width = 3,
      conditionalPanel(
        condition = "input.tabsetPanel === 'Map'",
        ns = ns,
        radioButtons(
          ns("variable"), 
          "Select Variable", 
          choices = c(
            "Total Sheep" = "Total Sheep",
            "Ewes for breeding" = "Ewes for breeding",
            "Other sheep 1 year and over for breeding" = "Other sheep 1 year and over for breeding",
            "Rams for service" = "Rams for service",
            "Lambs" = "Lambs"
          )
        )
      )
      ,
      conditionalPanel(
        condition = "input.tabsetPanel === 'Time Series' || input.tabsetPanel === 'Area Chart'",
        ns = ns,
        checkboxGroupInput(
          ns("timeseries_variables"),
          "Select Time Series Variables",
          choices = unique(number_of_sheep$`Sheep by category`),
            # "Ewes for breeding",
            # "Other sheep one year old and over for breeding",
            # "Rams for service",
            # "Total other sheep 1 year and over",
            # "Lambs",
            # "Total Sheep",
            # "Other"
          selected = c(
            "Ewes Used For Breeding In Previous Season",
            "Sheep For Breeding Aged 1 Year And Over",
            "Rams To Be Used For Service",
            "Total Other Sheep 1 Year And Over",
            "Lambs"
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
        tabPanel("Time Series", lineChartUI(ns("line"))),
        tabPanel("Area Chart", areaChartUI(ns("area"))),
        tabPanel("Data Table", 
                 DTOutput(ns("table")),
                 downloadButton(ns("downloadData"), "Download Data"),
                 generateCensusTableFooter()

        )
      )
    )
  )
}

sheepServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    sheep_data <- livestock_subregion %>%
      filter(`Livestock by category` %in%   
        c("Ewes for breeding",
        "Other sheep 1 year and over for breeding",
        "Rams for service",
        "Lambs",
        "Total Sheep"))%>%
      select(-`Scotland total`) %>%
      mutate(across(everything(), as.character)) %>%
      pivot_longer(cols = -`Livestock by category`, names_to = "sub_region", values_to = "value") %>%
      mutate(value = as.numeric(value))
    
    mapServer(
      id = "map",
      data = reactive({
        req(input$variable)
        sheep_data %>% filter(`Livestock by category` == input$variable)
      }),
      footer = census_footer,
      variable = reactive(input$variable),
      title = paste("Sheep distribution by region in Scotland in", census_year),
      legend_title = "Number of sheep"
    )
    
    chart_data <- reactive({
      req(input$timeseries_variables)
      filtered_data <- number_of_sheep %>% select (-last_col()) %>%  # remove %change column
        filter(`Sheep by category` %in% input$timeseries_variables) %>%
        pivot_longer(cols = -`Sheep by category`, names_to = "year", values_to = "value") %>%
        mutate(year = as.numeric(year))  # Ensure year is numeric
      filtered_data
    })
    
    areaChartServer(
      id = "area",
      chart_data = chart_data,
      title = "Number of sheep by category across time",
      yAxisTitle = "Number of sheep (1,000)",
      xAxisTitle = "Year",
      footer = census_footer,
      x_col = "year",
      y_col = "value"
    )
    
    lineChartServer(
      id = "line",
      chart_data = chart_data,
      title = "Number of sheep by category across time",
      yAxisTitle = "Number of sheep (1,000)",
      xAxisTitle = "Year",
      footer = census_footer,
      x_col = "year",
      y_col = "value"
    )
    
    output$table <- renderDT({
      req(input$tabsetPanel == "Data Table")
      if (input$table_data == "map") {
        req(input$variable)
        sheep_data %>%
          pivot_wider(names_from = sub_region, values_from = value) %>%
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
        number_of_sheep %>%
          pivot_longer(cols = -`Sheep by category`, names_to = "year", values_to = "value") %>%
          pivot_wider(names_from = year, values_from = value) %>%
          mutate(across(where(is.numeric) & !contains("Year"), comma)) %>% 
          datatable(
            options = list(
              scrollX = TRUE,  # Enable horizontal scrolling
              pageLength = 20,  # Show 20 entries by default
              autoWidth = TRUE, # Apply column widths
              columnDefs = list(
                list(width = '150px', targets = 1))
            )
          )
      }
    })
    
    output$downloadData <- downloadHandler(
      filename = function() {
        if (input$table_data == "map") {
          paste("Sheep_Map_Data_", Sys.Date(), ".csv", sep = "")
        } else {
          paste("Sheep_Timeseries_Data_", Sys.Date(), ".csv", sep = "")
        }
      },
      content = function(file) {
        data <- if (input$table_data == "map") {
          sheep_data %>%
            filter(`Livestock by category` == input$variable) %>%
            pivot_wider(names_from = sub_region, values_from = value) %>%
            mutate(across(where(is.numeric) & !contains("Year"), comma))
        } else {
          number_of_sheep %>%
            pivot_longer(cols = -`Sheep by category`, names_to = "year", values_to = "value") %>%
            pivot_wider(names_from = year, values_from = value) %>%
            mutate(across(where(is.numeric) & !contains("Year"), comma))
        }
        write.csv(data, file, row.names = FALSE)
      }
    )
  })
}

sheep_demo <- function() {
  ui <- fluidPage(sheepUI("sheep_test"))
  server <- function(input, output, session) {
    sheepServer("sheep_test")
  }
  shinyApp(ui, server)
}

sheep_demo()
