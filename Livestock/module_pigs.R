# File: module_pigs.R

pigsUI <- function(id) {
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
            "Total Pigs" = "Total Pigs",
            "Female pigs breeding herd" = "Female pigs breeding herd",
            "All other non-breeding pigs" = "All other non-breeding pigs"
          ))
      ),
      conditionalPanel(
        condition = "input.tabsetPanel === 'Time Series' || input.tabsetPanel === 'Area Chart'",
        ns = ns,
        selectizeInput(
          ns("timeseries_variables"),
          "Click within the box to select variables (not all shown)",
          choices = unique(number_of_pigs$`Pigs by category`),
          selected = c(
            "Total Breeding Herd",
            "80 Kg Liveweight And Over",
            "50 Kg And Under 80 Kg Liveweight",
            "Under 50 Kg Liveweight"
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
        tabPanel("Time Series", lineChartUI(ns("line"), note_type = 2)),
        tabPanel("Area Chart", areaChartUI(ns("area"), note_type = 2)),
        tabPanel("Data Table", 
                 DTOutput(ns("table")),
                 downloadButton(ns("downloadData"), "Download Data"),
                 generateCensusTableFooter()

        )
      )
    )
  )
}

pigsServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    pigs_data <- livestock_subregion %>%
      filter(`Livestock by category` %in% c(
        "Female pigs breeding herd",
        "All other non-breeding pigs",
        "Total Pigs"
      )) %>%
      select(-`Scotland total`) %>%
      mutate(across(everything(), as.character)) %>%
      pivot_longer(cols = -`Livestock by category`, names_to = "sub_region", values_to = "value") %>%
      mutate(value = as.numeric(value))  # Ensure value is numeric
    
    mapServer(
      id = "map",
      data = reactive({
        req(input$variable)
        pigs_data %>% filter(`Livestock by category` == input$variable)
      }),
      footer = census_footer,
      variable = reactive(input$variable),
      title = paste("Pig distribution by region in Scotland in", census_year),
      legend_title = "Number of pigs"
    )
    
    chart_data <- reactive({
      req(input$timeseries_variables)
      
      filtered_data <- number_of_pigs %>% select (-last_col()) %>%  # remove %change column
        mutate(across(-`Pigs by category`, as.numeric)) %>%
        filter(`Pigs by category` %in% input$timeseries_variables) %>%
        pivot_longer(cols = -`Pigs by category`, names_to = "year", values_to = "value") %>%
        mutate(value = as.numeric(value))  # Ensure value is numeric
      filtered_data
    })
    
    areaChartServer(
      id = "area",
      chart_data = chart_data,
      title = "Number of pigs by category over time",
      yAxisTitle = "Number of pigs (1,000)",
      xAxisTitle = "Year",
      footer = census_footer,
      x_col = "year",
      y_col = "value"
    )
    
    lineChartServer(
      id = "line",
      chart_data = chart_data,
      title = "Number of pigs by category over time",
      yAxisTitle = "Number of pigs (1,000)",
      xAxisTitle = "Year",
      footer = census_footer,
      x_col = "year",
      y_col = "value"
    )
    
    output$table <- renderDT({
      req(input$tabsetPanel == "Data Table")
      if (input$table_data == "map") {
        req(input$variable)
        pigs_data %>%
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
        number_of_pigs %>%
          pivot_longer(cols = -`Pigs by category`, names_to = "year", values_to = "value") %>%
          pivot_wider(names_from = year, values_from = value)  %>%
          mutate(across(where(is.numeric) & !contains("Year"), comma))%>%
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
          paste("Pigs_Map_Data_", Sys.Date(), ".csv", sep = "")
        } else {
          paste("Pigs_Timeseries_Data_", Sys.Date(), ".csv", sep = "")
        }
      },
      content = function(file) {
        data <- if (input$table_data == "map") {
          pigs_data %>%
            filter(`Livestock by category` == input$variable) %>%
            pivot_wider(names_from = sub_region, values_from = value)
        } else {
          number_of_pigs %>%
            pivot_longer(cols = -`Pigs by category`, names_to = "year", values_to = "value") %>%
            pivot_wider(names_from = year, values_from = value)
        }
        write.csv(data, file, row.names = FALSE)
      }
    )
  })
}

#Testing module
pigs_demo <- function() {
ui <- fluidPage(pigsUI("pigs_test"))
server <- function(input, output, session) {
pigsServer("pigs_test")
}
shinyApp(ui, server)
}
 
pigs_demo()
