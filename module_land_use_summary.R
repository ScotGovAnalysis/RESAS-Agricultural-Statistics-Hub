landUseSummaryUI <- function(id) {
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
          choices = unique(land_use_subregion$`Land use by category`)
        )
      ), 
      conditionalPanel(
        condition = "input.tabsetPanel === 'Summary'",
        ns = ns,
        div(
          style = "font-size: 24px; font-weight: bold;",
          " "
        )
      ), 
      conditionalPanel(
        condition = "input.tabsetPanel === 'Bar Chart'",
        ns = ns,
        checkboxGroupInput(
          ns("variables"), 
          "Choose variables to add to chart", 
          choices = c("Total crops, fallow, and set-aside", "Total grass", "Rough grazing", 
                      "Total sole right agricultural area", "Common grazings"),
          selected = c("Total crops, fallow, and set-aside", "Total grass", "Rough grazing", 
                       "Total sole right agricultural area", "Common grazings")
        )
      ),
      conditionalPanel(
        condition = "input.tabsetPanel === 'Time Series'",
        ns = ns,
        checkboxGroupInput(
          ns("timeseries_variables"),
          "Select Time Series Variables",
          choices = unique(land_use_data$`Crop/Land use`),
          selected = c(
            "Common grazings",
            "Rough grazing",
            "Total crops, fallow, and set-aside",
            "Total grass",
            "Cauliflower",
            "Total sole right agricultural area"
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
      id = ns("mainpanel"),
      width = 9,
      tabsetPanel(
        id = ns("tabsetPanel"),
        tabPanel("Map", mapUI(ns("map"))),
        tabPanel("Bar Chart", barChartUI(ns("bar_chart"))),
        tabPanel("Time Series", lineChartUI(ns("line"))),
        tabPanel("Data Table", 
                 DTOutput(ns("table")),
                 tags$div(
                   style = "margin-top: 20px;",
                   downloadButton(ns("download_data"), "Download Data"),
                   generateCensusTableFooter()
                 )
        )
      )
    )
  )
}


landUseSummaryServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Map data remains unformatted for proper rendering
    land_use_map <- reactive({
      land_use_subregion %>%
        select(-Scotland) %>%
        mutate(across(everything(), as.character)) %>%
        pivot_longer(cols = -`Land use by category`, names_to = "sub_region", values_to = "value") %>%
        mutate(value = as.numeric(value))
    })
    
    # Table data with formatted values for better readability
    table_data <- reactive({
      if (input$table_data == "map") {
        land_use_subregion %>%
          mutate(across(where(is.numeric), comma))  # Format numeric columns with commas
      } else {
        land_use_data %>%
          mutate(across(where(is.numeric), comma))  # Format numeric columns with commas
      }
    })
    
    mapServer(
      id = "map",
      data = reactive({
        req(input$variable)
        land_use_map() %>% filter(`Land use by category` == input$variable)
      }),
      unit = "hectares",
      footer = census_footer,
      variable = reactive(input$variable),
      title = paste("Land use by region in Scotland in", census_year),
      legend_title = "Area (hectares)"
    )
    
    chart_data <- reactive({
      agricultural_area_hectares %>%
        filter(`Crop/Land use` %in% input$variables) %>%
        select(`Crop/Land use`, `2023 Area`) %>%
        rename(Variable = `Crop/Land use`, Value = `2023 Area`)
    })
    
    timeseries_data <- reactive({
      req(input$timeseries_variables)
      land_use_data %>%
        filter(`Crop/Land use` %in% input$timeseries_variables) %>%
        pivot_longer(cols = -`Crop/Land use`, names_to = "year", values_to = "value") %>%
        mutate(year = as.numeric(year))  # Ensure year is numeric
    })
    

    
    barChartServer(
      id = "bar_chart",
      chart_data = chart_data,
      title = paste("Agricultural area by land use type in", census_year),
      yAxisTitle = "Area (1,000 hectares)",
      xAxisTitle = "Land use type",
      unit = "hectares",
      footer = census_footer,
      x_col = "Variable",
      y_col = "Value",
      tooltip_unit= reactive("hectares")
    )
    
    lineChartServer(
      id = "line",
      chart_data = timeseries_data,
      title = "Land use over time",
      yAxisTitle = "Area of land use (1,000 hectares)",
      xAxisTitle = "Year",
      unit = "hectares",
      footer = census_footer,
      x_col = "year",
      y_col = "value"
    )
    
    # Render the data table with formatted values
    output$table <- renderDT({
      datatable(
        table_data(),
        options = list(
          scrollX = TRUE,  # Enable horizontal scrolling
          pageLength = 20  # Show 20 entries by default
        )
      )
    })
    
    # Download handler with formatted values
    output$download_data <- createDownloadHandler(
      input = input,
      file_map_name = "Land Use Subregion Data 2023.xlsx",
      file_timeseries_name = "Land Use Timeseries Data 2012 to 2023.xlsx",
      map_data = table_data(),  # Use the formatted data for download
      timeseries_data = table_data()  # Use the formatted data for download
    )
  })
}


land_use_demo <- function() {
  ui <- fluidPage(landUseSummaryUI("land_use_test"))
  server <- function(input, output, session) {
    landUseSummaryServer("land_use_test")
  }
  shinyApp(ui, server)
}

land_use_demo()
