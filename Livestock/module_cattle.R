# File: module_cattle.R
cattleUI <- function(id) {
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
            "Total Cattle" = "Total Cattle",
            "Total Female Dairy Cattle" = "Total Female Dairy Cattle",
            "Total Female Beef Cattle" = "Total Female Beef Cattle",
            "Total Male Cattle" = "Total Male Cattle",
            "Total Calves" = "Total Calves"
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
            "Total Cattle" = "Total Cattle (Number)",
            "Total Female Dairy Cattle" = "Total Female Dairy Cattle (Number)",
            "Total Female Beef Cattle" = "Total Female Beef Cattle (Number)",
            "Total Male Cattle" = "Total Male Cattle (Number)",
            "Total Calves" = "Total Calves (Number)"
          )
        )
      ),
      conditionalPanel(
        condition = "input.tabsetPanel === 'Local Authority Map'",
        ns = ns,
        radioButtons(
          ns("variable_uni"), 
          "Select Variable", 
          choices = c(
            "Total Cattle" = "Total Cattle (Number)",
            "Total Female Dairy Cattle" = "Total Female Dairy Cattle (Number)",
            "Total Female Beef Cattle" = "Total Female Beef Cattle (Number)",
            "Total Male Cattle" = "Total Male Cattle (Number)",
            "Total Calves" = "Total Calves (Number)"
          )
        )
      ),
      conditionalPanel(
        condition = "input.tabsetPanel === 'Time Series' || input.tabsetPanel === 'Area Chart'",
        ns = ns,
        selectizeInput(
          ns("timeseries_variables"),
          "Click within the box to select variables (not all shown)",
          choices = unique(number_of_cattle$`Cattle by category`),
          selected = c(
            "Total Female Dairy Cattle",
            "Total Female Beef Cattle",
            "Total Male Cattle",
            "Total Calves"
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
          choices = c("Map Data" = "map",
                      "Time Series Data" = "timeseries",
                      "Constituency Data" = "map_con",
                      "Local Authority Data" = "map_uni"),
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
        tabPanel("Local Authority Map", mapUnitaryUI(ns("map_uni"))),
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


cattleServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Processing data for Map
    cattle_data <- livestock_subregion %>%
      filter(`Livestock by category` %in% c(
        "Total Female Dairy Cattle",
        "Total Female Beef Cattle",
        "Total Male Cattle",
        "Total Calves",
        "Total Cattle"
      )) %>%
      select(-`Scotland total`) %>%
      mutate(across(everything(), as.character)) %>%
      pivot_longer(cols = -`Livestock by category`, names_to = "sub_region", values_to = "value") %>%
      mutate(value = as.numeric(value))
    
    # Map Server
    mapServer(
      id = "map",
      data = reactive({
        req(input$variable_region)
        cattle_data %>% filter(`Livestock by category` == input$variable_region)
      }),
      footer = census_footer,
      variable = reactive(input$variable_region),
      title = paste("Cattle distribution by region in Scotland in", census_year),
      legend_title = "Number of cattle"
    )
    
    cattle_const_map <- reactive({
      cattle_constituency %>%         # <— your constituency land use table
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
        cattle_const_map() %>% filter(`livestock` == input$variable_con)
      }),
      unit = "number",
      footer = census_footer,
      variable = reactive(input$variable_con),
      title = paste("Cattle distribution by 2026 Scottish Parliamentary Constituency"),
      legend_title = "Cattle (number)"
    )
    
    cattle_uni_map <- reactive({
      cattle_unitauth %>% 
        mutate(across(everything(), as.character)) %>%
        pivot_longer(
          cols = -`livestock`,
          names_to = "unitauth",
          values_to = "value"
        ) %>% 
        mutate(
          value = if_else(is.na(value), NA_integer_, as.integer(value))
        )
    })
    
    mapUnitaryServer(
      id = "map_uni",
      data = reactive({
        req(input$variable_uni)
        cattle_uni_map() %>% filter(`livestock` == input$variable_uni)
      }),
      unit = "number",
      footer = census_footer,
      variable = reactive(input$variable_uni),
      title = paste("Cattle distribution by local authority in", census_year),
      legend_title = "Cattle (number)"
    )
    # Processing data for Area Chart and Time Series
    chart_data <- reactive({
      req(input$timeseries_variables)
      filtered_data <- number_of_cattle %>% select (-last_col()) %>%
        filter(`Cattle by category` %in% input$timeseries_variables) %>%
        pivot_longer(cols = -`Cattle by category`, names_to = "year", values_to = "value") %>%
        mutate(year = as.numeric(year))  # Ensure year is numeric
      filtered_data
    })
    
    # Area Chart Server
    areaChartServer(
      id = "area",
      chart_data = chart_data,
      title = "Number of cattle by category across time",
      yAxisTitle = "Number of cattle (1,000)",
      xAxisTitle = "Year",
      footer = census_footer,
      x_col = "year",
      y_col = "value"
    )
    
    # Line Chart Server
    lineChartServer(
      id = "line",
      chart_data = chart_data,
      title = "Number of cattle by category across time",
      yAxisTitle = "Number of cattle (1,000)",
      xAxisTitle = "Year",
      footer = census_footer,
      x_col = "year",
      y_col = "value"
    )
    
    # Data Table output
    
    # Data Table output
    output$table <- renderDT({
      req(input$tabsetPanel == "Data Table")
      
      # -------------------------
      # Select which dataset to show
      # -------------------------
      data <- switch(input$table_data,
                     
                     # -------------------
                     # 1. MAP data (existing)
                     # -------------------
                     "map" = {
                       cattle_data %>%
                         pivot_wider(names_from = sub_region, values_from = value) %>%
                         mutate(across(where(is.numeric) & !contains("Year"), comma))
                     },
                     
                     # -------------------
                     # 2. TIME-SERIES data (existing)
                     # -------------------
                     "timeseries" = {
                       number_of_cattle %>%
                         pivot_longer(cols = -`Cattle by category`,
                                      names_to = "year",
                                      values_to = "value") %>%
                         pivot_wider(names_from = year, values_from = value) %>%
                         mutate(across(where(is.numeric) & !contains("Year"), comma))
                     },
                     
                     # -------------------
                     # 3. Constituency Table
                     # -------------------
                     
                     "map_con" = {
                       cattle_constituency %>%
                         rename(`Cattle by category` = `livestock`) %>%
                         mutate(across(
                           where(is.character),
                           ~ ifelse(grepl("^\\d+$", .x), comma(as.numeric(.x)), .x)
                         ))
                     },
                     
                     
                     # -------------------
                     # 4. Local authority table
                     # -------------------
                     "map_uni" = {
                       cattle_unitauth %>% 
                         rename(`Cattle by category` = `livestock`) %>%
                         mutate(across(
                           where(is.character),
                           ~ ifelse(grepl("^\\d+$", .x), comma(as.numeric(.x)), .x)
                         ))                     }
      )
      
      # -------------------------
      # Render the chosen table
      # -------------------------
      datatable(
        data,
        options = list(
          scrollX = TRUE,
          autoWidth = TRUE,
          pageLength = 20,
          columnDefs = list(
            list(width = '200px', targets = 1)
          )
        )
      )
    })
    
    # Data Download Handler
    output$downloadData <- downloadHandler(
      filename = function() {
        if (input$table_data == "map") {
          paste("Cattle_Map_Data_", Sys.Date(), ".csv", sep = "")
        } else {
          paste("Cattle_Timeseries_Data_", Sys.Date(), ".csv", sep = "")
        }
      },
      content = function(file) {
        data <- if (input$table_data == "map") {
          cattle_data %>%
            filter(`Livestock by category` == input$variable_region) %>%
            pivot_wider(names_from = sub_region, values_from = value)
        } else {
          number_of_cattle %>%
            pivot_longer(cols = -`Cattle by category`, names_to = "year", values_to = "value") %>%
            pivot_wider(names_from = year, values_from = value)
        }
        write.csv(data, file, row.names = FALSE)
      }
    )
  })
}


cattle_demo <- function() {
  ui <- fluidPage(cattleUI("cattle_test"))
  server <- function(input, output, session) {
    cattleServer("cattle_test")
  }
  shinyApp(ui, server)
}

cattle_demo()
