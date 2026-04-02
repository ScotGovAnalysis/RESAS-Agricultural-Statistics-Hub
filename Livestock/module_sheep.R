# File: module_sheep.R
sheepUI <- function(id) {
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
            "Total Sheep" = "Total Sheep",
            "Ewes for breeding" = "Ewes for breeding",
            "Other sheep 1 year and over for breeding" = "Other sheep 1 year and over for breeding",
            "Rams for service" = "Rams for service",
            "Lambs" = "Lambs"
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
        condition = "input.tabsetPanel === 'Local Authority Map'",
        ns = ns,
        radioButtons(
          ns("variable_uni"), 
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
          choices = c("Agricultural Region Data" = "map", 
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
    
    # Agricultural region map
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
        req(input$variable_region)
        sheep_data %>% filter(`Livestock by category` == input$variable_region)
      }),
      footer = census_footer,
      variable = reactive(input$variable_region),
      title = paste("Sheep distribution by region in Scotland in", census_year),
      legend_title = "Number of sheep"
    )
    
    # Constituency map
    sheep_const_map <- reactive({
      sheep_constituency %>%       
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
        sheep_const_map() %>% filter(`livestock` == input$variable_con)
      }),
      unit = "number",
      footer = census_footer,
      variable = reactive(input$variable_con),
      title = paste("Sheep distribution by 2026 Scottish Parliamentary Constituency"),
      legend_title = "Sheep (number)"
    )
    
    # Local Authority map
    sheep_uni_map <- reactive({
      sheep_unitauth %>%        
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
        sheep_uni_map() %>% filter(`livestock` == input$variable_uni)
      }),
      unit = "number",
      footer = census_footer,
      variable = reactive(input$variable_uni),
      title = paste("Sheep distribution by local authority", census_year),
      legend_title = "Sheep (number)"
    )
    
    # Time series
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
      
      # Choose which dataset to display
      data <- switch(input$table_data,
                    
                    # Agricultural region table
                    "map" = {
                      req(input$variable_region)
                      sheep_data %>%
                        pivot_wider(names_from = sub_region, values_from = value) %>%
                        mutate(across(where(is.numeric), comma))
                    },
                    
                    # Timeseries table
                    "timeseries" = {
                      number_of_sheep %>%
                        pivot_longer(cols = -`Sheep by category`,
                                     names_to = "year",
                                     values_to = "value") %>%
                        pivot_wider(names_from = year, values_from = value) %>%
                        mutate(across(where(is.numeric) & !contains("Year"), comma))
                    },
                    
                    # Constituency Table
                    "map_con" = {
                      sheep_constituency %>%
                        rename(`Sheep by category` = `livestock`) %>%
                        mutate(across(where(is.numeric), comma))
                    },
                    
                    # Local authority table
                    "map_uni" = {
                      sheep_unitauth %>% 
                        rename(`Sheep by category` = `livestock`) %>%
                        mutate(across(where(is.numeric), comma))                    
                    }
      )
      
      # Render the chosen table
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
      
      # Dynamic filename depending on selected table
      filename = function() {
        switch(input$table_data,
               "map" = paste0("Sheep_Map_Data_", Sys.Date(), ".csv"),
               "timeseries" = paste0("Sheep_Timeseries_Data_", Sys.Date(), ".csv"),
               "map_con" = paste0("Sheep_Constituency_Data_", Sys.Date(), ".csv"),
               "map_uni" = paste0("Sheep_Local_Authority_Data_", Sys.Date(), ".csv"),
               
               # fallback
               paste0("Downloaded_Data_", Sys.Date(), ".csv")
        )
      },
      
      # ---- Write selected dataset to disk ----
      content = function(file) {
        
        data <- switch(input$table_data,
                       
                       # ---- Agricultural region map ----
                       "map" = {
                         sheep_data %>%
                           filter(`Livestock by category` == input$variable_region) %>%
                           pivot_wider(names_from = sub_region, values_from = value)
                       },
                       
                       # ---- Timeseries ----
                       "timeseries" = {
                         number_of_sheep %>%
                           pivot_longer(
                             cols = -`Sheep by category`,
                             names_to = "year",
                             values_to = "value"
                           ) %>%
                           pivot_wider(names_from = year, values_from = value)
                       },
                       
                       # ---- Constituency ----
                       "map_con" = {
                         sheep_constituency
                       },
                       
                       # ---- Local authority ----
                       "map_uni" = {
                         sheep_unitauth
                       }
        )
        
        write.csv(data, file, row.names = FALSE)
      }
    )
  }
)
}


sheep_demo <- function() {
  ui <- fluidPage(sheepUI("sheep_test"))
  server <- function(input, output, session) {
    sheepServer("sheep_test")
  }
  shinyApp(ui, server)
}

sheep_demo()
