# File: module_pigs.R

pigsUI <- function(id) {
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
            "Total Pigs" = "Total Pigs",
            "Female pigs breeding herd" = "Female pigs breeding herd",
            "All other non-breeding pigs" = "All other non-breeding pigs"
          ))
      ),
      conditionalPanel(
        condition = "input.tabsetPanel === 'Constituency Map'",
        ns = ns,
        radioButtons(
          ns("variable_con"), 
          "Select Variable", 
          choices = c(
            "Total Pigs" = "Total Pigs",
            "Female pigs breeding herd" = "Female pigs breeding herd",
            "All other non-breeding pigs" = "All other non-breeding pigs"
          ))
      ),
      conditionalPanel(
        condition = "input.tabsetPanel === 'Local Authority Map'",
        ns = ns,
        radioButtons(
          ns("variable_uni"), 
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
        req(input$variable_region)
        pigs_data %>% filter(`Livestock by category` == input$variable_region)
      }),
      footer = census_footer,
      variable = reactive(input$variable_region),
      title = paste("Pig distribution by region in Scotland in", census_year),
      legend_title = "Number of pigs"
    )
    
    pig_const_map <- reactive({
      pigs_constituency %>%   
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
        pig_const_map() %>% filter(`livestock` == input$variable_con)
      }),
      unit = "number",
      footer = census_footer,
      variable = reactive(input$variable_con),
      title = paste("Pig distribution by 2026 Scottish Parliamentary Constituency"),
      legend_title = "Pigs (number)"
    )
    
    pig_uni_map <- reactive({
      pigs_unitauth %>%
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
        pig_uni_map() %>% filter(`livestock` == input$variable_uni)
      }),
      unit = "number",
      footer = census_footer,
      variable = reactive(input$variable_uni),
      title = paste("Pig distribution by Local Authority in", census_year),
      legend_title = "Pigs (number)"
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
      
      # Choose which dataset to display
      data <- switch(input$table_data,
                     
                     # ---- Agricultural Region Table ----
                     "map" = {
                       req(input$variable_region)
                       pigs_data %>%
                         pivot_wider(names_from = sub_region, values_from = value) %>%
                         mutate(across(where(is.numeric), comma))
                     },
                     
                     # ---- Timeseries Table ----
                     "timeseries" = {
                       number_of_pigs %>%
                         pivot_longer(cols = -`Pigs by category`,
                                      names_to = "year",
                                      values_to = "value") %>%
                         pivot_wider(names_from = year, values_from = value) %>%
                         mutate(across(where(is.numeric) & !contains("Year"), comma))
                     },
                     
                     # ---- Constituency Table ----
                     "map_con" = {
                       pigs_constituency %>%
                         rename(`Pigs by category` = `livestock`) %>%
                         mutate(across(where(is.numeric), comma))
                     },
                     
                     # ---- Local authority table ----
                     "map_uni" = {
                       pigs_unitauth %>% 
                         rename(`Pigs by category` = `livestock`) %>%
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
      
      # ---- Dynamic filename depending on selected table ----
      filename = function() {
        
        switch(input$table_data,
               "map" = paste0("Pigs_Map_Data_", Sys.Date(), ".csv"),
               "timeseries" = paste0("Pigs_Timeseries_Data_", Sys.Date(), ".csv"),
               "map_con" = paste0("Pigs_Constituency_Data_", Sys.Date(), ".csv"),
               "map_uni" = paste0("Pigs_Local_Authority_Data_", Sys.Date(), ".csv"),
               
               # fallback
               paste0("Downloaded_Data_", Sys.Date(), ".csv")
        )
      },
      
      # ---- Write selected dataset to disk ----
      content = function(file) {
        
        data <- switch(input$table_data,
                       
                       # ---- Agricultural region map ----
                       "map" = {
                         pigs_data %>%
                           filter(`Livestock by category` == input$variable_region) %>%
                           pivot_wider(names_from = sub_region, values_from = value)
                       },
                       
                       # ---- Timeseries ----
                       "timeseries" = {
                         number_of_pigs %>%
                           pivot_longer(
                             cols = -`Pigs by category`,
                             names_to = "year",
                             values_to = "value"
                           ) %>%
                           pivot_wider(names_from = year, values_from = value)
                       },
                       
                       # ---- Constituency map ----
                       "map_con" = {
                          pigs_constituency
                       },
                       
                       # ---- Local authority map ----
                       "map_uni" = {
                         pigs_unitauth
                       }
        )
        
        write.csv(data, file, row.names = FALSE)
      }
    )
  }
  )
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
