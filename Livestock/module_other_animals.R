# File: module_other_animals.R

otherAnimalsUI <- function(id) {
  ns <- NS(id)
  sidebarLayout(
    sidebarPanel(
      width = 3,
      
      # Maps
      conditionalPanel(
        condition = "input.tabsetPanel === 'Maps'",
        ns = ns,
        selectInput(
          ns("map_region"),
          "Geography",
          choices = c(
            "Agricultural Region" = "region",
            "Local Authority" = "la"
          ),
          selected = "region"
        ),
        radioButtons(
          ns("map_variable"),
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

      # Constituency map
      # conditionalPanel(
      #   condition = "input.tabsetPanel === 'Constituency Map'",
      #   ns = ns,
      #   radioButtons(
      #     ns("variable_con"), 
      #     "Select Variable", 
      #     choices = c(
      #       "Goats and kids" = "Goats and kids",
      #       "Deer" = "Deer",
      #       "Horses" = "Horses",
      #       "Donkeys" = "Donkeys",
      #       "Camelids" = "Camelids",
      #       "Beehives" = "Beehives"
      #     )
      #   )
      # ),
      
      # Time series
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
      
      # Data table
      conditionalPanel(
        condition = "input.tabsetPanel === 'Data Table'",
        ns = ns,
        radioButtons(
          ns("table_data"),
          "Select Data to Display",
          choices = c("Agricultural Region Data" = "map", 
                      "Local Authority Data" = "map_uni",
                      "Time Series Data" = "timeseries"
                      # "Constituency Data" = "map_con",
                      ),
          selected = "map"
        )
      )
    ),
    mainPanel(
      width = 9,
      tabsetPanel(
        id = ns("tabsetPanel"),
        tabPanel(
          "Maps",
          conditionalPanel(
            condition = sprintf("input['%s'] == 'region'", ns("map_region")),
            mapUI(ns("map"))),
          conditionalPanel(
            condition = sprintf("input['%s'] == 'la'", ns("map_region")),
            mapUnitaryUI(ns("map_uni")))
        ),
        # tabPanel("Constituency Map", mapConstituenciesUI(ns("map_con"))),
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
    
    # Agricultural region map server
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
        req(input$map_variable)
        other_animals_data %>% filter(`Livestock by category` == input$map_variable)
      }),
      footer = census_footer,
      variable = reactive(input$map_variable),
      title = paste("Other animals distribution by region in Scotland in", census_year),
      legend_title = "Number of animals"
    )
    
    # Constituency map server
    # other_const_map <- reactive({
    #   other_animals_constituency %>%    
    #     mutate(across(everything(), as.character)) %>%
    #     pivot_longer(
    #       cols = -`livestock`,
    #       names_to = "constituency",
    #       values_to = "value"
    #     ) %>% 
    #     mutate(
    #       value = if_else(is.na(value), NA_integer_, as.integer(value))
    #     )
    # })
    # 
    # mapConstituenciesServer(
    #   id = "map_con",
    #   data = reactive({
    #     req(input$variable_con)
    #     other_const_map() %>% filter(`livestock` == input$variable_con)
    #   }),
    #   unit = "number",
    #   footer = census_footer,
    #   variable = reactive(input$variable_con),
    #   title = paste("Other animals distribution by 2026 Scottish Parliamentary Constituency"),
    #   legend_title = "Animals (number)"
    # )
    
    # Local authority map server
    other_uni_map <- reactive({
      other_animals_unitauth %>%
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
        req(input$map_variable)
        other_uni_map() %>% filter(`livestock` == input$map_variable)
      }),
      unit = "number",
      footer = census_footer,
      variable = reactive(input$map_variable),
      title = paste("Other animals distribution by local authority in", census_year),
      legend_title = "Animals (number)"
    )
    
    # Time series server
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
    
    # Data Table output
    output$table <- renderDT({
      req(input$tabsetPanel == "Data Table")
      
      # -------------------------
      # Select which dataset to show
      # -------------------------
      data <- switch(input$table_data,
                     
                     # -------------------
                     # 1. Agricultural Region data
                     # -------------------
                     "map" = {
                       other_animals_data %>%
                         pivot_wider(names_from = sub_region, values_from = value) %>%
                         mutate(across(where(is.numeric), comma))
                     },
                     
                     # -------------------
                     # 2. Timeseries data
                     # -------------------
                     "timeseries" = {
                       number_of_other_livestock %>%
                         pivot_longer(cols = -`Livestock by category`,
                                      names_to = "year",
                                      values_to = "value") %>%
                         pivot_wider(names_from = year, values_from = value) %>%
                         mutate(across(where(is.numeric) & !contains("Year"), comma))
                     },
                     
                     # -------------------
                     # 3. Constituency Table
                     # -------------------
                     # 
                     # "map_con" = {
                     #   other_animals_constituency %>%
                     #     rename(`Livestock by category` = `livestock`) %>%
                     #     mutate(across(where(is.numeric), comma))
                     # },
                     # 
                     
                     # -------------------
                     # 4. Local authority table
                     # -------------------
                     "map_uni" = {
                       other_animals_unitauth %>% 
                         rename(`Livestock by category` = `livestock`) %>%
                         mutate(across(where(is.numeric), comma))           
                     }
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
      
      # ---- Dynamic filename depending on selected table ----
      filename = function() {
        
        switch(input$table_data,
               "map" = paste0("Other_Animals_Map_Data_", Sys.Date(), ".csv"),
               "timeseries" = paste0("Other_Animals_Timeseries_Data_", Sys.Date(), ".csv"),
               # "map_con" = paste0("Other_Animals_Constituency_Data_", Sys.Date(), ".csv"),
               "map_uni" = paste0("Other_Animals_Local_Authority_Data_", Sys.Date(), ".csv"),
               
               # fallback
               paste0("Downloaded_Data_", Sys.Date(), ".csv")
        )
      },
      
      # ---- Write selected dataset to disk ----
      content = function(file) {
        
        data <- switch(input$table_data,
                       
                       # ---- Agricultural region map ----
                       "map" = {
                         other_animals_data %>%
                           filter(`Livestock by category` == input$map_variable) %>%
                           pivot_wider(names_from = sub_region, values_from = value)
                       },
                       
                       # ---- Timeseries ----
                       "timeseries" = {
                         number_of_other_livestock %>%
                           pivot_longer(
                             cols = -`Livestock by category`,
                             names_to = "year",
                             values_to = "value"
                           ) %>%
                           pivot_wider(names_from = year, values_from = value)
                       },
                       
                       # # ---- Constituency ----
                       # "map_con" = {
                       #   other_animals_constituency
                       # },
                        
                       # ---- Local authority ----
                       "map_uni" = {
                         other_animals_unitauth
                       }
        )
        
        write.csv(data, file, row.names = FALSE)
      }
    )
  }
  )
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
