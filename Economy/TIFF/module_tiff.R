
tiff_year <- max(main_tiff_data_long$Year) #Current TIFF year
tiff_year_min <- min(main_tiff_data_long$Year)

# Define UI ----
tiffUI <- function(id) {
  ns <- NS(id)
  tagList(
    useShinyjs(),
    sidebarLayout(
      sidebarPanel(
        id = ns("sidebar"),
        width = 3,
        radioButtons(
          ns("in_out_type"), "Select a measure",
          choices = c("Total" = "tiff_Total", "Outputs" = "tiff_Outputs", "Costs" = "tiff_Costs"),
          selected = "tiff_Total"
        ),
        radioButtons(
          ns("tiff_prices"), "Select a price type",
          choices = c("Current (nominal)", "Real terms (Constant 2024)"),
          selected = "Current (nominal)"
        ),
        conditionalPanel(
          condition = sprintf("input['%s'] == 'tiff_Total'", ns("in_out_type")),
          checkboxGroupInput(
            ns("support_payments"), 
            "Include support payments", 
            choices = c("Yes", "No"), 
            selected = c("Yes")
          )
        ),
        conditionalPanel(
          condition = sprintf("input['%s'] != 'tiff_Total'", ns("in_out_type")),
          checkboxGroupInput(ns("selected_var"), "Select variables", choices = NULL)
        ),
        sliderInput(
          ns("selected_year"), "Select the year range by dragging the ends of the slider",
          min = tiff_year_min,
          max = tiff_year,
          value = c(tiff_year - 10, tiff_year),
          step = 1,
          sep = ""
        )
      ),
      mainPanel(
        id = ns("mainpanel"),
        width = 9,
        tabsetPanel(
          id = ns("tabs"),
          tabPanel("Time series", lineChartUI(ns("line")), value = ns("line"), note_type = 1),
          tabPanel(
            "Data Table",
            DTOutput(ns("data_table")),
            downloadButton(ns("downloadData"), "Download Data"),
            generatetiffTableFooter(),
            value = ns("data")
          )
        )
      )
    )
  )
}

# Define Server ----
tiffServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # ---- Chart data (unformatted) ----
    chart_data <- reactive({
      req(input$tiff_prices)
      data <- main_tiff_data_long %>% 
        filter(Price == input$tiff_prices)
      
      if (input$in_out_type == "tiff_Total") {
        selected_measures <- c()
        if ("Yes" %in% input$support_payments) {
          selected_measures <- c(selected_measures, "23. Total income from farming (19-20-21)")
        }
        if ("No" %in% input$support_payments) {
          selected_measures <- c(selected_measures, "24. Total income from farming, without support payments (23-17)")
        }
        data <- data %>% filter(Measure %in% selected_measures)
      } else {
        if (!is.null(input$selected_var)) {
          data <- data %>%
            filter(Measure %in% input$selected_var)
        }
      }
      
      req(nrow(data) > 0)  # ensure non-empty
      data
    })
    
    # ---- Table data (formatted) ----
    table_data <- main_tiff_data_long %>%
      select(Measure, Price, Year, Value) %>% 
      rename(`Value (GBP 000)` = Value) %>%
      mutate(
        `Value (GBP 000)` = comma(round(`Value (GBP 000)`, 0))
      )
    
    # ---- Update checkboxes dynamically ----
    observe({
      if (input$tabs == ns("data")) {
        shinyjs::hide(id = "sidebar", anim = TRUE, animType = "slide")
      } else {
        shinyjs::show(id = "sidebar", anim = TRUE, animType = "slide")
      }
      if (input$in_out_type == "tiff_Total") {
        selected <- if (!is.null(input$support_payments)) {
          c(
            if ("Yes" %in% input$support_payments) "23. Total income from farming (19-20-21)" else NULL,
            if ("No"  %in% input$support_payments) "24. Total income from farming, without support payments (23-17)" else NULL
          )
        } else {
          "23. Total income from farming (19-20-21)"
        }
        updateCheckboxGroupInput(session, "selected_var", choices = tiff_Total, selected = selected)
      } else {
        choices <- switch(
          input$in_out_type,
          "tiff_Outputs" = tiff_Outputs,   
          "tiff_Costs"   = tiff_Costs,
          NULL
        )
        default_selection <- switch(
          input$in_out_type,
          "tiff_Outputs" = "5. Gross output (1+2+3+4)",     
          "tiff_Costs"   = "22. Total costs (13+15+20+21)",      
          NULL
        )
        updateCheckboxGroupInput(session, "selected_var", 
                                 choices = choices, 
                                 selected = default_selection)
      }
    })
    
    # ---- Line chart ----
    lineChartServer(
      id = "line",
      chart_data = reactive({
        req(input$selected_var)
        data <- chart_data()
        
        if (input$in_out_type != "tiff_Total") {
          req(input$selected_var)
          data <- data %>% 
            filter(Measure %in% input$selected_var)
        }
        
        data <- data %>%
          filter(Year >= input$selected_year[1],
                 Year <= input$selected_year[2])
        
        req(nrow(data) > 0)
        data
      }),
      title = "Total income from farming timeseries",
      yAxisTitle = "Value (£000)",
      xAxisTitle = "Year",
      footer = tiff_footer,
      x_col = "Year",
      y_col = "Value"
    )
    
    # ---- Data Table ----
    
    output$data_table <- renderDT({
      datatable(
        table_data %>% 
          arrange(Price, desc(Year)),
        colnames = c("Measure","Price", "Year", "Value (£000)"),
        options = list(pageLength = 20, scrollX = TRUE, order = list(list(3, 'desc')))
      )
    })
    
    output$downloadData <- downloadHandler(
      filename = function() paste0("Total_income_from_farming_data_", tiff_year, ".csv"),
      content = function(file) write.csv(table_data, file, row.names = FALSE)
    )
  })
}


  
  
# # ### Testing module --------
# source(here("Economy/TIFF", "line_chart_copy.R"))
# source("Economy/TIFF/tiff_utility.R")
# source("utility/util_updates.R")
# source("utility/util_functions.R")
# source("utility/hc_theme.R")
# source("utility/util_options.R")
# 
# 
# content_demo <- function() {
#   ui <- fluidPage(tiffUI("tifftest"))
#   server <- function(input, output, session) {
#     tiffServer("tifftest")
#   }
#   shinyApp(ui, server)
# }
# 
# content_demo()
# 
