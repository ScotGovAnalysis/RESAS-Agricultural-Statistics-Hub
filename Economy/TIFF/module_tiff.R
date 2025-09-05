
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
        selectInput(
          ns("in_out_type"), "Select a measure",
          choices = c("Total income from farming" = "tiff_Total",
                      "Total income from farming, without support payments" = "tiff_Total_wsp", 
                      "Outputs" = "tiff_Outputs", 
                      "Costs" = "tiff_Costs", 
                      "GVA" = "tiff_GVA", 
                      "Net value added" = "tiff_NVA", 
                      "Support payments"= "tiff_Support_payments"),
          selected = "tiff_Total"
        ),
        # only show if  outputs or costs are selected
        conditionalPanel(
          condition = sprintf("input['%s'] == 'tiff_Outputs' || input['%s'] == 'tiff_Costs'", ns("in_out_type"), ns("in_out_type")),
          checkboxGroupInput(ns("selected_var"), "Select variables", choices = NULL)
        ),
        radioButtons(
          ns("tiff_prices"), "Select a price type",
          choices = c("Real (constant 2024)", "Current (nominal)"),
          selected = "Real (constant 2024)"
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
          tabPanel("Time series", lineChartUI(ns("line"), note_type = 3), value = ns("line")),
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
    
    # Chart data #
    # Reactive expression that returns the chart data based on in_out_type
    chart_data <- reactive({
      req(input$in_out_type)
      
    # Map in_out_type to corresponding measures
    measures <- switch(
      input$in_out_type,
      "tiff_Total"         = tiff_Total,
      "tiff_Total_wsp"     = tiff_Total_wsp,
      "tiff_Outputs"       = tiff_Outputs,
      "tiff_Costs"         = tiff_Costs,
      "tiff_GVA"           = tiff_GVA,
      "tiff_NVA"           = tiff_NVA,
      "tiff_Support_payments" = tiff_Support_payments,
      NULL
    )
    
    main_tiff_data_long %>%
      filter(Measure %in% measures,
             Price == input$tiff_prices
             )
  })
    
    
    # ---- Table data (formatted) ----
    table_data <- main_tiff_data_long %>%
      select(Measure, Price, Year, Value) %>% 
      rename(`Value (GBP 000)` = Value) %>%
      mutate(
        `Value (GBP 000)` = comma(round(`Value (GBP 000)`, 0))
      )
    
    # Hide side bar for data table tab
    observe({
      if (input$tabs == ns("data")) {
        shinyjs::hide(id = "sidebar", anim = TRUE, animType = "slide")
      } else {
        shinyjs::show(id = "sidebar", anim = TRUE, animType = "slide")
      }
      
      # Only update choices if Outputs or Costs are selected
      if (input$in_out_type %in% c("tiff_Outputs", "tiff_Costs")) {
        choices <- switch(
          input$in_out_type,
          "tiff_Outputs" = tiff_Outputs,
          "tiff_Costs"   = tiff_Costs
        )
        
        default_selection <- switch(
          input$in_out_type,
          "tiff_Outputs" = "5. Gross output (1+2+3+4)",     
          "tiff_Costs"   = "22. Total costs (13+15+20+21)",      
          NULL
        )
        
        updateCheckboxGroupInput(
          session,
          "selected_var",
          choices = choices,
          selected = default_selection
        )
        
      } else {
        # Clear checkbox selections when not Outputs or Costs
        updateCheckboxGroupInput(
          session,
          "selected_var",
          choices = character(0),
          selected = character(0)
        )
      }
    })
    
    
    # ---- Line chart ----
    lineChartServer(
      id = "line",
      chart_data = reactive({
        data <- chart_data()
        
        # Only apply sub-variable filtering if Outputs or Costs selected
        if (input$in_out_type %in% c("tiff_Outputs", "tiff_Costs")) {
          req(input$selected_var)
          data <- data %>% filter(Measure %in% input$selected_var)
        }
        
        data <- data %>%
          filter(Year >= input$selected_year[1],
                 Year <= input$selected_year[2])
        
        req(nrow(data) > 0)
        data
      }),
      title = "Total income from farming timeseries",
      yAxisTitle = "Value (£Thousand)",
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
        colnames = c("Measure","Price", "Year", "Value (£Thousand)"),
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
