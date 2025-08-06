# library(shiny)

### Delete once module is finished to prevent loading twice ###
#  load(here("Data","TIFF_data.Rda"))
###############################################################

# Define UI ----
tiffUI <- function(id) {
  ns <- NS(id)
  tagList(
    sidebarLayout(
      sidebarPanel(
        width = 3,
        
        radioButtons(
          ns("in_out_type"), "Measure",
          choices = c("Total" = "tiff_Total", "Outputs" = "tiff_Outputs", "Inputs" = "tiff_Inputs"),
          selected = "tiff_Total"
        ),
        
        conditionalPanel(
          condition = sprintf("input['%s'] == 'tiff_Total'", ns("in_out_type")),
          radioButtons(
            ns("support_payments"), "With Support Payments",
            choices = c("Yes", "No"),
            selected = "Yes"
          )
        ),
        
        conditionalPanel(
          condition = sprintf("input['%s'] != 'tiff_Total'", ns("in_out_type")),
          checkboxGroupInput(ns("selected_var"), "Select variables", choices = NULL)
        ),
        
        sliderInput(
          ns("selected_year"), "Select year range",
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
          tabPanel("Time series", lineChartUI(ns("line")), value = ns("line")),
          tabPanel(
            "Data Table",
            DTOutput(ns("data_table")),
            downloadButton(ns("downloadData"), "Download Data"),
            generateCensusTableFooter(),
            value = ns("data")
          )
        )
      )
    )
  )
}

# Define Server ----
tiffServer <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    # Chart data remains unformatted for proper rendering
    chart_data <- reactive({
      data <- main_tiff_data_long
      data  # return data explicitly
    })
    
    # Table data with formatted values for better readability
    table_data <- reactive({
      req(input$selected_year)
      req(length(input$selected_year) == 2)
      req(input$selected_var)
      
      year_start <- as.numeric(input$selected_year[1])
      year_end <- as.numeric(input$selected_year[2])

      chart_data() %>%
        mutate(Measure = str_replace_all(Measure, setNames(names(all_tiff), unname(all_tiff))))|>
        filter(Measure %in% input$selected_var) %>%
        filter(Year >= year_start& Year <= year_end) %>%
        mutate(Value = scales::comma(Value))
      
    })
    
    # change checkboxes dynamically depending on radio box selection
    observe({
      if (input$in_out_type == "tiff_Total") {
        selected <- if (is.null(input$support_payments) || input$support_payments == "Yes") {
          "Total income from farming"
        } else {
          "Total income from farming, without support payments"
        }
        
        updateCheckboxGroupInput(
          session,
          "selected_var",
          choices = tiff_Total,
          selected = selected
        )
      } else {
        choices <- switch(
          input$in_out_type,
          "tiff_Outputs" = names(tiff_Outputs),
          "tiff_Inputs" = names(tiff_Inputs),
          NULL
        )
        
        default_selection <- switch(
          input$in_out_type,
          "tiff_Outputs" = grep("Gross_output", choices, value = TRUE),
          "tiff_Inputs" = grep("Total_Costs", choices, value = TRUE),
          NULL
        )
        
        updateCheckboxGroupInput(
          session,
          "selected_var",
          choices = choices,
          selected = default_selection
        )
      }
    })
    
    ### chart logic ####   
    # Select the appropriate column based on data_type
    
    # y_col <- reactive({
    #   req(input$selected_var)
    #   input$selected_var  # returns a character vector of selected column names
    # })
    # 
    yAxisTitle <- "£ Million"
    
    lineChartServer(
      id = "line",
      chart_data = reactive({
        req(input$selected_var)
        year_start <- as.numeric(input$selected_year[1])
        year_end <- as.numeric(input$selected_year[2])
        
       data <-  chart_data() %>% 
         select(Measure, Year, Value) |> 
         # need to convert Measures column to values in all_Tiff list...this will match value of input$selected var
        mutate(Measure = str_replace_all(Measure, setNames(names(all_tiff), unname(all_tiff))))|>
        filter(Measure %in% input$selected_var)%>%
       filter(Year >= year_start & Year <= year_end)
      
        #no filtering on Category here
       data
      }),
      title = "TIFF Timeseries",
      yAxisTitle = "£ Million",
      xAxisTitle = "Year",
      footer = census_footer,
      x_col = "Year",
      y_col = "Value"
    )
    
    
    # Render the data table with formatted values
    # Render the data table based only on data_type selection with 20 entries by default
    output$data_table <- renderDT({
      datatable(
        table_data() %>%
          select(`Measure`, Year, Value),
        colnames = c("Measure", "Year", "Value"),
        options = list(pageLength = 20, scrollX = TRUE)  # Show 20 entries by default, enable horizontal scrolling
      )
    })
    
    
    # Download handler with formatted values
    output$downloadData <- downloadHandler(
      filename = function() {
        paste("tiff_data", Sys.Date(), ".csv", sep = "")
      },
      content = function(file) {
        write.csv(table_data(), file, row.names = FALSE)
      }
    )
  })
}



  
  
### Testing module --------
source(here("Economy/TIFF", "line_chart_copy.R"))
source("Economy/TIFF/tiff_utility.R")
source("utility/util_updates.R")
source("utility/util_functions.R")
source("utility/hc_theme.R")
source("utility/util_options.R")


content_demo <- function() {
  ui <- fluidPage(tiffUI("tifftest"))
  server <- function(input, output, session) {
    tiffServer("tifftest")
  }
  shinyApp(ui, server)
}

content_demo()

