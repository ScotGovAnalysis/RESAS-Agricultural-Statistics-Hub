### Delete once module is finished to prevent loading twice ###
#load(here("Data","TIFF_data.Rda"))
###############################################################

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
          ns("in_out_type"), "Measure",
          choices = c("Total" = "tiff_Total", "Outputs" = "tiff_Outputs", "Inputs" = "tiff_Inputs"),
          selected = "tiff_Total"
        ),
        radioButtons(
          ns("tiff_prices"), "Price",
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
          tabPanel("Time series", lineChartUI(ns("line")), value = ns("line"), note_type = 1),
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
      req(input$tiff_prices)
      data <- data %>% filter(Price == input$tiff_prices)
      
      if (input$in_out_type == "tiff_Total") {
        # create a vector of selected Measure names depending on Yes/No
        selected_measures <- c()
        if ("Yes" %in% input$support_payments) {
          selected_measures <- c(selected_measures, "Total income from farming")
        }
        if ("No" %in% input$support_payments) {
          selected_measures <- c(selected_measures, "Total income from farming, without support payments")
        }
        data <- data %>% filter(Measure %in% selected_measures)
      } else {
        data <- data %>% filter(Measure %in% input$selected_var)
      }
      
      data
    })
    
    # Table data with formatted values for better readability
    table_data <- main_tiff_data_long %>%
      rename(`Value (GBP 000)` = Value)
    
    # change checkboxes dynamically depending on radio box selection
    observe({
      if (input$tabs == ns("data")) {  # your Data Table tab value
        shinyjs::hide("sidebar")
      } else {
        shinyjs::show("sidebar")
      }
      if (input$in_out_type == "tiff_Total") {
        selected <- c()
        if (!is.null(input$support_payments)) {
          if ("Yes" %in% input$support_payments) selected <- c(selected, "Total income from farming")
          if ("No"  %in% input$support_payments) selected <- c(selected, "Total income from farming, without support payments")
        } else {
          # default selection if nothing is selected
          selected <- "Total income from farming"
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
          "tiff_Outputs" = setNames(tiff_Outputs, gsub("_", " ", tiff_Outputs)),
          "tiff_Inputs"  = setNames(tiff_Inputs, gsub("_", " ", tiff_Inputs)),
          NULL
        )
        
        default_selection <- switch(
          input$in_out_type,
          "tiff_Outputs" = "Gross_output",  # internal value
          "tiff_Inputs"  = "Total_Costs",
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
    
    lineChartServer(
      id = "line",
      chart_data = reactive({
        year_start <- as.numeric(input$selected_year[1])
        year_end <- as.numeric(input$selected_year[2])
        
        data <- chart_data() %>%
          filter(Measure %in% input$selected_var) %>%
          filter(Year >= year_start & Year <= year_end) %>%
          mutate(Measure = gsub("_", " ", Measure))  # overwrite Measure for display
        
        data
      }),
      chart_title = titleText,
      yAxisTitle = "£ 000",
      xAxisTitle = "Year",
      footer = census_footer,
      x_col = "Year",
      y_col = "Value"
    )
    
    
    # Render the data table with formatted values
    # Render the data table based only on data_type selection with 20 entries by default
    output$data_table <- renderDT({
      datatable(
        table_data %>%
          select(Measure, Price, Year, `Value (GBP 000)`) %>% 
        arrange(Price, desc(Year)) %>%
          mutate(`Value (GBP 000)` = comma(round(`Value (GBP 000)`, 0))),  # 0 dp
        colnames = c("Measure","Price", "Year", "Value (£ 000)"),
        options = list(pageLength = 25, # Show 25 entries by default
                       scrollX = TRUE, #enable horizontal scrolling
                       order = list(3, 'desc') # Price asc, Year desc
                       ), 
      )
    })
    
    
    # Download handler with formatted values
    output$downloadData <- downloadHandler(
      filename = function() {
        paste("tiff_data", Sys.Date(), ".csv", sep = "")
      },
      content = function(file) {
    write.csv(table_data, file, row.names = FALSE)
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

