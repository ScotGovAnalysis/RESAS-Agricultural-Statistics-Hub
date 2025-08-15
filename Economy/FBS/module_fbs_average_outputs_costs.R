# File: module_average_outputs_costs.R
# source("Economic/FBS/fbs_data_process.R")

###UI#####
CostOutUI <- function(id) {
  ns <- NS(id)
  tagList(
    sidebarLayout(
      sidebarPanel(
        width = 3,
        radioButtons(ns("in_out_type"), "Measure", choices = c("Outputs" = "output", "Costs" = "costs"), selected = "output"),
        radioButtons(ns("data_type"), "Choose summary totals or itemised", choices = c("Totals" = "totals", "Sub-category totals" = "sub-cat"#, "Itemised" = "itemised"
                                                                                                                    ), selected = "totals"),
        checkboxGroupInput(ns("selected_var"), "Select variables", choices = NULL),
        checkboxGroupInput(ns("selected_year"), "Select year", choices = c(current_year, prev_year), selected = c(current_year, pre_year))
      ),
      mainPanel(
        id = ns("mainpanel"),
        width = 9,
        tabsetPanel(
          id = ns("tabs"),
          tabPanel("All farms", barChartUI(ns("bar_chart1")), value = ns("bar")),
          tabPanel("Cereals", barChartUI(ns("bar_chart2")), value = ns("bar")),
          tabPanel("General cropping", barChartUI(ns("bar_chart3")), value = ns("bar")),
          tabPanel("Dairy", barChartUI(ns("bar_chart4")), value = ns("bar")),
          tabPanel("LFA sheep", barChartUI(ns("bar_chart5")), value = ns("bar")),
          tabPanel("LFA cattle", barChartUI(ns("bar_chart6")), value = ns("bar")),
          tabPanel("LFA cattle and sheep", barChartUI(ns("bar_chart7")), value = ns("bar")),
          tabPanel("Lowland cattle and sheep", barChartUI(ns("bar_chart8")), value = ns("bar")),
          tabPanel("Mixed", barChartUI(ns("bar_chart9")), value = ns("bar")),
          
          
          tabPanel("Data Table",
                   DTOutput(ns("data_table")),
                   downloadButton(ns("downloadData"), "Download Data"),
                   generateCensusTableFooter(),
                   value = ns("data"))
        )
      )
    )
  )
  
}
###server####
CostOutServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Chart data remains unformatted for proper rendering
    chart_data <- reactive({
      data <- fbs_long
    })
    
    # Table data with formatted values for better readability
    table_data <- reactive({
      chart_data() %>% filter(paste(input$in_out_type, input$data_type, sep = "_")  == tot_item) %>%  
        filter(year %in% input$selected_year) %>%  # filter for selected year
        filter(Measure %in% input$selected_var) %>%  # fitler for selected var
        mutate(across(where(is.numeric), comma))  # Format numeric columns with commas
    })
    
# change checkboxes dynamically depending on radio box selection
    observeEvent({
      input$in_out_type
      input$data_type
    }, {
      choices <- switch(paste(input$in_out_type, input$data_type, sep = "_"),
                        "output_totals" = out_totals,
                        "output_sub-cat" = out_sub_totals,
                        # "output_itemised" = out_items,
                        "costs_totals" = cost_totals,
                         "costs_sub-cat" = cost_sub_totals,
                       # "costs_itemised" = cost_sub_items,
                        NULL)
      
      updateCheckboxGroupInput(session, "selected_var",
                               choices = choices,
                               selected = out_totals[1])
    })
    
  ### chart logic ####   
 # Select the appropriate column based on data_type

    y_col <- reactive({
      req(input$selected_var)
      input$selected_var  # returns a character vector of selected column names
    })
  
    
    tooltip_unit <- reactive({
    "(£ per farm, real prices)"
    })

   
    
    multibarChartServer(
      id = "bar_chart1",
      chart_data = reactive({
        data <- chart_data() %>% 
          filter(farm_type == farm_types[1]) %>% # select farm type
          filter(paste(input$in_out_type, input$data_type, sep = "_")  == tot_item) %>%  
         filter(year %in% input$selected_year) %>%  # filter for selected year
          filter(Measure %in% input$selected_var) # filter for selected var
        data
      }),
      title = paste0(farm_types[1], ": ",# input$in_out_type, ", 
                      prev_year, " and ", current_year),
      yAxisTitle =NULL,
      xAxisTitle = "" ,
     
      footer = fbs_footer,
      x_col = "year",
      y_col = y_col,
      tooltip_unit = tooltip_unit,
      maintain_order = TRUE
    )
    
    multibarChartServer(
      id = "bar_chart2",
      chart_data = reactive({
        data <- chart_data() %>% filter(input_output_type == input$in_out_type) %>% # select output lines
          filter(farm_type == farm_types[2]) %>% # select farm type
          filter(paste(input$in_out_type, input$data_type, sep = "_")  == tot_item) %>%  
          filter(year %in% input$selected_year) %>%  # filter for selected year
          filter(Measure %in% input$selected_var) # fitler for selected var
        data
      }),
      title = paste0(farm_types[2], ": ",# input$in_out_type, ", "
                      prev_year, " and ", current_year),
      yAxisTitle ="",
      xAxisTitle = "",
      
      footer = fbs_footer,
      x_col = "year",
      y_col = y_col,
      tooltip_unit = tooltip_unit,
      maintain_order = TRUE
    )
    
    multibarChartServer(
      id = "bar_chart3",
      chart_data = reactive({
        data <- chart_data() %>% filter(input_output_type == input$in_out_type) %>% # select output lines
          filter(farm_type == farm_types[3]) %>% # select farm type
          filter(paste(input$in_out_type, input$data_type, sep = "_")  == tot_item) %>%  
          filter(year %in% input$selected_year) %>%  # filter for selected year
          filter(Measure %in% input$selected_var) # fitler for selected var
        data
      }),
      title = paste0(farm_types[3], ": ",# input$in_out_type, ", ",
                     prev_year, " and ", current_year),
      yAxisTitle ="",
      xAxisTitle = "" ,
      
      footer = fbs_footer,
      x_col = "year",
      y_col = y_col,
      tooltip_unit = tooltip_unit,
      maintain_order = TRUE
    )
    
    multibarChartServer(
      id = "bar_chart4",
      chart_data = reactive({
        data <- chart_data() %>% filter(input_output_type == input$in_out_type) %>% # select output lines
          filter(farm_type == farm_types[4]) %>% # select farm type
          filter(paste(input$in_out_type, input$data_type, sep = "_")  == tot_item) %>%  
          filter(year %in% input$selected_year) %>%  # filter for selected year
          filter(Measure %in% input$selected_var) # filter for selected var
        data
      }),
      title = paste0(farm_types[4], ": ",# input$in_out_type, ", ", 
                     prev_year, " and ", current_year),
      yAxisTitle =NULL,
      xAxisTitle = "",
      
      footer = fbs_footer,
      x_col = "year",
      y_col = y_col,
      tooltip_unit = tooltip_unit,
      maintain_order = TRUE
    )
    
    multibarChartServer(
      id = "bar_chart5",
      chart_data = reactive({
        data <- chart_data() %>% filter(input_output_type == input$in_out_type) %>% # select output lines
          filter(farm_type == farm_types[5]) %>% # select farm type
          filter(paste(input$in_out_type, input$data_type, sep = "_")  == tot_item) %>%  
          filter(year %in% input$selected_year) %>%  # filter for selected year
          filter(Measure %in% input$selected_var) # fitler for selected var
        data
      }),
      title = paste0(farm_types[5], ": ",#input$in_out_type, ", ", 
                     prev_year, " and ", current_year),
      yAxisTitle =NULL,
      xAxisTitle = "",
      
      footer = fbs_footer,
      x_col = "year",
      y_col = y_col,
      tooltip_unit = tooltip_unit,
      maintain_order = TRUE
    )
     multibarChartServer(
      id = "bar_chart6",
      chart_data = reactive({
        data <- chart_data() %>% filter(input_output_type == input$in_out_type) %>% # select output lines
          filter(farm_type == farm_types[6]) %>% # select farm type
          filter(paste(input$in_out_type, input$data_type, sep = "_")  == tot_item) %>%  
          filter(year %in% input$selected_year) %>%  # filter for selected year
          filter(Measure %in% input$selected_var) # fitler for selected var
        data
      }),
      title = paste0(farm_types[6], ": ", #input$in_out_type, ", ",
                     prev_year, " and ", current_year),
      yAxisTitle =NULL,
      xAxisTitle ="" ,
      
      footer = fbs_footer,
      x_col = "year",
      y_col = y_col,
      tooltip_unit = tooltip_unit,
      maintain_order = TRUE
     )
    
     multibarChartServer(
       id = "bar_chart7",
       chart_data = reactive({
         data <- chart_data() %>% filter(input_output_type == input$in_out_type) %>% # select output lines
           filter(farm_type == farm_types[7]) %>% # select farm type
           filter(paste(input$in_out_type, input$data_type, sep = "_")  == tot_item) %>%  
           filter(year %in% input$selected_year) %>%  # filter for selected year
           filter(Measure %in% input$selected_var) # fitler for selected var
         data
       }),
       title = paste0(farm_types[7], ": ", #input$in_out_type, ", ", 
                      prev_year, " and ", current_year),
       yAxisTitle =NULL,
       xAxisTitle = "",
       
       footer = fbs_footer,
       x_col = "year",
       y_col = y_col,
       tooltip_unit = tooltip_unit,
       maintain_order = TRUE
     )
     
     multibarChartServer(
       id = "bar_chart8",
       chart_data = reactive({
         data <- chart_data() %>% filter(input_output_type == input$in_out_type) %>% # select output lines
           filter(farm_type == farm_types[8]) %>% # select farm type
           filter(paste(input$in_out_type, input$data_type, sep = "_")  == tot_item) %>%  
           filter(year %in% input$selected_year) %>%  # filter for selected year
           filter(Measure %in% input$selected_var) # fitler for selected var
         data
       }),
       title = paste0(farm_types[8], ": ",# input$in_out_type, ", ",
                      prev_year, " and ", current_year),
       yAxisTitle =NULL,
       xAxisTitle = "",
       
       footer = fbs_footer,
       x_col = "year",
       y_col = y_col,
       tooltip_unit = tooltip_unit,
       maintain_order = TRUE
     )
     multibarChartServer(
       id = "bar_chart9",
       chart_data = reactive({
         data <- chart_data() %>% filter(input_output_type == input$in_out_type) %>% # select output lines
           filter(farm_type == farm_types[9]) %>% # select farm type
           filter(paste(input$in_out_type, input$data_type, sep = "_")  == tot_item) %>%  
           filter(year %in% input$selected_year) %>%  # filter for selected year
           filter(Measure %in% input$selected_var) # fitler for selected var
         data
       }),
       title = paste0(farm_types[9], ": ", #input$in_out_type, ", ", 
                      prev_year, " and ", current_year),
       yAxisTitle =NULL,
       xAxisTitle ="" ,
       
       footer = fbs_footer,
       x_col = "year",
       y_col = y_col,
       tooltip_unit = tooltip_unit,
       maintain_order = TRUE
     )
     
     
     
    # Render the data table with formatted values
     # Render the data table based only on data_type selection with 20 entries by default
     output$data_table <- renderDT({
       datatable(
    table_data() %>%
           select(`Measure`, farm_type, main_category, year, value),
         colnames = c("Measure", "Main Farm Type", "Output or Cost category", "Year", "Value"),
         options = list(pageLength = 20, scrollX = TRUE)  # Show 20 entries by default, enable horizontal scrolling
       )
     })
     
     
    # Download handler with formatted values
    output$downloadData <- downloadHandler(
      filename = function() {
        paste("Farm_level_output_costs_", Sys.Date(), ".csv", sep = "")
      },
      content = function(file) {
        write.csv(table_data(), file, row.names = FALSE)
      }
    )
  })
}

### Testing module --------
source("testing/util_test_barchart_function.R")
source("utility/util_updates.R")
source("utility/util_functions.R")
source("utility/hc_theme.R")
source("utility/util_options.R")



content_demo <- function() {
  ui <- fluidPage(CostOutUI("test"))
  server <- function(input, output, session) {
    CostOutServer("test")
  }
  shinyApp(ui, server)
}

content_demo()

