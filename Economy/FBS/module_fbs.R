# File: module_FBS.R
#source(here("Economy/FBS/fbs_data_process.R"))

###UI#####
CostOutUI <- function(id) {
  ns <- NS(id)
  tagList(
    sidebarLayout(
      sidebarPanel(
        width = 3,
        selectInput(ns("in_out_type"), "Measure", choices = c(
          "Farm Business Income (FBI)" = "fbi",        
          "Support payments" = "supp",
          "Diversified income" = "div_inc",
          "Net farm income" = "nfi",
          "Off farm income" = "ofi",
          "Detailed outputs" = "output", 
          "Detailed costs" = "costs"
          #"FBI without support payments" = "fbi_nosupp",
        ), selected = "fbi"),
        
        # only show if detailed outputs or detailed costs are selected
        conditionalPanel(
          condition = sprintf("input['%s'] == 'output' || input['%s'] == 'costs'", ns("in_out_type"), ns("in_out_type")),
          radioButtons(ns("data_type"), "Choose summary totals or itemised", choices = c(
            "Totals" = "totals", 
            "Sub-category totals" = "sub-cat"
            # "Itemised" = "itemised"
          ), selected = "totals")
        ),
        conditionalPanel(
          condition = sprintf("input['%s'] == 'output' || input['%s'] == 'costs'", ns("in_out_type"), ns("in_out_type")),
          checkboxGroupInput(ns("selected_var"), "Select variables", choices = NULL)
        ),
        
        # For output/costs
        conditionalPanel(
          condition = sprintf("input['%s'] == 'output' || input['%s'] == 'costs'", ns("in_out_type"), ns("in_out_type")),
          checkboxGroupInput(ns("selected_years_checkbox"), "Select year", choices = c(current_year, prev_year), selected = c(current_year, prev_year))
        ),
        
        # For all other types
        conditionalPanel(
          condition = sprintf("input['%s'] != 'output' && input['%s'] != 'costs'", ns("in_out_type"), ns("in_out_type")),
          sliderTextInput(
            inputId = ns("selected_years_slider"),
            label = "Select year range",
            choices = year_levels,
            selected = c("2012-13", current_year),
            grid = TRUE
          )
        )
        
        
      ),
      mainPanel(
        id = ns("mainpanel"),
        width = 9,
        tabsetPanel(
          id = ns("tabs"),
          tabPanel("All farms", uiOutput(ns("chart1")), value = ns("all_farms")),
          tabPanel("Cereals", uiOutput(ns("chart2")), value = ns("cereals")),
          tabPanel("General cropping", uiOutput(ns("chart3")), value = ns("general_cropping")),
          tabPanel("Dairy", uiOutput(ns("chart4")), value = ns("dairy")),
          tabPanel("LFA sheep", uiOutput(ns("chart5")), value = ns("lfa_sheep")),
          tabPanel("LFA cattle", uiOutput(ns("chart6")), value = ns("lfa_cattle")),
          tabPanel("LFA cattle and sheep", uiOutput(ns("chart7")), value = ns("lfa_cattle_sheep")),
          tabPanel("Lowland cattle and sheep", uiOutput(ns("chart8")), value = ns("lowland_cattle_sheep")),
          tabPanel("Mixed", uiOutput(ns("chart9")), value = ns("mixed")),
          tabPanel("Data Table",
                   DTOutput(ns("data_table")),
                   downloadButton(ns("downloadData"), "Download Data"),
                   generateFBSTableFooter(),
                   value = ns("data"))
        )
      )
    )
  )
}

# ###server####
CostOutServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Reactive expression that returns the chart data based on in_out_type
    chart_data <- reactive({
      req(input$in_out_type)
      if (input$in_out_type %in% c("output", "costs")) {
        fbs_cost_centre 
      } else {
        fbs_income
      }
    })
    
    # Reactive expression to unify selected years from checkbox or slider inputs
    selected_years <- reactive({
      req(input$in_out_type)
      
      if (input$in_out_type %in% c("output", "costs")) {
        req(input$selected_years_checkbox)
        input$selected_years_checkbox
      } else {
        req(input$selected_years_slider)
        start <- match(input$selected_years_slider[1], year_levels)
        end <- match(input$selected_years_slider[2], year_levels)
        if (is.na(start) || is.na(end)) return(character(0))
        if (start > end) {
          tmp <- start
          start <- end
          end <- tmp
        }
        year_levels[start:end]
      }
    })
    
    # Reactive expression for filtered table data with formatted numeric columns
    table_data <- reactive({
      req(input$in_out_type, selected_years())
       
      data <- chart_data() %>%
        filter(input_output_type == input$in_out_type) %>%
        #filter(farm_type == selected_tab) %>%  # Adjust farm_type as needed
        filter(year %in% selected_years())
      
      if (input$in_out_type %in% c("output", "costs")) {
        req(input$data_type, input$selected_var)
        data <- data %>%
          filter(paste(input$in_out_type, input$data_type, sep = "_") == tot_item) %>%
          filter(Measure %in% input$selected_var)
      }
      
      data %>%
        mutate(across(where(is.numeric), scales::comma))
    })
    
    # Dynamically update checkboxGroupInput choices based on in_out_type and data_type
    observeEvent(c(input$in_out_type, input$data_type), {
      choices <- switch(
        paste(input$in_out_type, input$data_type, sep = "_"),
        "output_totals" = out_totals,
        "output_sub-cat" = out_sub_totals,
        # "output_itemised" = out_items,
        "costs_totals" = cost_totals,
        "costs_sub-cat" = cost_sub_totals,
        # "costs_itemised" = cost_sub_items,
        NULL
      )
      
      if (!is.null(choices)) {
        updateCheckboxGroupInput(session, "selected_var",
                                 choices = choices,
                                 selected = choices[1])
      }
    })
    
    # Reactive y_col (returns selected variable for y-axis)
    y_col <- reactive({
      req(input$selected_var)
      input$selected_var
    })
    
    # Reactive tooltip unit text
    tooltip_unit <- reactive({
      ""
    })
    
    # Function to get filtered chart data reactive for a given farm_type index
    get_filtered_chart_data <- function(chart_data, farm_type_index) {
      reactive({
        req(input$in_out_type, selected_years())
        
        data <- chart_data() %>%
          filter(
            input_output_type == input$in_out_type,
            farm_type == farm_types[farm_type_index],
            year %in% selected_years()
          )
        
        if (input$in_out_type %in% c("output", "costs")) {
          req(input$data_type, input$selected_var)
          data <- data %>%
            filter(tot_item == paste(input$in_out_type, input$data_type, sep = "_")) %>%
            filter(Measure %in% input$selected_var)
        }
        
        data
      })
    }
    
   
      
    # Render UI for each chart based on filtered data availability
    renderChartUI <- function(chart_id, farm_type_index) {
      filtered_chart_data <- get_filtered_chart_data(chart_data, farm_type_index)
      
      output[[paste0("chart", farm_type_index)]] <- renderUI({
        data <- filtered_chart_data()
        if (nrow(data) == 0) {
          div(style = "color: red; font-weight: bold;", "No data available for the selected filters.")
        } else {
          if (input$in_out_type %in% c("output", "costs")) {
            multibarChartUI(ns(paste0("bar_chart", farm_type_index)))
          } else {
            fbsline_ChartUI(ns(paste0("fbs_line_chart", farm_type_index)), in_out_type = input$in_out_type)
          }
        }
      })
      
     
    }
    
    # Server logic to render charts and respond to input changes
    renderChartServer <- function(bar_id, line_id, farm_type_index) {
      filtered_chart_data <- get_filtered_chart_data(chart_data, farm_type_index)
      
      observeEvent(
        {
          list(
            input$in_out_type,
            selected_years(),
            input$data_type,
            input$selected_var
          )
        },
        {
          req(input$in_out_type)
          
          years_sorted <- sort(selected_years())
          
          chart_title <- paste0(
            farm_types[farm_type_index], ": ",
            if(input$in_out_type %in% c("output", "costs")){input$in_out_type} else # Outputs or Costs
              {gsub("Average", "", (head(filtered_chart_data()$Measure[filtered_chart_data()$input_output_type == input$in_out_type], n =1 )))}, # Long name for measure, select last row for only one title to appear!
            " ", 
            if (length(years_sorted) == 1) {
              years_sorted[1]
            } else {
              paste(years_sorted[1], years_sorted[length(years_sorted)], sep = " to ")
            },
            ", real (constant ", substr(current_year, 1, 4), ") prices"
          )
          
          if (input$in_out_type %in% c("output", "costs")) {
            multibarChartServer(
              id = bar_id,
              chart_data = filtered_chart_data,
              title =chart_title,
              yAxisTitle = "£ Thousand",
              xAxisTitle = "",
              footer = fbs_footer,
              x_col = "year",
              y_col = y_col, 
              tooltip_unit = tooltip_unit,
              maintain_order = TRUE
            )
          } else {
            fbsline_ChartServer(
              id = line_id,
              chart_data = filtered_chart_data,
              title =  chart_title, 
              yAxisTitle = "£ Thousand",
              xAxisTitle = "",
              footer = fbs_footer,
              x_col = "year",
              y_col= "value",
              unit = tooltip_unit
            )
          }
        },
        ignoreNULL = TRUE,
        ignoreInit = FALSE
        
      )
    }
    
    # Loop to initialize UI and server for each farm type
    for (i in seq_along(farm_types)) {
      local({
        farm_type_index <- i
        
        renderChartUI(paste0("bar_chart", farm_type_index), farm_type_index)
        renderChartServer(
          paste0("bar_chart", farm_type_index),
          paste0("fbs_line_chart", farm_type_index),
          farm_type_index
        )
      })
    }
    
    # Render data table with conditional columns based on in_out_type
    output$data_table <- renderDT({
      
   
     
       data <- table_data()
      

    
    
      if (input$in_out_type %in% c("output", "costs")) {
        data <- data %>%
          select(Measure, farm_type, input_output_type, year, value) |> 
          mutate(
            input_output_type = sub("^([a-z])", "\\U\\1", tolower(input_output_type), perl = TRUE)
          )
        col_names <- c("Measure", "Main Farm Type", "Output or Cost Category", "Year", "Value")
      } else {
        data <- data %>%
          select(Measure, farm_type, year, value)
        col_names <- c("Measure", "Main Farm Type", "Year", "Value")
      }
      
      datatable(
        data,
        colnames = col_names,
        options = list(pageLength = 20, scrollX = TRUE)
      )
    })
    
    # Download handler for CSV export of table data
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

# check for empty data:

# 
## Testing module --------
# source(here("testing", "test_multibarchart_function.R"))
# source(here("utility", "util_updates.R"))
# source(here("utility", "util_functions.R"))
# source(here("utility", "hc_theme.R"))
# source(here("utility", "util_options.R"))
# #source(here("testing", "test_fbs_function_line_chart.R"))
# 
# 
# 
# content_demo <- function() {
#   ui <- fluidPage(CostOutUI("test"))
#   server <- function(input, output, session) {
#     CostOutServer("test")
#   }
#   shinyApp(ui, server)
# }

content_demo()

