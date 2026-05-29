load("Data/n_balance.Rda")
load("Data/nue.Rda")


n_balance<-table_5_df %>% 
  mutate(n_type="n_balance") %>% 
  pivot_longer(
    cols = `2019-20`:fbs_current_year,
    names_to = "year",
    values_to="value")


nue<-table_6_df %>% 
  mutate(n_type="nue") %>% 
  pivot_longer(
    cols = `2019-20`:fbs_current_year,
    names_to = "year",
    values_to="value")

nitrogen_data<-dplyr::bind_rows(n_balance, nue) %>% 
  rename(farm_type=`Farm type`) %>% 
  filter(Measure=="Average (median)")



nitrogenUI<- function(id) {
  ns <- NS(id)
  tagList(
    sidebarLayout(
      sidebarPanel(
        width = 3,
        selectInput(ns("n_type"), "Measure", choices = c(
          "Nitrogen balance" = "n_balance",        
          "Nitrogen use efficiency" = "nue"
        ), selected = "n_balance"),
        
        ),

      mainPanel(
        id = ns("mainpanel"),
        width = 9,
        tabsetPanel(
          id = ns("tabs"),
          tabPanel("All farms", uiOutput(ns("chart1")), value = ns("all_farms")),
          tabPanel("Cereal", uiOutput(ns("chart2")), value = ns("cereal")),
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




# # ###server####
nitrogenServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Reactive expression that returns the chart data based on n_type
    chart_data <-  reactive({
        nitrogen_data
      })
    
    unit_map <- c(
      "n_balance" = "kg N surplus/ha",
      "nue" = "%"
    )

    # Reactive tooltip unit text
    
    tooltip_unit <- reactive({
      req(input$n_type)
      unit_map[[input$n_type]]
    })
    
    table_data <- reactive({
      req(input$n_type)
      
     nitrogen_data %>%
        dplyr::filter(n_type == input$n_type) %>% 
       mutate(value=janitor::round_half_up(value, 2))
    })
    
    download_data <- reactive({
      req(input$n_type)
      
      nitrogen_data %>%
        dplyr::filter(n_type == input$n_type) %>% 
        rename("Farm type" = farm_type,
               Year=year,
              Value=value) %>% 
        select(Measure, `Farm type`, Year, Value)
    })
    
   
    # Function to get filtered chart data reactive for a given farm_type index
    get_filtered_chart_data <- function(chart_data, farm_type_index) {
      reactive({
        req(input$n_type)

        data <- chart_data() %>%
          filter(
            n_type == input$n_type,
            farm_type == farm_types[farm_type_index]
          )

  
        data
      })
    }

    # Render UI for each chart based on filtered data availability
    renderChartUI <- function(chart_id, farm_type_index, n_type) {
      filtered_chart_data <- get_filtered_chart_data(chart_data, farm_type_index)

      output[[paste0("chart", farm_type_index)]] <- renderUI({
        data <- filtered_chart_data()
       
            nitrogenline_ChartUI(ns(paste0("nitrogen_line_chart", farm_type_index)))
          
      })

    }

    # Server logic to render charts and respond to input changes
    renderChartServer <- function(line_id, farm_type_index) {
      filtered_chart_data <- get_filtered_chart_data(chart_data, farm_type_index)

      observeEvent(
        {
          list(
            input$n_type,
            input$data_type
          )
        },
        {
          req(input$n_type)
          
          title_map <- c(
            "n_balance" = "Nitrogen balance",
            "nue" = "Nitrogen use efficiency"
          )
          
          unit_map <- c(
            "n_balance" = "kg N surplus/ha",
            "nue" = "%"
          )
          
          
          df <- filtered_chart_data()
          
          chart_title <- paste0(
            farm_types[farm_type_index], ": ",
            title_map[input$n_type], ", ",
            min(df$year), " to ", max(df$year)
          )
          
          yaxis_title <- paste0(
            unit_map[input$n_type]
            
          )
          

          {
           nitrogenline_ChartServer(
              id = line_id,
              chart_data = filtered_chart_data,
              title =  chart_title,
              yAxisTitle = yaxis_title,
              xAxisTitle = "",
              footer = nitrogen_footer,
              x_col = "year",
              y_col= "value",
              unit = tooltip_unit
            )
          }
        },
        # ignoreNULL = TRUE,
        # ignoreInit = FALSE

      )
    }

    # Loop to initialize UI and server for each farm type
    for (i in seq_along(farm_types)) {
      local({
        farm_type_index <- i

        renderChartUI(nitrogen_line_chart, farm_type_index)
        
        renderChartServer( line_id = paste0("nitrogen_line_chart", farm_type_index),
                          farm_type_index = farm_type_index
        )
        
      })
    }

    # Render data table with conditional columns based on in_out_type
    output$data_table <- renderDT({
      
      df <- table_data()
      
      df %>%
        select(Measure, farm_type, year, value) %>%
        datatable(
          colnames = c("Measure", "Farm type", "Year", "Value"),
          options = list(pageLength = 20, scrollX = TRUE)
        )
    })
    
      

    # Download handler for CSV export of table data
    output$downloadData <- downloadHandler(
      filename = function() {
        paste("Nitrogen_", Sys.Date(), ".csv", sep = "")
      },
      content = function(file) {
        write.csv(download_data(), file, row.names = FALSE)
      }
    )
  })
}

# check for empty data:

#
## Testing module --------
source(here("testing", "test_multibarchart_function.R"))
source(here("utility", "util_updates.R"))
source(here("utility", "util_functions.R"))
source(here("utility", "hc_theme.R"))
source(here("utility", "util_options.R"))
source(here("charts_tables_functions", "nitrogen_function_line_chart.R"))
#source(here("testing", "test_fbs_function_line_chart.R"))



content_demo <- function() {
  ui <- fluidPage(nitrogenUI("test"))
  server <- function(input, output, session) {
      nitrogenServer("test")
  }
  shinyApp(ui, server)
}


#
content_demo()

