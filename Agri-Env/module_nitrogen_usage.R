n_balance<-table_5_df %>% 
  mutate(n_type="n_balance") %>% 
  pivot_longer(
    cols = `2019-20`:fbs_current_year,
    names_to = "year",
    values_to="value") %>% 
  tidyr::pivot_wider(
    names_from = Measure,
    values_from = value)


nue<-table_6_df %>% 
  mutate(n_type="nue") %>% 
  pivot_longer(
    cols = `2019-20`:fbs_current_year,
    names_to = "year",
    values_to="value") %>% 
  tidyr::pivot_wider(
    names_from = Measure,
    values_from = value)


nitrogen_data<-dplyr::bind_rows(n_balance, nue) %>% 
  rename(farm_type=`Farm type`)%>% 
  rename(Lower=`95% CI (lower limit)`,
         Upper=`95% CI (upper limit)`,
         Median=`Average (median)`) %>% 
  data.frame()

test<-nitrogen_data %>%
  pivot_longer(
    cols = Median:Upper,
    names_to = "Estimate",
    values_to="value") %>%  
  mutate(value=janitor::round_half_up(value, 2))


nitrogenUI<- function(id) {
  ns <- NS(id)
  tagList(
    sidebarLayout(
          sidebarPanel(
            width = 3,
            radioButtons(ns("n_type"), 
                         "Measure", choices = c("Nitrogen balance" = "n_balance", "Nitrogen use efficiency" = "nue"), selected = "n_balance"),
        
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
                   generateNitrogenTableFooter(),
                   value = ns("data"))
        )
      )
    )
  )
}


nitrogenServer<- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Reactive expression that returns the chart data based on n_type
    chart_data <-  reactive({
      nitrogen_data
    })
    
    
    table_data <- reactive({
      req(input$n_type)
      
      nitrogen_data %>%
        dplyr::filter(n_type == input$n_type) %>% 
        pivot_longer(
          cols = Median:Upper,
          names_to = "Estimate",
          values_to="value") %>%  
        mutate(value=janitor::round_half_up(value, 2)) %>% 
        rename(Measure=n_type) %>% 
        mutate(Measure = if_else(
          Measure == "n_balance",
          "Nitrogen balance ",
          "Nitrogen use efficiency"
        )) %>% 
        mutate(Estimate= if_else(
          Estimate == "Median",
          "Average (median)",
          ifelse(Estimate == "Lower",
                 "95% CI (lower limit)",
                 "95% CI (upper limit)")))
      
    })
    
    download_data <- reactive({
      req(input$n_type)
      
      nitrogen_data %>%
        dplyr::filter(n_type == input$n_type) %>% 
        pivot_longer(
          cols = Median:Upper,
          names_to = "Estimate",
          values_to="value") %>% 
        rename("Farm type" = farm_type,
               Year=year,
               Value=value,
               Measure=n_type) %>%         
        select(Measure, `Farm type`, Year, Estimate, Value) %>% 
        mutate(Measure = if_else(
          Measure == "n_balance",
          "Nitrogen balance (kg N surplus/ha)",
          "Nitrogen use efficiency (%)"
        )) %>% 
        mutate(Estimate= if_else(
          Estimate == "Median",
          "Average (median)",
          ifelse(Estimate == "Lower",
          "95% CI (lower limit)",
          "95% CI (upper limit)")
        ))
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
          
          yaxistitle <- paste0(
            unit_map[input$n_type]
            
          )
          
          yrange <- switch(
            input$n_type,
            "n_balance" = c(-16,max(df$Upper)),
            "nue" = c(0, max(df$Upper))
          )
          
          
          {
            nitrogenline_ChartServer(
              id = line_id,
              chart_data = filtered_chart_data,
              title =  chart_title,
              yaxistitle = yaxistitle,
              yrange=yrange
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
        
        renderChartServer(line_id = paste0("nitrogen_line_chart", farm_type_index),
                           farm_type_index = farm_type_index
        )
        
      })
    }
    
    # Render data table with conditional columns based on in_out_type
    output$data_table <- renderDT({
      
      df <- table_data()
      
      df %>%
        select(Measure, farm_type, year, Estimate, value) %>%
        datatable(
          colnames = c("Measure", "Farm type", "Year", "Estimate", "Value"),
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


# #
# ## Testing module --------
## remember to comment out when finished - otherwise will get a "hc_theme" error message when running app
# 
# source(here("utility", "util_updates.R"))
# source(here("utility", "util_functions.R"))
# source(here("utility", "hc_theme.R"))
# source(here("utility", "util_options.R"))
# 
# 
# 
# 
# 
# content_demo <- function() {
#   ui <- fluidPage(nitrogenUI("test"))
#   server <- function(input, output, session) {
#     nitrogenServer("test")
#   }
#   shinyApp(ui, server)
# }
# 
# 
# #
# content_demo()
