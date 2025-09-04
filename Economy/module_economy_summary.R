# economySummaryUI <- function(id) {
#   ns <- NS(id)
#   # min_econ_year <- as.numeric(substr(year_levels[1], 1, 4))
  #max_econ_year <- as.numeric(tiff_year)
  # tags$head(tags$style(
  #   HTML(
  #     "
  #   body, html {
  #     background-color: #ffffff !important;
  #   }
  # 
  #   .row-equal {
  #     display: flex;
  #     flex-wrap: wrap;
  #   }
  # 
  #   .row-equal > [class*='col-'] {
  #     display: flex;
  #     flex-direction: column;
  #   }
  # 
  #   .panel-like {
  #     flex: 1;
  #     display: flex;
  #     flex-direction: column;
  #     justify-content: space-between;
  #     background: #ffffff;
  #     border: 1px solid #e5e5e5;
  #     border-radius: 8px;
  #     padding: 16px;
  #   }
  #   "
  #   )
  # ),
  # 
  # tags$head(
  #  tags$style(HTML("
  #     .economy-row {
  #       display: flex;
  #       flex-wrap: nowrap;
  #     }
  #     .economy-col {
  #       flex: 1;
  #       display: flex;
  #       flex-direction: column;
  #     }
  #     .economy-panel {
  #       flex: 1;
  #       display: flex;
  #       flex-direction: column;
  #       justify-content: space-between;
  #       background: #ffffff;
  #       border: 1px solid #e5e5e5;
  #       border-radius: 8px;
  #       padding: 16px;
  #     }")),
  
   economySummaryUI <- function(id) {
     ns <- NS(id)
     tagList(
       # Scoped CSS styles for this module only
       tags$style(HTML("
      .economy-row-equal {
        display: flex;
        flex-wrap: nowrap;
      }
      .economy-col-flex {
        flex: 1;
        display: flex;
        flex-direction: column;
      }
      .economy-panel-like {
        flex: 1;
        display: flex;
        flex-direction: column;
        justify-content: space-between;
        background: #ffffff;
        border: 1px solid #e5e5e5;
        border-radius: 8px;
        padding: 16px;
      }
    ")),
       
       sidebarLayout(
         sidebarPanel(
           width = 3,
           div(
             "Adjust the sliders to compare data from different years.",
             "The top two sliders will adjust TIFF data, the bottom two will adjust FBI.",
             style = "font-size: 14px; font-weight: bold; margin-bottom: 10px;"
           ),
           sliderInput(
             ns("tiff_summary_current_year"),
             "TIFF year of interest",
             min = tiff_year_min,
             max = tiff_year,
             value = tiff_year,
             step = 1,
             sep = ""
           ),
           sliderInput(
             ns("tiff_summary_comparison_year"),
             "TIFF comparison year",
             min = tiff_year_min,
             max = tiff_year,
             value = (tiff_year-10),
             step = 1,
             sep = ""
           ),
           sliderTextInput(
             inputId = ns("fbs_summary_current_year"),
             label = "FBI year of interest",
             choices = year_levels,
             selected = current_year,
             grid = TRUE
           ),
           sliderTextInput(
             inputId = ns("fbs_summary_comparison_year"),
             label = "FBI comparison year",
             choices = year_levels,
             selected = "2012-13",
             grid = TRUE
           )
         ),
         
         mainPanel(
           width = 9,
           fluidRow(class = "economy-row-equal",
                    div(class = "economy-col-flex",
                        div(class = "economy-panel-like",
                            div(style = "margin-top: 4px; font-size: 20px; font-weight: bold;", "Total income from farming"),
                            tags$p(
                              style = "font-size: 16px; margin-top: 8px;",
                              "Total income from farming (TIFF) is the total profit from all farming businesses within the agricultural industry in Scotland. ",
                              "This data is sourced from the",
                              tags$a(
                                "total income from farming (TIFF) publication",
                                href = "https://www.gov.scot/collections/total-income-from-farming/",
                                target = "_blank"
                              ),
                              "which was published on 29 May 2025. Full data tables and detailed analysis are available within the full report."
                            ),
                            div(style = "padding: 0; margin-top: 10px;",
                                fluidRow(column(width = 12, valueBoxEconomyUI(ns("tiff"))))
                            )
                        )
                    ),
                    div(class = "economy-col-flex",
                        div(class = "economy-panel-like",
                            div(style = "margin-top: 4px; font-size: 20px; font-weight: bold;", "Farm business income"),
                            tags$p(
                              style = "font-size: 16px; margin-top: 8px;",
                              "Farm business income measures average total income from commercial farms in Scotland, including income from diversified activities. More farm level average income estimates are available in the ",
                              tags$a(
                                "Scottish farm business income publication.",
                                href = "https://www.gov.scot/collections/scottish-farm-business-income-fbi-annual-estimates/",
                                target = "_blank"
                              )
                            ),
                            div(style = "padding: 0; margin-top: 10px;",
                                fluidRow(column(width = 12, valueBoxEconomyUI(ns("fbs"))))
                            )
                        )
                    )
           ),
           
           fluidRow(column(
             width = 12,
             div(class = "economy-panel-like",
                 h3(strong("Glossary")),
                 div(style = "font-size: 16px; margin-top: 8px;",
                     tags$ul(
                       tags$li(tags$b("Total income from farming (TIFF):"), " The official measure of agricultural industry profit."),
                       tags$li(tags$b("Farm Business income (FBI):"), " The total income available to all unpaid labour and their capital invested in the business."),
                       tags$li(tags$b("Current (nominal) prices:"), " Current (nominal) prices make no adjustment for inflation. These measure prices and inflation using the actual prices in that particular year. For example, current price estimates shown for 2020 are based on 2020 prices."),
                       tags$li(tags$b("Real (constant) prices:"), " Real (constant) prices use the latest gross domestic product GDP deflators to convert historic figures into prices representing prices in the most recent year."),
                       tags$li(tags$b("Gross Domestic Product (GDP):"), " GDP is a measure of the value of goods and services produced by residents, before allowing for depreciation or capital consumption. Net receipts from interest, profits and dividends abroad are excluded."),
                       tags$li(tags$b("Gross value added (GVA):"), " GVA is a measure of the net contribution of the industry to the wider economy.
                               It is the value of total output minus the intermediate costs that went into producing the output, such as raw materials and services. GVA represents the income generated by businesses out of which is paid wages and salaries, the cost of capital investment and financial charges, before arriving at a figure for profit. 
                               For agriculture, this figure is calculated before support payments, and costs such as labour, rent, taxes and interest are taken into account to produce the estimate of total income from farming.")
                     )
                 )
             )
           ))
         )
       )
     )
   }
   
economySummaryServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    tiff_current_year <-  reactive(input$tiff_summary_current_year)
    tiff_comparison_year <- reactive(input$tiff_summary_comparison_year)
    fbs_current_year <-  reactive(input$fbs_summary_current_year)
    fbs_comparison_year <- reactive(input$fbs_summary_comparison_year)
    
    #Data process ------
    tiff_filtered <- main_tiff_data_long %>%
      filter(Measure == "23. Total income from farming (19-20-21)",
             Price == "Real (constant 2024)") %>% # FBI is real!!
      select(Year, Value, Measure) %>%
      mutate(Value = signif(Value * 1000, 2), Measure = "Total income from farming")
    
    fbi_timeseries <- fbs_income %>%
      filter(grepl("farm business income", Measure)) %>%
      filter(farm_type == "All farms") %>%
      mutate(Value = signif(value, 2)) %>%
      rename(Year = year)
    
    # chart logic ------
    valueBoxEconomyServer(
      id = "tiff",
      data = reactive(tiff_filtered),
      category = "Total income from farming",
      current_year = tiff_current_year,
      comparison_year = tiff_comparison_year,
      unit = "   real (constant 2024) prices" # change if not
      #display_title = "Value of TIFF in Scotland"
    )
    
    valueBoxEconomyServer(
      id = "fbs",
      data = reactive(fbi_timeseries),
      category = "Average farm business income",
      current_year = fbs_current_year,
      comparison_year = fbs_comparison_year,
      unit = "   real (constant) prices"
      # display_title = "Average farm business income (FBI) of commercial farms in Scotland"
    )
  })
}

# Testing module
content_demo <- function() {
  ui <- fluidPage(economySummaryUI("summary_economy_test"))
  server <- function(input, output, session) {
    economySummaryServer("summary_economy_test")
  }
  shinyApp(ui, server)
}

content_demo()
