# module_economy_summary.R

economySummaryUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    tags$head(
      tags$style(HTML("
    body, html {
      background-color: #ffffff !important;
    }
    .panel-like {
      background: #ffffff;  /* or keep #f9f9fb if you like subtle contrast */
      border: 1px solid #e5e5e5;
      border-radius: 8px;
      padding: 16px;
      height: 100%;
    }
    /* Make the top row columns equal height */
    .row-equal { display: flex; flex-wrap: wrap; }
    .row-equal > [class*='col-'] { display: flex; }
    .row-equal .panel-like { width: 100%; }
  "))
    ),
    
    fluidPage(
      # ---- Top row: two side-by-side panels ----
      fluidRow(class = "row-equal",
               column(
                 width = 6,
                 div(class = "panel-like",
                     div(style = "margin-top: 4px; font-size: 20px; font-weight: bold;",
                         "Farm business income"),
                     tags$p(style = "font-size: 16px; margin-top: 8px;",
                        "Farm business income measures average total income from commercial farms in Scotland, including income from diversified activities. More farm level income estimates are available in the ",
                       a("Scottish farm business income publication.",
                         href = "https://www.gov.scot/collections/scottish-farm-business-income-fbi-annual-estimates/",
                         target = "_blank"
                       )
                     )
                 )
               ),
               column(
                 width = 6,
                 div(class = "panel-like",
                     div(style = "margin-top: 4px; font-size: 20px; font-weight: bold;",
                         "Total income from farming"),
                     tags$p(
                       style = "font-size: 16px; margin-top: 8px;",
                       "Total income from farming (TIFF) is the official measure of the profit gained by the agricultural industry in Scotland. ",
                       "It provides a breakdown of the value of farm production, support payments and costs. ",
                       "TIFF is the total profit from all farming businesses within the agricultural industry in Scotland. ",
                       "It measures the return to all entrepreneurs for their management, inputs, labour and capital invested, on a calendar year basis.",
                       "Estimates for the net income gained by the agriculture industry in Scotland are available in the ",
                       tags$a(
                         "Total income from farming (TIFF) publication",
                         href = "https://www.gov.scot/collections/total-income-from-farming/",
                         target = "_blank"
                       ),
                     div(
                       style = "padding: 0; margin-top: 10px;",
                       fluidRow(
                         column(width = 12, valueBoxEconomyUI(ns("Total income from farming")))
                        )
                      )
                    )
                   )
                  )
                 ),
      
      # ---- Bottom row: full-width panel ----
      fluidRow(
        column(
          width = 12,
          div(class = "panel-like",
              h3(strong("Glossary")),
              div(style = "font-size: 16px; margin-top: 8px;",
              tags$ul(
                tags$li(
                  tags$b("FBI:"), 
                  " The total income available to all unpaid labour and their capital invested in the business. Income from diversified activities are included in overall FBI."
                ),
                tags$li(
                  tags$b("TIFF:"), 
                  " Total Income From Farming — the official measure of agricultural industry profit."
                ),
                tags$li(
                  tags$b("Current (nominal) prices:"), 
                  " Current (nominal) prices make no adjustment for inflation. These measure prices and inflation using the actual prices in that particular year. For example, current price estimates shown for 2020 are based on 2020 prices."
                ),
                tags$li(
                  tags$b("Real (constant) prices:"), 
                  " Real (constant) prices use the latest gross domestic product GDP deflators to convert historic figures into prices representing prices in the most recent year."
                ),
                tags$li(
                  tags$b("Gross Domestic Product (GDP):"), 
                  " GDP is a measure of the value of goods and services produced by residents, before allowing for depreciation or capital consumption. Net receipts from interest, profits and dividends abroad are excluded."
                ),
                tags$li(
                  tags$b("Gross value added (GVA):"), 
                  " GVA is a measure of the net contribution of the industry to the wider economy. It is the value of total output minus the intermediate costs that went into producing the output, such as raw materials and services. GVA represents the income generated by businesses out of which is paid wages and salaries, the cost of capital investment and financial charges, before arriving at a figure for profit. For agriculture, this figure is calculated before support payments, and costs such as labour, rent, taxes and interest are taken into account to produce the estimate of total income from farming."
                ),
                tags$li(
                  tags$b("Farm types:"), 
                  " Farms are classified based on how much of their standard output is from the crop and livestock enterprises on each farm."
                ),
                tags$li(
                  tags$b("Less Favoured Area (LFA):"), 
                  " Land where farming is more difficult due to natural constraints, such as hills and soil quality."
                )
              )
          )
        )
      )
    )
  )
)}

economySummaryServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    tiff_filtered <- main_tiff_data_long %>%
      filter(
        Measure == "23. Total income from farming (19-20-21)",
        Price == "Current (nominal)"
      ) %>%
      select(Year, Value, Measure) %>%
      mutate(
        Value = round(Value * 1000, 0),
        Measure = "Total income from farming"  # rename here
      )
    
    valueBoxEconomyServer(
      id = "Total income from farming",
      data = reactive(tiff_filtered),
      category = "Measure",
      industry = reactive("Total income from farming"),  # actual data value
      current_year = reactive(tiff_year),
      comparison_year = reactive(tiff_year - 10),
      unit = NULL,
      display_title = "Value of TIFF in Scotland"   # this will be displayed in the box
    )
  })
}


#Testing module
content_demo <- function() {
  ui <- fluidPage(economySummaryUI("summary_economy_test"))
  server <- function(input, output, session) {
    economySummaryServer("summary_economy_test")
  }
  shinyApp(ui, server)
}

content_demo()
