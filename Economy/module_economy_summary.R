# module_economy_summary.R

economySummaryUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidPage(
      h3(strong("Content under development")),
      p("Estimates for the net income gained by the agriculture industry in Scotland are available in the ",
        a("Total income from farming (TIFF) publication.", href = "https://www.gov.scot/collections/total-income-from-farming/", target = "_blank")),
      p("Farm business level estimates of average incomes from commercial farms in Scotland are available in the ",
        a("Scottish farm business income publication.", href = "https://www.gov.scot/collections/scottish-farm-business-income-fbi-annual-estimates/", target = "_blank")),
      tags$div(
        style = "margin-top: 20px; font-size: 20px; font-weight: bold;",
        "Total income from farming: estimates"
      ),
      tags$p(
        style = "font-size: 16px;",
        "Total income from farming (TIFF) is the official measure of the profit gained by the agricultural industry in Scotland. It provides a breakdown of the value of farm production, support payments and costs. Estimates for the net income gained by the agriculture industry in Scotland are available in the ",
        tags$a(
          "Total income from farming (TIFF) publication",
          href = "https://www.gov.scot/collections/total-income-from-farming/",
          target = "_blank"  # opens in new tab
        ),
        "."
      ),
      fluidRow(
        column(width = 6, valueBoxEconomyUI(ns("Total income from farming")), style = "padding-right: 0; padding-left: 0; padding-bottom: 10px;")
      )
      )
    )}

economySummaryServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    tiff_filtered <- main_tiff_data_long %>%
      filter(
        Measure == "Total income from farming",
        Price == "Current (nominal)"
      ) %>%
      select(Year, Value, Measure) %>%
      mutate(Value = round(Value * 1000, 0))  # KEEP numeric
    
    valueBoxEconomyServer(
      id = "Total income from farming",
      data = reactive(tiff_filtered),
      category = "Measure",
      industry = reactive("Total income from farming"),
      current_year = reactive(tiff_year),
      comparison_year = reactive(tiff_year - 10),
      unit = NULL
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