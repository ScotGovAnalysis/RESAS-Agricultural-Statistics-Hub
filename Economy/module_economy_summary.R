# module_economy_summary.R

economySummaryUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidPage(
      h3(strong("Content under development")),
      p("Estimates for the net income gained by the agriculture industry in Scotland are available in the ",
        a("Total income from farming (TIFF) publication.", href = "https://www.gov.scot/collections/total-income-from-farming/", target = "_blank")),
      p("Farm business level estimates of average incomes from commercial farms in Scotland are available in the ",
        a("Scottish farm business income publication.", href = "https://www.gov.scot/collections/scottish-farm-business-income-fbi-annual-estimates/", target = "_blank"))
    )
  )
}

economySummaryServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    # Add server logic if necessary
  })
}
