structureInformationUI <- function(id) {
  ns <- NS(id)
  tagList(
    div(
      class = "container",
      tags$div(style = "font-size: 24px; font-weight: bold;", "Other Sources:"),  # Bigger and bold title
      tags$div(
        style = "margin-top: 20px;",
        tags$div(style = "font-size: 18px; font-weight: bold;", "Scottish Agricultural Census: results"),
        p(HTML('More data on land use, crop areas, livestock, farm structure, and the number of people working on agricultural holdings are available in the <a href="https://www.gov.scot/collections/june-scottish-agricultural-census/" target="_blank">Scottish Agricultural Census: results</a>.')),
        tags$div(style = "font-size: 18px; font-weight: bold;", "Economic data"),
        p(HTML('Economic data about Scottish agriculture, including the value of production, costs, and support payments, are available through <a href="https://www.gov.scot/collections/total-income-from-farming/" target="_blank">Total income from farming</a>, <a href="https://www.gov.scot/publications/scottish-agriculture-economic-reports-2023/" target="_blank">Scottish agriculture: economic reports</a>, and <a href="https://www.gov.scot/collections/scottish-farm-business-income-fbi-annual-estimates/" target="_blank">Scottish farm business income estimates</a>.')),
        tags$div(style = "font-size: 18px; font-weight: bold;", "UK-wide Agricultural Data"),
      #   p(HTML('Useful links to data collected across the UK: <a href="https://www.gov.uk/government/collections/structure-of-the-agricultural-industry" target="_blank">Defra census</a>, <a href="https://www.daera-ni.gov.uk/topics/statistics/statistics-latest-releases" target="_blank">Northern Ireland statistics</a>, <a href="https://statswales.gov.wales/Catalogue/Agriculture" target="_blank">Wales agricultural statistics</a>.'))
      # ),
      
      p(HTML('Useful links to data collected across the UK:'),
        tags$ul(
          style = "list-style-type: disc; padding-left: 20px;",
          tags$li(
            tags$a(href = "https://www.gov.uk/government/collections/structure-of-the-agricultural-industry", target = "_blank", "Defra England and UK agricultural statistics")
          ),
          tags$li(
            tags$a(href = "https://www.daera-ni.gov.uk/topics/statistics/statistics-latest-releases", target = "_blank", "Northern Ireland statistics")
          ),
          tags$li(
            tags$a(href = "https://statswales.gov.wales/Catalogue/Agriculture", target = "_blank", "Wales agricultural statistics")
          )
        )
      ),
      
      tags$div(style = "font-size: 24px; font-weight: bold; margin-top: 40px;", "Glossary:"),  # Bigger and bold title
      tags$div(
        style = "margin-top: 20px;",
        tags$div(style = "font-size: 18px; font-weight: bold;", "Farm types"),
        p("Farms are classified into farm types based on the relative contribution of their farming activities. A farm is allocated to a farm type where at least two-thirds of their Standard Output is associated with that activity. Standard Output is the estimated worth of crops and livestock."),
        tags$div(style = "font-size: 18px; font-weight: bold;", "The main farm types reported on in Scotland are:"),
        tags$ul(
          style = "font-size: 16px;",
          tags$li("cattle and sheep in Less Favoured Areas (LFA)"),
          tags$li("cattle and sheep"),
          tags$li("cereals"),
          tags$li("dairy"),
          tags$li("general cropping"),
          tags$li("forage"),
          tags$li("horticulture and permanent crops"),
          tags$li("pigs"),
          tags$li("poultry"),
          tags$li("mixed"),
          tags$li("other")
        )
      )
    )
  )
  )
}

structureInformationServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # If you want to add any server-side logic, you can include it here
  })
}

# Testing the module
structureInformationDemo <- function() {
  ui <- fluidPage(structureInformationUI("structure_information_test"))
  server <- function(input, output, session) {
    structureInformationServer("structure_information_test")
  }
  shinyApp(ui, server)
}

# Uncomment the line below to run the test
# structureInformationDemo()
