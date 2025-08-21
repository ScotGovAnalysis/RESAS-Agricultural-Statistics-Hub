# Define UI for the home module
homeUI <- function(id) {
  ns <- NS(id)
  fluidPage(
    sidebarLayout(
      sidebarPanel(
        width = 3,
        tags$div(
          style = "margin-top: 0px; font-size: 24px; font-weight: bold;",
          "Navigation"
        ),
        tags$div(
          style = "margin-top: 20px; font-size: 20px; font-weight: bold;",
          "Structure"
        ),
        tags$ul(
          tags$li(actionLink(ns("nav_land_use"), "Land Use")),
          tags$li(actionLink(ns("nav_farm_types"), "Farm Types")),
          tags$li(actionLink(ns("nav_employees"), "Employees")),
          tags$li(actionLink(ns("nav_occupiers"), "Occupiers")),
          tags$li(actionLink(ns("nav_owned_land"), "Ownership Status")),
          tags$li(actionLink(ns("nav_legal_responsibility"), "Legal Responsibility")),
          tags$li(actionLink(ns("nav_structure_information"), "Further Information"))
        ),
        tags$div(
          style = "margin-top: 20px; font-size: 20px; font-weight: bold;",
          "Agri-Environment"
        ),
        tags$ul(
          tags$li(actionLink(ns("nav_subsector"), "Agriculture Emissions")),
          tags$li(actionLink(ns("nav_nitrogen"), "Nitrogen Usage")),
          tags$li(actionLink(ns("nav_manure"), "Manure Usage")),
          tags$li(actionLink(ns("nav_soil"), "Soil Testing")),
          tags$li(actionLink(ns("nav_fertiliser"), "Fertiliser Usage")),
          tags$li(actionLink(ns("nav_info"), "Further Information"))
        ),
        tags$div(
          style = "margin-top: 20px; font-size: 20px; font-weight: bold;",
          "Livestock"
        ),
        tags$ul(
          tags$li(actionLink(ns("nav_animals_summary_module"), "Summary")),
          tags$li(actionLink(ns("nav_cattle_module"), "Cattle")),
          tags$li(actionLink(ns("nav_sheep_module"), "Sheep")),
          tags$li(actionLink(ns("nav_pigs_module"), "Pigs")),
          tags$li(actionLink(ns("nav_poultry_module"), "Poultry")),
          tags$li(actionLink(ns("nav_other_animals_module"), "Other Animals"))
        ),
        tags$div(
          style = "margin-top: 20px; font-size: 20px; font-weight: bold;",
          "Crops"
        ),
        tags$ul(
          tags$li(actionLink(ns("nav_crops_summary_module"), "Summary")),
          tags$li(actionLink(ns("nav_cereals_module"), "Cereals")),
          tags$li(actionLink(ns("nav_oilseed_module"), "Oilseed")),
          tags$li(actionLink(ns("nav_potatoes_module"), "Potatoes")),
          tags$li(actionLink(ns("nav_beans_module"), "Beans")),
          tags$li(actionLink(ns("nav_stockfeeding_module"), "Stockfeeding")),
          tags$li(actionLink(ns("nav_human_vegetables_module"), "Vegetables")),
          tags$li(actionLink(ns("nav_fruit_module"), "Fruit"))
        ),
        tags$div(
          style = "margin-top: 20px; font-size: 20px; font-weight: bold;",
          "Economy"
        ),
        tags$ul(
          tags$li(actionLink(ns("nav_economy"), "Economy")), 
          tags$li(actionLink(ns("nav_module_fbs"), "Farm level economic data")),
          tags$li(actionLink(ns("nav_tiff_module"), "Total Income From Farming"))
        )
      ),
      mainPanel(
        width = 9,
        tags$div(
          style = "margin-top: 20px; font-size: 24px; font-weight: bold;",
          "Welcome to the RESAS Agricultural Statistics Hub - in development"
        ),
        
        HTML("<br>"),
        tags$p(
          style = "font-size: 16px;",
          "This hub provides access to a variety of agricultural statistics and data visualisations developed by the RESAS (Rural & Environment Science & Analytical Services) division of the Scottish Government. This content is still in development. We would be grateful if you could fill in our ",
          tags$a(href = "https://forms.office.com/e/Y9Eixgf4c1", target = "_blank", "feedback survey"),
          ". You can also email us at ",
          tags$a(href = "mailto:agric.stats@gov.scot", "agric.stats@gov.scot"),
          "."
        ),
        tags$div(
          style = "margin-top: 20px; font-size: 20px; font-weight: bold;",
          "How to use the site"
        ),
        tags$p(
          style = "font-size: 16px;",
          "The site is divided into several sections, each accessible via the navigation bar at the top of the page. Each section contains interactive elements such as charts, tables, and maps, which can be selected through the sub-menu on each page. Here are some tips on how to interact with the site's features:"
        ),
        tags$ul(
          style = "font-size: 16px;",
          tags$li("Charts: Hover over chart elements to see detailed data. Click and drag on an area of the graph to zoom in to see an area in more detail. Press the reset zoom button to return to the default view."),
          tags$li("Checkboxes: Select or deselect checkboxes to include or exclude variables from the charts."),
          tags$li("Sliders: Adjust the sliders to filter data based on specific years."),
          tags$li("Multiple inputs: Use the dropdown menus on some pages by clicking within the white box to select specific data categories. You can also type in the dropdown to quickly find the options you need."),
          tags$li("Data tables: Click on table headers to sort data. Use the search box to filter table contents. Click the download data button to save the data for your own use."),
          tags$li("Maps: Hover over regions on the map to see detailed statistics. Use the zoom and pan controls to navigate the map.")
        ),
        tags$div(
          style = "margin-top: 20px; font-size: 20px; font-weight: bold;",
          "Data sources"
        ),
        tags$p(
          style = "font-size: 16px;",
          "The data presented on this site is sourced from various RESAS publications, which provide comprehensive statistics on Scottish agriculture. These publications cover topics such as land use, farm types, employee demographics, livestock numbers, crop production, and agricultural emissions. Full results are available within each publication, whilst complete data tables are available within the supporting documentation of each publication."
        ),
        tags$div(
          style = "margin-top: 20px; font-size: 16px; font-weight: bold;",
          "Publications include:"
        ),
        tags$ul(
          style = "font-size: 16px;",
          tags$li(
            tags$a(href = "https://www.gov.scot/collections/june-scottish-agricultural-census/", target = "_blank", "Scottish Agricultural Census: results")
          ),
          tags$li(
            tags$a(href = "https://www.gov.scot/collections/scottish-farm-business-income-fbi-annual-estimates/", target = "_blank", "Scottish farm business income (FBI)")
          ),
          tags$li(
            tags$a(href = "https://www.gov.scot/collections/scottish-cereal-harvest-estimates/", target = "_blank", "Scottish cereal harvest: estimates")
          ),
          tags$li(
            tags$a(href = "https://www.gov.scot/collections/total-income-from-farming/", target = "_blank", "Total income from farming: estimates")
          ),
          tags$li(
            tags$a(href = "https://www.gov.scot/collections/scottish-agriculture-greenhouse-gas-emissions-and-nitrogen-use/", target = "_blank", "Scottish agriculture greenhouse gas emissions and nitrogen use")
          ),
          tags$li(
            tags$a(href = "https://www.gov.scot/collections/economic-report-on-scottish-agriculture/", target = "_blank", "Scottish agriculture: economic reports")
          )
        ),
        tags$div(
          style = "margin-top: 20px; font-size: 20px; font-weight: bold;",
          "Feedback survey"
        ),
        tags$p(
          style = "font-size: 16px;",
          HTML("This content is still in development. We would be grateful if you could fill in our <a href='https://forms.office.com/e/Y9Eixgf4c1' target='_blank'>feedback survey</a>. You can also email us at <a href='mailto:agric.stats@gov.scot'>agric.stats@gov.scot</a>.")
        )
      )
    )
  )
}

homeServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # List of pages and their corresponding tab values
    pages <- list(
      land_use = "land_use",
      farm_types = "farm_types",
      employees = "employees",
      occupiers = "occupiers",
      owned_land = "owned_land",
      legal_responsibility = "legal_responsibility",
      subsector = "subsector",
      nitrogen = "nitrogen",
      manure = "manure",
      soil = "soil",
      fertiliser = "fertiliser",
      info = "info",
      animals_summary_module = "animals_summary_module",  # Working summary animals module
      cattle_module = "cattle_module",
      sheep_module = "sheep_module",
      pigs_module = "pigs_module",
      poultry_module = "poultry_module",
      other_animals_module = "other_animals_module",
      crops_summary_module = "crops_summary_module",  # Adding the crops summary module
      cereals_module = "cereals_module",
      oilseed_module = "oilseed_module",
      potatoes_module = "potatoes_module",
      beans_module = "beans_module",
      stockfeeding_module = "stockfeeding_module",
      human_vegetables_module = "human_vegetables_module",
      fruit_module = "fruit_module",
      structure_information = "structure_information",
      economy = "economy_summary_module",
      module_fbs = "module_fbs",
      tiff_module = "tiff_module"
    )
    
    # Set up observeEvent for each page
    lapply(names(pages), function(page) {
      observeEvent(input[[paste0("nav_", page)]], {
        updateTabsetPanel(session, "navbar", selected = pages[[page]])
        updateQueryString(paste0("?page=", pages[[page]]), mode = "push")
      })
    })
  })
}
