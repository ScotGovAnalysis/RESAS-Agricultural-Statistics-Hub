# server.R



server <- function(input, output, session) {
  # Maintain selected tab state based on URL query parameters
  observe({
    query <- parseQueryString(session$clientData$url_search)
    page <- query$page
    if (!is.null(page)) {
      updateTabsetPanel(session, "navbar", selected = page)
    }
  })
  
  observeEvent(input$navbar, {
    query <- parseQueryString(session$clientData$url_search)
    page <- query$page
    if (is.null(page) || input$navbar != page) {
      if (input$navbar != "home") {
        updateQueryString(paste0("?page=", input$navbar), mode = "push")
      }
    }
  })
  
  
  
  subsectorEmissionsServer("subsector")
  fertiliserUsageServer("fertiliser")
  employeesMapServer("employees")
  landUseSummaryServer("land_use")
  legalResponsibilityServer("legal_responsibility")
  farmTypesServer("farm_types")
  occupiersServer("occupiers")
  ownedLandServer("owned_land")
  cattleServer("cattle_module")
  sheepServer("sheep_module")
  pigsServer("pigs_module")
  poultryServer("poultry_module")
  otherAnimalsServer("other_animals_module")
  cerealsServer("cereals_module")
  oilseedServer("oilseed_module")
  potatoesServer("potatoes_module")
  beansServer("beans_module")
  stockfeedingServer("stockfeeding_module")
  humanVegetablesServer("human_vegetables_module")
  fruitServer("fruit_module")
  homeServer("home")
  animalsSummaryServer("animals_summary_module")
  soilTestingServer("soil")
  manureServer("manure")
  nitrogenServer("nitrogen")
  cropsSummaryServer("crops_summary_module")
  totalnumberofvehiclesServer("total_vehicles_module")
  agmachfarmtypeServer("ag_mach_farm_type_module")
  agmachownershipServer("ag_mach_ownership_module")
  agmachfuelServer("ag_mach_fuel_module")
  CostOutServer("module_fbs")
}
