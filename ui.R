source("util_updates.R")
source("util_functions.R")
source("util_options.R")
source("hc_theme.R")


source("function_summary.R")
source("function_line_chart.R")
source("function_area_chart.R")
source("function_bar_chart.R")
source("function_percentage_bar_chart.R")
source("function_timelapse_bar_chart.R")
source("function_breakdown_chart.R")
source("function_data_table.R")
source("function_regions_map.R")
source("function_map.R")


source("module_subsector_emissions.R")
source("module_information.R")
source("module_fertiliser_usage.R")
source("module_employees.R")
source("module_legal_responsibility.R")
source("module_farm_types.R")
source("module_occupiers.R")
source("module_land_use_summary.R")
source("module_owned_land.R")
source("module_cattle.R")
source("module_sheep.R")
source("module_pigs.R")
source("module_poultry.R")
source("module_other_animals.R")
source("module_cereals.R")
source("module_oilseed.R")
source("module_potatoes.R")
source("module_beans.R")
source("module_stockfeeding.R")
source("module_human_vegetables.R")
source("module_fruit.R")
source("module_home.R")
source("module_economy_summary.R")
source("module_animals_summary.R")
source("module_soil_testing.R")
source("module_manure.R")
source("module_nitrogen_usage.R")
source("module_crops_summary.R")
source("module_structure_information.R")
source("module_total_number_of_vehicles.R")
source("module_ag_mach_farm_type.R")
source("module_ag_mach_ownership.R")
source("module_ag_mach_fuel.R")


create_footer <- function() {
  div(
    class = "footer",
    span(style = "font-weight: bold;", "Content in development "),
    span("\n | Last updated: 29/10/2024"),
    img(src = "sg.png", alt = "SG Logo", style = "height: 30px; margin-left: 10px;")
  )
}


# Integrate the home module into the main UI
ui <- fluidPage(
  useShinyjs(),  # Initialize shinyjs
  theme = shinytheme("flatly"),
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
    includeHTML("google-analytics.html"),
    tags$script(HTML("
      $(document).on('click', 'a[data-value=\"home\"]', function() {
        history.pushState(null, '', '/AgStatsHub');
      });
    "))
  ),
  div(class = "container-fluid full-height",
      div(class = "content",
          navbarPage(
            title = div(
              tags$li(class = "nav-item", img(src = "RESAS Logo.png", class = "header-logo"))
            ),
            id = "navbar",
            windowTitle = "RESAS Agricultural Statistics Hub",  # Set the name for the browser tab
            tabPanel("Home", value = "home", homeUI("home")),  # Set this tabPanel as the default page
            navbarMenu("Structure",
                       tabPanel("Land Use", value = "land_use", landUseSummaryUI("land_use")),
                       tabPanel("Farm Types", value = "farm_types", farmTypesUI("farm_types")),
                       tabPanel("Employees", value = "employees", employeesMapUI("employees")),
                       tabPanel("Occupiers", value = "occupiers", occupiersUI("occupiers")),
                       tabPanel("Ownership Status", value = "owned_land", ownedLandUI("owned_land")),
                       tabPanel("Legal Responsibility", value = "legal_responsibility", legalResponsibilityUI("legal_responsibility")),
                       tabPanel("Further Information", value = "structure_information", structureInformationUI("structure_information"))
            ),
            navbarMenu("Agri-Environment",
                       tabPanel("Agriculture Emissions", value = "subsector", subsectorEmissionsUI("subsector")),
                       tabPanel("Nitrogen Usage", value = "nitrogen", nitrogenUI("nitrogen")),
                       tabPanel("Manure Usage", value = "manure", manureUI("manure")),
                       tabPanel("Soil Testing", value = "soil", soilTestingUI("soil")),
                       tabPanel("Fertiliser Usage", value = "fertiliser", fertiliserUsageUI("fertiliser")),
                       tabPanel("Further Information", value = "info", informationUI("info"))
            ),
            navbarMenu("Livestock",
                       tabPanel("Summary", value= "animals_summary_module", animalsSummaryUI("animals_summary_module")),
                       tabPanel("Cattle", value = "cattle_module", cattleUI("cattle_module")),
                       tabPanel("Sheep", value = "sheep_module", sheepUI("sheep_module")),
                       tabPanel("Pigs", value = "pigs_module", pigsUI("pigs_module")),
                       tabPanel("Poultry", value = "poultry_module", poultryUI("poultry_module")),
                       tabPanel("Other Animals", value = "other_animals_module", otherAnimalsUI("other_animals_module"))
            ),
            navbarMenu("Crops",
                       tabPanel("Summary", value = "crops_summary_module", cropsSummaryUI("crops_summary_module")),
                       tabPanel("Cereals", value = "cereals_module", cerealsUI("cereals_module")),
                       tabPanel("Oilseed", value = "oilseed_module", oilseedUI("oilseed_module")),
                       tabPanel("Potatoes", value = "potatoes_module", potatoesUI("potatoes_module")),
                       tabPanel("Beans", value = "beans_module", beansUI("beans_module")),
                       tabPanel("Stockfeeding", value = "stockfeeding_module", stockfeedingUI("stockfeeding_module")),
                       tabPanel("Vegetables", value = "human_vegetables_module", humanVegetablesUI("human_vegetables_module")),
                       tabPanel("Fruit", value = "fruit_module", fruitUI("fruit_module"))
            ),
            navbarMenu("Economy",
                       tabPanel("Summary", value = "economy", economySummaryUI("economy"))
            ),
            navbarMenu("Machinery",
                       tabPanel("Total number", value = "total_vehicles_module", totalnumberofvehiclesUI("total_vehicles_module")),
                       tabPanel("Farm type", value = "ag_mach_farm_type_module", agmachfarmtypeUI("ag_mach_farm_type_module")),
                       tabPanel("Ownership", value = "ag_mach_ownership_module", agmachownershipUI("ag_mach_ownership_module")),
                       tabPanel("Fuel", value = "ag_mach_fuel_module", agmachfuelUI("ag_mach_fuel_module"))
                                ))
          ),
          create_footer()
      )
  )

