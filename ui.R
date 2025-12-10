source("utility/util_updates.R")
source("utility/util_functions.R")
source("utility/util_options.R") # data load
source("utility/hc_theme.R")

### Load scripts with functions used to create charts in module scripts
source("charts_tables_functions/function_summary.R")
source("charts_tables_functions/function_line_chart.R")
source("charts_tables_functions/function_area_chart.R")
source("charts_tables_functions/function_bar_chart.R")
source("charts_tables_functions/function_percentage_bar_chart.R")
source("charts_tables_functions/function_timelapse_bar_chart.R")
source("charts_tables_functions/function_breakdown_chart.R")
source("charts_tables_functions/function_data_table.R")
source("charts_tables_functions/function_regions_map.R")
source("charts_tables_functions/function_map.R")
source("charts_tables_functions/function_multi_bar_chart.R")
source("charts_tables_functions/function_economy_summary.R")
source("charts_tables_functions/fbs_function_line_chart.R")



### Load module scripts
source("Agri-Env/module_subsector_emissions.R")
source("Agri-Env/module_information.R")
source("Agri-Env/module_fertiliser_usage.R")
source("Structure/module_employees.R")
source("Structure/module_legal_responsibility.R")
source("Structure/module_farm_types.R")
source("Structure/module_occupiers.R")
source("Structure/module_land_use_summary.R")
source("Structure/module_owned_land.R")
source("Livestock/module_cattle.R")
source("Livestock/module_sheep.R")
source("Livestock/module_pigs.R")
source("Livestock/module_poultry.R")
source("Livestock/module_other_animals.R")
source("Crops/module_cereals.R")
source("Crops/module_oilseed.R")
source("Crops/module_potatoes.R")
source("Crops/module_beans.R")
source("Crops/module_stockfeeding.R")
source("Crops/module_human_vegetables.R")
source("Crops/module_fruit.R")
source("Home/module_home.R")
source("Economy/module_economy_summary.R")
source("Economy/FBS/module_fbs.R")
source("Economy/TIFF/module_tiff.R")
source("Livestock/module_animals_summary.R")
source("Agri-Env/module_soil_testing.R")
source("Agri-Env/module_manure.R")
source("Agri-Env/module_nitrogen_usage.R")
source("Crops/module_crops_summary.R")
source("Structure/module_structure_information.R")
source("Machinery/module_total_number_of_vehicles.R")
source("Machinery/module_ag_mach_farm_type.R")
source("Machinery/module_ag_mach_ownership.R")
source("Machinery/module_ag_mach_fuel.R")
source("Irrigation/module_irrigation_methods.R")
source("Irrigation/module_irrigation_drought_flood_protections.R")
source("Organic Farming/module_organic_summary.R")
source("Organic Farming/module_organic_land_area.R")
source("Organic Farming/module_organic_land_use.R")
source("Organic Farming/module_organic_livestock.R")
source("Organic Farming/module_organic_operators.R")
source("Organic Farming/module_regional_variation.R")


create_footer <- function() {
  div(
    class = "footer",
    span(style = "font-weight: bold;", "Content in development "),
    span("\n | Last updated: 11/12/2025"),
    img(src = "sg.png", alt = "SG Logo", style = "height: 30px; margin-left: 10px;")
  )
}


# Integrate the home module into the main UI
ui <- fluidPage(
  useShinyjs(),  # Initialize shinyjs
  theme = shinytheme("flatly"),
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
    includeHTML("utility/google-analytics.html"),
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
                       tabPanel("Peas and Beans", value = "beans_module", beansUI("beans_module")),
                       tabPanel("Stockfeeding", value = "stockfeeding_module", stockfeedingUI("stockfeeding_module")),
                       tabPanel("Vegetables", value = "human_vegetables_module", humanVegetablesUI("human_vegetables_module")),
                       tabPanel("Fruit", value = "fruit_module", fruitUI("fruit_module"))
            ),
            navbarMenu("Machinery",
                       tabPanel("Total number", value = "total_vehicles_module", totalnumberofvehiclesUI("total_vehicles_module")),
                       tabPanel("Farm type", value = "ag_mach_farm_type_module", agmachfarmtypeUI("ag_mach_farm_type_module")),
                       tabPanel("Ownership", value = "ag_mach_ownership_module", agmachownershipUI("ag_mach_ownership_module")),
                       tabPanel("Fuel", value = "ag_mach_fuel_module", agmachfuelUI("ag_mach_fuel_module"))
            ),
            navbarMenu("Irrigation",
                       tabPanel("Irrigation methods", value = "module_irrigation_methods", irrigationmethodsUI("module_irrigation_methods")),
                       tabPanel("Drought and flood protection", value = "module_irrigation_drought_flood_protection", irrigationfloodUI("module_irrigation_drought_flood_protection"))
                       ),                                                                                                                                        
            navbarMenu("Economy",
                       tabPanel("Summary", value = "economy_summary_module", economySummaryUI("economy_summary_module")),
                       tabPanel("Total income from farming", value = "tiff_module", tiffUI("tiff_module")),
                       tabPanel("Farm business income", value = "module_fbs", CostOutUI("module_fbs"))
            )
            # navbarMenu("Organic Farming",
            #            tabPanel("Summary", value = "module_organic_summary", organicsummaryUI("module_organic_summary")),
            #            tabPanel("Land use", value = "module_organic_land_use", organiclanduseUI("module_organic_land_use")),
            #            tabPanel("Land area", value = "module_organic_land_area", organiclandareaUI("module_organic_land_area")),
            #            tabPanel("Livestock", value = "module_organic_livestock", organiclivestockUI("module_organic_livestock")),
            #            tabPanel("Operators", value = "module_organic_operators", organicoperatorsUI("module_organic_operators")),
            #            tabPanel("Regional variation", value = "module_regional_variation", organicregionalvariationUI("module_regional_variation")))
            # )
          ),
          create_footer()
      )
  )
)

