
#########################################################################
### 
###
### Emissions Data Processing
####import and save GHG data
###
##########################################################################

#agri_gas = Gas Breakdown of Emissions from Scottish agriculture greenhouse gas emissions and nitrogen use
#subsector_total = Agricultural emissions broken down by subsector from Scottish agriculture greenhouse gas emissions and nitrogen use
#subsector_source = Breakdown of subsectors by source from Scottish agriculture greenhouse gas emissions and nitrogen use
#national_total = Yearly breakdown of Scottish Emissions from Scottish Greenhouse Gas Emissions publication


library(here)

emissions_year <- 2024
ghg_file_path <- paste0("//s0196a/ADM-Rural and Environmental Science-Farming Statistics/Agriculture/Source/GHG Inventory/GHGI ", emissions_year, "/ghg_data.rda")
ghg_data <- load(ghg_file_path)

#format for module_Subsector
national_total <- national_total |> rename(Industry = `TES Sector`)
subsector_total <- subsector_total |> select(Subsector, Year, Value)

# add agriculture total to subsector_total dataset
agri_total <- national_total |> filter(Industry == "Agriculture") |> 
  mutate(Subsector =  "Total") |> 
  select(Subsector, Year, Value)

subsector_total <- rbind(subsector_total, agri_total)

#arrange highest to lowest for bar chart

subs_tot <- colSums(subsector_source[,-1]) |>   
  sort(decreasing = T)
subsector_source <- subsector_source |>
  select(Source, names(subs_tot))
# save
save(national_total, subsector_total, subsector_source, agri_total, agri_gas, file = "Data/ghg_data.RData")

