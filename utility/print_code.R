# Define the paths to your module files
code_files <- c(
  "app.R",
  "data.R",
  "hc_theme.R",
  "home.R",
  "module_animals_summary.R",
  "module_area_chart.R",
  "module_bar_chart.R",
  "module_beans.R",
  "module_breakdown_chart.R",
  "module_cattle.R",
  "module_cereals.R",
  "module_crops_summary.R",
  "module_data_functions.R",
  "module_data_table.R",
  "module_double_bar_chart.R",
  "module_economy_summary.R",
  "module_employees.R",
  "module_farm_types.R",
  "module_fertiliser_usage.R",
  "module_fruit.R",
  "module_gas_emissions.R",
  "module_human_vegetables.R",
  "module_information.R",
  "module_land_use_summary.R",
  "module_legal_responsibility.R",
  "module_line_chart.R",
  "module_manure.R",
  "module_map.R",
  "module_nitrogen_usage.R",
  "module_occupiers.R",
  "module_oilseed.R",
  "module_other_animals.R",
  "module_owned_land.R",
  "module_percentage_bar_chart.R",
  "module_pigs.R",
  "module_potatoes.R",
  "module_poultry.R",
  "module_regions_map.R",
  "module_sheep.R",
  "module_soil_testing.R",
  "module_stockfeeding.R",
  "module_subsector_emissions.R",
  "module_summary.R",
  "module_timelapse_bar_chart.R",
  "module_total_emissions.R",
  "options.R",
  "server.R",
  "test.R",
  "ui.R",
  "utils.R"
  
  
  
)

# Function to read and concatenate the contents of the files
combine_code_files <- function(files) {
  code <- sapply(files, function(file) {
    file_content <- tryCatch({
      paste(readLines(file), collapse = "\n")
    }, error = function(e) {
      message(paste("Error reading file:", file))
      return("")
    })
    paste("## File:", file, "\n", file_content)
  }, USE.NAMES = FALSE)
  paste(code, collapse = "\n\n")
}

# Combine the contents of the specified files
combined_code <- combine_code_files(code_files)

# Specify the output file
output_file <- "combined_code.txt"

# Write the combined contents to the output file
writeLines(combined_code, con = output_file)

# Notify the user
cat("The combined code has been written to", output_file)
