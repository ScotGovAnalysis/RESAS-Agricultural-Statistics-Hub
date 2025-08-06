tiff_year <- max(main_tiff_data_long$Year) #Current TIFF year
tiff_year_min <- min(main_tiff_data_long$Year)

tiff_Outputs <- c(
  "Gross Output" = "Gross_output",
  "Crops" = "Total_output_from_crops",
  "Livestock " = "Total_output_from_livestock",
  "Other Agricultural Output" = "Total_output_from_other_agricultural_activities",
  "Non-Agricultural Output" = "Total_output_from_non_agricultural_activities"
)

tiff_Inputs <- c(
  "Total Costs" = "Total_Costs",
  "Seeds" = "Total_input_from_seeds",
  "Feed" = "Total_input_from_feedstuffs",
  "Fertilisers and Lime" = "Total_input_from_fertilisers_and_lime",
  "Farm Maintenance" = "Total_input_from_farm_maintenance",
  "Miscellaneous Expenses" = "Total_input_from_miscellaneous_expenses",
  "FISIM" = "FISIM",
  "Non-Agricultural Input" = "Total_input_from_non_agricultural_activities",
  "Gross Input" = "Gross_input",
  "Gross Value Added" = "Gross_value_added",
  "Consumption of Fixed Capital" = "Total_consumption_of_fixed_capital",
  "Net Value Added" = "Net_value_added",
  "All Support Payments" = "Total_of_all_support_payments",
  "Other Support" = "Total_other_support",
  "Net Value Added (Factor Cost)" = "Net_value_added_at_factor_cost",
  "Hired Labour" = "Hired_labour",
  "Interest, Rent and Taxes" = "Interest,_rent_and_taxes"
)

tiff_Total <- c(
  "Total Income from Farming" = "Total income from farming",
  "Total income from farming, without support payments" = "Total income from farming, without support payments"
)
