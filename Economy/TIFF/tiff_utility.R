tiff_Outputs <- c(
  "Gross Output" = "Gross_output",
  "Crops output" = "Total_output_from_crops",
  "Livestock output" = "Total_output_from_livestock",
  "Other Agricultural Output" = "Total_output_from_other_agricultural_activities",
  "Non-Agricultural Output" = "Total_output_from_non_agricultural_activities"
)

tiff_prices <- c("Current (nominal)",
                 "Real terms (Constant 2024)")

tiff_Inputs <- c(
  "Total Costs" = "Total_Costs",
  "Seeds input" = "Total_input_from_seeds",
  "Feed input" = "Total_input_from_feedstuffs",
  "Fertilisers and Lime input" = "Total_input_from_fertilisers_and_lime",
  "Farm Maintenance input" = "Total_input_from_farm_maintenance",
  "Miscellaneous Expenses input" = "Total_input_from_miscellaneous_expenses",
  "FISIM" = "FISIM",
  "Non-Agricultural input" = "Total_input_from_non_agricultural_activities",
  "Gross input" = "Gross_input",
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
  "Total income from farming" = "Total income from farming",
  "Total income from farming, without support payments" = "Total income from farming, without support payments"
)

all_tiff <- c(tiff_Inputs, tiff_Outputs, tiff_Total)
all_tiff


