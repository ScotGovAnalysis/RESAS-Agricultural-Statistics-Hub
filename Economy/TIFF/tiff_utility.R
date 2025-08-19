tiff_list <- c("Gross_output" = "Gross Output",
               "Total_output_from_crops" = "Output from Crops",
               "Total_output_from_livestock" = "Output from Livestock",
               "Total_output_from_other_agricultural_activities" = "Output from Other Agricultural activities",
               "Total_output_from_non-agricultural_activities" = "Output from Non-Agricultural activities",
               "Total_Costs" = "Total Costs",
               "Total_input_from_seeds" = "Costs of Seed",
               "Total_input_from_feedstuffs" = "Costs of Feed",
               "Total_input_from_fertilisers_and_lime" = "Costs of Fertilisers and Lime",
               "Total_input_from_farm_maintenance" = "Costs of Farm Maintenance",
               "Total_input_from_miscellaneous_expenses" = "Costs of Miscellaneous Expenses",
               "FISIM" = "Costs of FISIM (Financial Intermediation Services Indirectly Measured)",
               "Total_input_from_non-agricultural_activities" = "Costs of Non-Agricultural activities",
               "Gross_input" = "Gross input",
               "Gross_value_added" = "Gross Value Added",
               "Total_consumption_of_fixed_capital" = "Consumption of Fixed Capital",
               "Net_value_added" = "Net Value Added",
               "Total_of_all_support_payments" = "All Support Payments",
               "Total_other_support" = "Other Support",
               "Net_value_added_at_factor_cost" = "Net Value Added (Factor Cost)",
               "Hired_labour" = "Costs of Hired Labour",
               "Interest,_rent_and_taxes" = "Costs of Interest, Rent and Taxes"
)

tiff_Outputs <- c("Gross Output",
                  "Output from Crops",
                  "Output from Livestock",
                  "Output from Other Agricultural activities",
                  "Output from Non-Agricultural activities")

tiff_prices <- c("Current (nominal)",
                 "Real terms (Constant 2024)")

tiff_Costs <- c("Total Costs",
                "Costs of Seed",
                "Costs of Feed",
                "Costs of Fertilisers and Lime",
                "Costs of Farm Maintenance",
                "Costs of Miscellaneous Expenses",
                "Costs of FISIM (Financial Intermediation Services Indirectly Measured)",
                "Costs of Non-Agricultural activities",
                "Gross input",
                "Gross Value Added",
                "Consumption of Fixed Capital",
                "Net Value Added",
                "All Support Payments",
                "Other Support",
                "Net Value Added (Factor Cost)",
                "Costs of Hired Labour",
                "Costs of Interest, Rent and Taxes"
                )


tiff_Total <- c(
  "Total income from farming" = "Total income from farming",
  "Total income from farming, without support payments" = "Total income from farming, without support payments"
)

all_tiff <- c(tiff_Costs, tiff_Outputs, tiff_Total)
all_tiff

measure_lookup2 <- setNames(names(tiff_list), tiff_list)


