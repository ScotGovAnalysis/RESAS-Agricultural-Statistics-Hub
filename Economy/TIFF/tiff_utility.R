main_categories <- c(
  "Total_output_from_crops", 
  "Total_output_from_livestock",
  "Total_output_from_other_agricultural_activities", 
  "Total_output_from_non-agricultural_activities",
  "Gross_output", 
  "Total_input_from_feedstuffs",
  "Total_input_from_seeds", 
  "Total_input_from_fertilisers_and_lime", 
  "Total_input_from_farm_maintenance",
  "Total_input_from_miscellaneous_expenses", 
  "FISIM",
  "Total_input_from_non-agricultural_activities", 
  "Gross_input", 
  "Gross_value_added",
  "Total_consumption_of_fixed_capital",
  "Net_value_added",
  "Total_other_support",
  "Total_of_all_support_payments", 
  "Net_value_added_at_factor_cost",
  "Hired_labour", 
  "Interest,_rent_and_taxes",
  "Total_Costs", 
  "Total income from farming", 
  "Total income from farming, without support payments"
)


tiff_list <- c("Gross_output" = "Gross output",
               "Total_output_from_crops" = "Output from crops",
               "Total_output_from_livestock" = "Output from livestock",
               "Total_output_from_other_agricultural_activities" = "Output from other agricultural activities",
               "Total_output_from_non-agricultural_activities" = "Output from non-agricultural activities",
               "Total_Costs" = "Total costs",
               "Total_input_from_seeds" = "Costs of seed",
               "Total_input_from_feedstuffs" = "Costs of feed",
               "Total_input_from_fertilisers_and_lime" = "Costs of fertilisers and lime",
               "Total_input_from_farm_maintenance" = "Costs of farm maintenance",
               "Total_input_from_miscellaneous_expenses" = "Costs of miscellaneous expenses",
               "FISIM" = "Costs of FISIM (Financial intermediation services indirectly measured)",
               "Total_input_from_non-agricultural_activities" = "Costs of non-agricultural activities",
               "Gross_input" = "Gross input",
               "Gross_value_added" = "Gross value added",
               "Total_consumption_of_fixed_capital" = "Consumption of fixed capital",
               "Net_value_added" = "Net value added",
               "Total_of_all_support_payments" = "All support payments",
               "Total_other_support" = "Other support",
               "Net_value_added_at_factor_cost" = "Net value added (Factor cost)",
               "Hired_labour" = "Costs of hired labour",
               "Interest,_rent_and_taxes" = "Costs of interest, rent and taxes",
               "Total income from farming"= "Total income from farming",
               "Total income from farming, without support payments" = "Total income from farming, without support payments"
)

tiff_Outputs <- c("Gross output",
                  "Output from crops",
                  "Output from livestock",
                  "Output from other agricultural activities",
                  "Output from non-agricultural activities")

tiff_prices <- c("Current (nominal)",
                 "Real terms (Constant 2024)")

tiff_Costs <- c("Total costs",
                "Costs of seed",
                "Costs of feed",
                "Costs of fertilisers and lime",
                "Costs of farm maintenance",
                "Costs of miscellaneous expenses",
                "Costs of FISIM (Financial intermediation services indirectly measured)",
                "Costs of non-agricultural activities",
                "Gross input",
                "Gross value added",
                "Consumption of fixed capital",
                "Net value added",
                "All support payments",
                "Other support",
                "Net value added (Factor cost)",
                "Costs of hired labour",
                "Costs of interest, rent and taxes"
                )


tiff_Total <- c(
  "Total income from farming",
  "Total income from farming, without support payments"
)

all_tiff <- c(tiff_Costs, tiff_Outputs, tiff_Total)
#all_tiff

measure_lookup2 <- setNames(names(tiff_list), tiff_list)


