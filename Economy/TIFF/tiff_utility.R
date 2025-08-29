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


tiff_list <- c("Total_output_from_crops" = "1. Output from crops",
               "Total_output_from_livestock" = "2. Output from livestock",
               "Total_output_from_other_agricultural_activities" = "3. Output from other agricultural activities",
               "Total_output_from_non-agricultural_activities" = "4. Output from non-agricultural activities",
               "Gross_output" = "5. Gross output (1+2+3+4)",
               "Total_input_from_seeds" = "6. Costs of seed",
               "Total_input_from_feedstuffs" = "7. Costs of feed",
               "Total_input_from_fertilisers_and_lime" = "8. Costs of fertilisers and lime",
               "Total_input_from_farm_maintenance" = "9. Costs of farm maintenance",
               "Total_input_from_miscellaneous_expenses" = "10. Costs of miscellaneous expenses",
               "FISIM" = "11. Costs of FISIM (Financial intermediation services indirectly measured)",
               "Total_input_from_non-agricultural_activities" = "12. Costs of non-agricultural activities",
               "Gross_input" = "13. Gross input (6+7+8+9+10+11+12)",
               "Gross_value_added" = "14. Gross value added (5-13)",
               "Total_consumption_of_fixed_capital" = "15. Consumption of fixed capital",
               "Net_value_added" = "16. Net value added (at basic price)(14-15)",
               "Total_of_all_support_payments" = "17. All support payments",
               "Total_other_support" = "18. Other support",
               "Net_value_added_at_factor_cost" = "19. Net value added (Factor cost)(16+18)",
               "Hired_labour" = "20. Costs of hired labour",
               "Interest,_rent_and_taxes" = "21. Costs of interest, rent and taxes",
               "Total_Costs" = "22. Total costs (13+15+20+21)",
               "Total income from farming"= "23. Total income from farming (19-20-21)",
               "Total income from farming, without support payments" = "24. Total income from farming, without support payments (23-17)"
)

tiff_Outputs <- c("1. Output from crops",
                  "2. Output from livestock",
                  "3. Output from other agricultural activities",
                  "4. Output from non-agricultural activities",
                  "5. Gross output (1+2+3+4)")

tiff_prices <- c("Current (nominal)",
                 "Real (Constant 2024)")

tiff_Costs <- c("6. Costs of seed",
                "7. Costs of feed",
                "8. Costs of fertilisers and lime",
                "9. Costs of farm maintenance",
                "10. Costs of miscellaneous expenses",
                "11. Costs of FISIM (Financial intermediation services indirectly measured)",
                "12. Costs of non-agricultural activities",
                "13. Gross input (6+7+8+9+10+11+12)",
                "14. Gross value added (5-13)",
                "15. Consumption of fixed capital",
                "16. Net value added (at basic price)(14-15)",
                "17. All support payments",
                "18. Other support",
                "19. Net value added (Factor cost)(16+18)",
                "20. Costs of hired labour",
                "21. Costs of interest, rent and taxes",
                "22. Total costs (13+15+20+21)"
                )


tiff_Total <- c(
  "23. Total income from farming (19-20-21)",
  "24. Total income from farming, without support payments (23-17)"
)

all_tiff <- c(tiff_Costs, tiff_Outputs, tiff_Total)
#all_tiff

measure_lookup2 <- setNames(names(tiff_list), tiff_list)


tiff_year <- max(main_tiff_data_long$Year) #Current TIFF year
tiff_year_min <- min(main_tiff_data_long$Year)

