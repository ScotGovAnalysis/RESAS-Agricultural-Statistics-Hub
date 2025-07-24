current_year <- "2023-24"
prev_year <- "2022-23"

farm_types <- c("All farms",
                "Cereals",
                "General cropping", 
                "Dairy",
                "LFA sheep", 
                "LFA cattle",
                "LFA cattle and sheep", 
                "Lowland cattle and sheep",
                "Mixed")

# categories

#totals
out_totals <- c("Output from agriculture", # outputs
                "Output from agri-environment activities and other payments",
                "Output from contracting",
                "Output from diversification out of agriculture",
                "Payment schemes in total",
                "Other payment schemes in total")

cost_totals <- c(
  "Agricultural costs",# costs
  "Costs of contracting",
  "Costs of agri-environment activities and other payments",
  "Costs of diversification out of agriculture",
  "Costs of payment schemes")

out_sub_totals <- c("Crop output",
                    "Livestock output",
                    "Support  payments to agriculture (severe weather payments)",
                    "Miscellaneous output (including agricultural work done on other farms)",
                    "Agri-environmental schemes",
                    "Project based schemes",
                    "Other grants and support payments",
                    "Output from contracting",
                    "Food processing and retailing",
                    "Tourism",
                    "Recreation",
                    "Rental income",
                    "Other diversified output",
                    "Basic Payment Scheme",
                    "Scottish Suckler Beef Support Scheme",
                    "Scottish Upland Sheep Scheme",
                    "PILLAR 2 Payments",
                    "ESA Grants",
                    "Support Advisory",
                    "Other miscellaneous grants")


cost_sub_totals <- c(
  "Variable agricultural costs", #costs 
  "Fixed agricultural costs",
  "Fixed contract costs",
  "Variable contract costs",
  "Variable agri-environment costs",
  "Fixed agri-environment costs",
  "Crop specific costs",
  "Livestock specific costs",
  "All feed and fodder",
  "Casual labour",
  "Miscellaneous variable costs (including for work done on other farms)",
  "Regular labour",
  "Machinery costs",
  "General farming costs",
  "Land and property costs",
  "Variable diversification costs",
  "Fixed diversification costs",
  "Variable payment schemes costs",
  "Fixed payment schemes costs"
  
)