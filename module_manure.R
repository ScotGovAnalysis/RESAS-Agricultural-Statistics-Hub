

# Load the data
load("module_2023.RData")

# Convert relevant columns to character before pivoting
manure_qty <- manure_qty %>%
  mutate(across(everything(), as.character))

# Transform the data, filtering out "All respondents"
manure_data_long <- manure_qty %>%
  filter(Region != "All respondents") %>%
  pivot_longer(cols = -Region, names_to = "variable", values_to = "value") %>%
  mutate(value = as.numeric(value)) %>%
  rename(region = Region)

# Extract the data for "All respondents"
national_data <- manure_qty %>%
  filter(Region == "All respondents") %>%
  select(-Region) %>%
  pivot_longer(cols = everything(), names_to = "variable", values_to = "value") %>%
  mutate(value = as.numeric(value)) # Ensure numeric values

# List of variables for radio buttons
variables_manure <- colnames(manure_qty)[-1]


# Helper function to format numbers with commas and appropriate decimal places
format_number <- function(number) {
  if (is.na(number)) {
    return("N/A")
  } else if (number %% 1 == 0) {
    format(number, big.mark = ",", scientific = FALSE)
  } else {
    format(round(number, 2), big.mark = ",", scientific = FALSE, nsmall = 2)
  }
}

valueBoxManureUI <- function(id, title, value, unit) {
  numeric_value <- as.numeric(value) # Ensure the value is numeric
  formatted_value <- format_number(numeric_value)
  
  box(
    class = "value-box",
    title = NULL,
    width = 12,
    solidHeader = TRUE,
    div(
      style = "display: flex; flex-direction: column; justify-content: center; align-items: center; height: 100%; padding: 5px;",  # Center content vertically and horizontally
      div(
        style = "font-size: 14px; font-weight: bold; margin-bottom: 15px; text-align: center;",  # Adjusted title style and added margin for vertical gap
        h5(class = "value-box-title", title)
      ),
      div(
        style = "text-align: center;",  # Center the value and unit together
        h3(HTML(formatted_value), style = "font-size: 24px; font-weight: bold; margin: 0;"),  # Main value centered
        div(style = "font-size: 16px; font-weight: normal; margin-top: 5px;", unit)  # Unit placed below the value, also centered
      )
    ),
    style = "border: 1px solid white; background-color: transparent; box-shadow: none; display: flex; align-items: center; justify-content: center;"  # Center the entire content of the box
  )
}



manureUI <- function(id) {
  ns <- NS(id)
  sidebarLayout(
    sidebarPanel(
      width = 3,
      uiOutput(ns("sidebar_ui"))
    ),
    mainPanel(
      width = 9,
      tabsetPanel(
        id = ns("tabs"),
        tabPanel("Summary",
                 fluidRow(
                   column(width = 4,
                          valueBoxManureUI(ns("total_manure"), "Total manure applied", 
                                           national_data$value[national_data$variable == "Manure"], "tonnes"),
                          p(style = "color: white;", "/"),
                          valueBoxManureUI(ns("total_holdings"), "Total holdings", 
                                           national_data$value[national_data$variable == "Holdings"], "holdings"),
                          p(style = "color: white;", "/"),
                          valueBoxManureUI(ns("total_application_rate"), "Total application rate", 
                                           national_data$value[national_data$variable == "Application rate"], "tonnes / hectare"),
                          p(style = "color: white;", "/"),
                          valueBoxManureUI(ns("average_mixed_sward"), "Average mixed sward per holding", 
                                           national_data$value[national_data$variable == "Average mixed sward area per holding"], "hectares"),
                          p(style = "color: white;", "/"),
                          valueBoxManureUI(ns("average_grassland"), "Average grassland area per holding", 
                                           national_data$value[national_data$variable == "Average grassland area per holding"], "hectares")
                   ),
                   column(width = 8,
                          mapRegionsUI(ns("map"))
                   )
                 ),
                 value = "map"
        ),
        tabPanel("Data Table", 
                 DTOutput(ns("data_table")), 
                 downloadButton(ns("downloadData"), "Download Data"), 
                 value = "data_table") ,
        footer = generateModuleTableFooter()  
  )
  )
  )
}

manureServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    output$sidebar_ui <- renderUI({
      req(input$tabs)
      if (input$tabs == "map") {
        radioButtons(ns("variable"), "Select Variable", choices = variables_manure)
      } else if (input$tabs == "data_table") {
        radioButtons(ns("data_source"), "Choose data to show:", choices = c("Map Data"))
      }
    })
    
    mapRegionsServer(
      id = "map",
      data = reactive({
        req(input$variable)
        manure_data_long %>%
          filter(variable == input$variable)
      }),
      unit = " ",
      footer = '<div style="font-size: 16px; font-weight: bold;"><a href="https://www.gov.scot/publications/results-from-the-scottish-agricultural-census-module-june-2023/" target="_blank">Source: Scottish Agricultural Census: Module June 2023</a></div>',
      variable = reactive(input$variable),
      title = paste("Manure quantity by region in Scotland in 2023"),
    )
    
    output$data_table <- renderDT({
      req(input$data_source)
      
      if (input$data_source == "Map Data") {
        datatable(
          manure_qty %>%
            mutate(across(where(is.numeric) & !contains("Year"), comma)),
          options = list(
            scrollX = TRUE,     # Enable horizontal scrolling
            pageLength = 20     # Show 20 entries by default
          )
        )
      }
    })
    
    output$downloadData <- downloadHandler(
      filename = function() {
        paste("map_data", Sys.Date(), ".csv", sep = "")
      },
      content = function(file) {
        write.csv(manure_qty, file, row.names = FALSE)
      }
    )
  })
}


# Test the module
content_demo <- function() {
  ui <- fluidPage(manureUI("manure_map_test"))
  server <- function(input, output, session) {
    manureServer("manure_map_test")
  }
  shinyApp(ui, server)
}

# Uncomment the line below to run the test
#content_demo()
