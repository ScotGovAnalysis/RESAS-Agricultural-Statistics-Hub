subsectorEmissionsUI <- function(id) {
  ns <- NS(id)
  tagList(
    sidebarLayout(
      sidebarPanel(
        width = 3,
        conditionalPanel(
          condition = "input.tabs != 'Summary_Page'",
          ns = ns,
          uiOutput(ns("variable_select")),
        ),
        conditionalPanel(
          condition = "input.tabs == 'Summary_Page'",
          ns = ns,
          div("Adjust the sliders to compare data from different years.", 
              style = "font-size: 14px; font-weight: bold; margin-bottom: 10px;"),
          sliderInput(ns("summary_current_year_subsector"), "Year of interest", min = 1990, max = emissions_year, value = emissions_year, step = 1, sep = ""),
          sliderInput(ns("summary_comparison_year_subsector"), "Comparison year", min = 1990, max = emissions_year, value = emissions_year - 1, step = 1, sep = "")
        )
      ),
      mainPanel(
        id = ns("mainpanel"),
        width = 9,
        tabsetPanel(
          id = ns("tabs"),
          tabPanel("Summary Page",
                   value = "Summary_Page",
                   fluidRow(
                     column(width = 12, div(class = "header-text", "Breakdown of Scottish agricultural emissions:"))
                   ),
                   fluidRow(
                     column(width = 4, chartUI(ns("industryPieChart_subsector")), style = "padding-right: 0; padding-left: 0;"),
                     column(width = 4, chartUI(ns("industryBarChart_subsector")), style = "padding-right: 0; padding-left: 0;"),
                     column(width = 4, chartUI(ns("industryLineChart_total")), style = "padding-right: 0; padding-left: 0;")
                   ),
                   fluidRow(
                     column(width = 12, div(class = "header-text", style = "padding-top: 20px;" ,"Emissions by subsector:"))
                   ),
                   fluidRow(
                     column(width = 4, valueBoxUI(ns("totalIndustry1_subsector")), style = "padding-right: 0; padding-left: 0;"),
                     column(width = 4, valueBoxUI(ns("totalIndustry2_subsector")), style = "padding-right: 0; padding-left: 0;"),
                     column(width = 4, valueBoxUI(ns("totalIndustry3_subsector")), style = "padding-right: 0; padding-left: 0;")
                   ),
                   fluidRow(
                     column(width = 4, valueBoxUI(ns("totalValue_subsector")), style = "padding-right: 0; padding-left: 0;"),
                     column(width = 4, valueBoxUI(ns("totalIndustry4_subsector")), style = "padding-right: 0; padding-left: 0;"),
                     column(width = 4, valueBoxUI(ns("totalIndustry5_subsector")), style = "padding-right: 0; padding-left: 0;")
                   ),
                   fluidRow(
                     column(width = 12, div(class = "header-text", style = "margin-top: 20px;" , "Scottish agricultural emissions by gas type:"))
                   ),
                   fluidRow(
                     column(width = 4, chartUI(ns("industryPieChart_gas"))),
                     column(width = 4, chartUI(ns("industryBarChart_gas")))
                   ),
                   fluidRow(
                     column(width = 12, 
                            div(
                              class = "footer-text", 
                              style = "font-size: 16px; font-weight: bold; text-align: left; margin-top: 5px;",
                              HTML(
                                '<a href="https://www.gov.scot/publications/scottish-agriculture-greenhouse-gas-emissions-and-nitrogen-use-2022-23/" target="_blank">Source: Scottish agriculture greenhouse gas emissions and nitrogen use 2022-23</a>, analysis based on results of the <a href="https://www.gov.scot/publications/scottish-greenhouse-gas-statistics-2022/" target="_blank">Scottish Greenhouse Gas Statistics 2022</a>.'
                              )
                            )
                     )
                   ),
                   fluidRow(
                     column(width = 12, 
                            div(
                              class = "note",
                              style = "margin-top: 20px; padding: 10px; border-top: 1px solid #ddd;",
                              HTML(
                                "<strong>Note:</strong><ul>
                                  <li>You can see data values by hovering your mouse over the content on charts.</li>
                                </ul>"
                              )
                            )
                     )
                   )
          ),
          tabPanel("Timelapse", timelapseBarChartUI(ns("timelapse_bar")), value = "Timelapse"),
          tabPanel("Sources of Agricultural Emissions", breakdownChartUI(ns("breakdown")), value = "Breakdown"),
          tabPanel("Time Series", lineChartUI(ns("line")), value = "Line_Chart"),
          tabPanel("Area Chart", areaChartUI(ns("area")), value = "Area_Chart"),
          tabPanel("Data Table",
                   DTOutput(ns("data_table")),
                   downloadButton(ns("downloadData"), "Download Data"),
                   generateEmissionsTableFooter(),
                   value = "Data_Table")
        )
      )
    )
  )
}




subsectorEmissionsServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Ensure full_data_subsector is defined correctly
    full_data_subsector <- reactive({ subsector_total })
        full_data_total <- reactive({ national_total })
    full_data_gas <- reactive({ agri_gas })
    
    # Reactive data for the line chart (Industry Emissions Over Time)
    line_chart_data <- reactive({
      data <- national_total
      #data <- data %>% filter(Industry %in% c("Agriculture", "Total"))
      data
    })
    
    # Reactive data for the gas charts (Pie and Bar charts)
    gas_chart_data <- reactive({
      data <- agri_gas

      data
    })
    
    # Reactive data for the subsector module
    chart_data <- reactive({
      data <- subsector_total
      if (!is.null(input$variables)) {
        data <- data %>%
          filter(Subsector %in% input$variables)
      }
      data
    })
    
    output$variable_select <- renderUI({
      choices <- unique(subsector_total$Subsector)
      selected <- setdiff(choices, "Total")
      checkboxGroupInput(ns("variables"), "Choose variables to add to chart", choices = choices, selected = selected)
    })
    
    observeEvent(input$select_all_button, {
      updateCheckboxGroupInput(session, ns("variables"), selected = setdiff(unique(subsector_total$Subsector), "Total"))
    })
    
    observeEvent(input$deselect_all_button, {
      updateCheckboxGroupInput(session, ns("variables"), selected = character(0))
    })
    
    # Load and render the subsector charts
    areaChartServer(
      id = "area",
      chart_data = chart_data,
      title = "Agricultural emissions by subsector over time",
      yAxisTitle = "Emissions (MtCO₂e)",
      xAxisTitle = "Year",
      unit = "MtCO₂e",
      footer = emissions_footer,
      x_col = "Year",
      y_col = "Value"
    )
    
    timelapseBarChartServer(
      id = "timelapse_bar",
      chart_data = chart_data,
      title = "Agricultural greenhouse gas emissions timelapse",
      yAxisTitle = "Emissions (MtCO₂e)",
      xAxisTitle = "Year",
      unit = "MtCO₂e",
      footer = emissions_footer,
      x_col = "Year",
      y_col = "Value"
    )
    
    lineChartServer(
      id = "line",
      chart_data = chart_data,
      title = "Agricultural greenhouse gas emissions by subsector in Scotland",
      yAxisTitle = "Emissions (MtCO₂e)",
      xAxisTitle = "Year",
      unit = "MtCO₂e",
      footer = emissions_footer,  
      x_col = "Year",
      y_col = "Value"
    )
    
    breakdownChartServer(
      id = "breakdown",
      chart_data = reactive({ subsector_source }),  # Assuming subsector_source is your data
      title = "Sources of Scottish agricultural emissions by subsector and type, 2022-23",
      yAxisTitle = "Emissions (MtCO₂e)",
      xAxisTitle = "Subsector",
      unit = "MtCO₂e",
      footer = emissions_footer
      )
    
    
    
    render_data_table(
      table_id = "data_table",
      chart_data = chart_data,
      output = output
    )
    
    handle_data_download(
      download_id = "downloadData",
      chart_type = "Subsector",
      chart_data = chart_data,
      input = input,
      output = output,
      year_input = "year"
    )
    
    #  Module for Subsector Emissions
    current_year <- reactive({ input$summary_current_year_subsector })
    comparison_year <- reactive({ input$summary_comparison_year_subsector })
    
    first_col_name <- "Subsector"
    
    valueBoxServer(
      id = "totalIndustry1_subsector",
      data = full_data_subsector,
      category = first_col_name,
      industry = get_industry(1, full_data_subsector, current_year, first_col_name),
      current_year = current_year,
      comparison_year = comparison_year,
      unit = "MtCO₂e"
    )
    valueBoxServer(
      id = "totalIndustry2_subsector",
      data = full_data_subsector,
      category = first_col_name,
      industry = get_industry(2, full_data_subsector, current_year, first_col_name),
      current_year = current_year,
      comparison_year = comparison_year,
      unit = "MtCO₂e"
    )
    valueBoxServer(
      id = "totalIndustry3_subsector",
      data = full_data_subsector,
      category = first_col_name,
      industry = get_industry(3, full_data_subsector, current_year, first_col_name),
      current_year = current_year,
      comparison_year = comparison_year,
      unit = "MtCO₂e"
    )
    valueBoxServer(
      id = "totalIndustry4_subsector",
      data = full_data_subsector,
      category = first_col_name,
      industry = get_industry(4, full_data_subsector, current_year, first_col_name),
      current_year = current_year,
      comparison_year = comparison_year,
      unit = "MtCO₂e"
    )
    valueBoxServer(
      id = "totalIndustry5_subsector",
      data = full_data_subsector,
      category = first_col_name,
      industry = get_industry(5, full_data_subsector, current_year, first_col_name),
      current_year = current_year,
      comparison_year = comparison_year,
      unit = "MtCO₂e"
    )
    valueBoxServer(
      id = "totalValue_subsector",
      data = full_data_subsector,
      category = first_col_name,
      industry = reactive("Total"),
      current_year = current_year,
      comparison_year = comparison_year,
      unit = "MtCO₂e"
    )
    summaryPieChartServer(
      id = "industryPieChart_subsector",
      data = full_data_subsector,
      title = "Agricultural emissions by subsector",
      current_year = current_year,
      category = "Subsector",
      unit = "MtCO₂e"
    )
    
    summaryBarChartServer(
      id = "industryBarChart_subsector",
      data = full_data_subsector,
      title = "Agricultural emissions by subsector",
      current_year = current_year,
      comparison_year = comparison_year,
      category = "Subsector",
      unit = "MtCO₂e"
    )
    
    summaryLineChartServer(
      id = "industryLineChart_total",
      data = line_chart_data,
      title = "Agricultural emissions and total Scottish emissions",
      unit = "MtCO₂e",
      x_col = "Year",
      y_col = "Value"
    )

    
    summaryPieChartServer(
      id = "industryPieChart_gas",
      data = gas_chart_data,
      title = "Breakdown of gas type",
      current_year = current_year,
      category = "Gas",
      unit = "MtCO₂e"
    )
    
    summaryBarChartServer(
      id = "industryBarChart_gas",
      data = gas_chart_data,
      title = "Emissions by gas type",
      current_year = current_year,
      comparison_year = comparison_year,
      category = "Gas",
      unit = "MtCO₂e"
    )
  })
}


subsector_demo <- function() {
  ui <- fluidPage(subsectorEmissionsUI("subsector_test"))
  server <- function(input, output, session) {
    subsectorEmissionsServer("subsector_test")
  }
  shinyApp(ui, server)
}

subsector_demo()
