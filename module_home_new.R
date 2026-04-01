homeUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    
    # Welcome header
    fluidRow(
      box(
        width = 12,
        title = NULL,
        status = "primary",
        solidHeader = TRUE,
        h3("Welcome to the RESAS Agricultural Statistics Hub"),
        p("This hub provides access to a variety of agricultural statistics and data visualisations developed by RESAS (Scottish Government)."),
        p(HTML("This content is still in development. We would be grateful if you could fill in our 
               <a href='https://forms.office.com/e/Y9Eixgf4c1' target='_u can also email us at mailto:agric.stats@gov.scotagric.stats@gov.scot</a>."))
      )
    ),
    
    # Dashboard tiles
    fluidRow(
      box(width = 3, height = 150, status = "info", solidHeader = TRUE,
          actionButton(ns("go_structure"), "Structure", icon = icon("tree"), class = "dashboard-tile-btn")),
      box(width = 3, height = 150, status = "success", solidHeader = TRUE,
          actionButton(ns("go_env"), "Agri‑Environment", icon = icon("leaf"), class = "dashboard-tile-btn")),
      box(width = 3, height = 150, status = "warning", solidHeader = TRUE,
          actionButton(ns("go_livestock"), "Livestock", icon = icon("cow"), class = "dashboard-tile-btn")),
      box(width = 3, height = 150, status = "danger", solidHeader = TRUE,
          actionButton(ns("go_crops"), "Crops", icon = icon("seedling"), class = "dashboard-tile-btn"))
    ),
    
    fluidRow(
      box(width = 3, height = 150, status = "primary", solidHeader = TRUE,
          actionButton(ns("go_machinery"), "Machinery", icon = icon("tractor"), class = "dashboard-tile-btn")),
      box(width = 3, height = 150, status = "info", solidHeader = TRUE,
          actionButton(ns("go_irrigation"), "Irrigation", icon = icon("tint"), class = "dashboard-tile-btn")),
      box(width = 3, height = 150, status = "success", solidHeader = TRUE,
          actionButton(ns("go_economy"), "Economy", icon = icon("chart-line"), class = "dashboard-tile-btn"))
    ),
    
    # Short instructions
    fluidRow(
      box(
        width = 12,
        title = "How to use the site",
        status = "primary",
        solidHeader = TRUE,
        p("Each section contains interactive charts, maps, and tables. Tips:"),
        tags$ul(
          tags$li("Hover on charts for detail; drag to zoom."),
          tags$li("Use dropdowns, checkboxes, or sliders to filter data."),
          tags$li("Sort and search tables using the built‑in controls."),
          tags$li("Hover and zoom on maps to explore regions.")
        )
      )
    )
  )
}

homeServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    # Map each tile to the sidebar tabName
    observeEvent(input$go_structure,  updateTabItems(session, "tabs", "land_use"))
    observeEvent(input$go_env,        updateTabItems(session, "tabs", "subsector"))
    observeEvent(input$go_livestock,  updateTabItems(session, "tabs", "animals_summary_module"))
    observeEvent(input$go_crops,      updateTabItems(session, "tabs", "crops_summary_module"))
    observeEvent(input$go_machinery,  updateTabItems(session, "tabs", "total_vehicles_module"))
    observeEvent(input$go_irrigation, updateTabItems(session, "tabs", "module_irrigation_methods"))
    observeEvent(input$go_economy,    updateTabItems(session, "tabs", "economy_summary_module"))
    
  })
}