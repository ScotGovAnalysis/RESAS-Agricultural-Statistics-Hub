#module_summary.R

library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(highcharter)

# Function to create a small line plot for the value boxes
small_line_plot <- function(data, color = "#002d54") {
  # Determine if Year is numeric or character
  is_numeric_year <- is.numeric(data$Year)
  
  # Extract first and last year
  year_range <- range(data$Year)
  
  # Build the plot
  p <- ggplot(data, aes(x = Year, y = Value, group = 1)) +
    geom_line(color = color) +
    theme_void() +
    theme(
      axis.text.x = element_text(size = 10, color = "#333"),
      plot.background = element_rect(fill = "transparent", color = NA)
    )
  
  # Apply appropriate x-axis scale
  if (is_numeric_year) {
    p <- p + scale_x_continuous(breaks = year_range)
  } else {
    # Ensure Year is an ordered factor
    data$Year <- factor(data$Year, levels = unique(data$Year), ordered = TRUE)
    p <- p + scale_x_discrete(breaks = as.character(year_range))
  }
  
  return(p)
}

# Function to create an arrow for Year on Year change
create_yoy_arrow <- function(change) {
  if (is.na(change) || is.nan(change)) {
    icon("minus", style = "color: grey;")
  } else if (change > 0) {
    icon("arrow-up", style = "color: #2b9c93;")
  } else {
    icon("arrow-down", style = "color: #002d54;")
  }
}

# UI Module for Value Box
valueBoxEconomyUI <- function(id) {
  ns <- NS(id)
  uiOutput(ns("valueBox"))
  
}

# Server Module for Value Box
valueBoxEconomyServer <- function(id, data, category, current_year, comparison_year, unit) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Filter data for current and comparison years
    reactive_data <- reactive({
      req(data())
      data() %>% filter(Year %in% c(current_year(), comparison_year()))
    })
  

output$valueBox <- renderUI({
  req(current_year(), comparison_year())
  df <- reactive_data()
  
  if (nrow(df) == 0) {
    return(div("No data available for selected years"))
  }
  

  current_value <- df %>%
    filter(Year == current_year()) %>%
    #summarise(Value = sum(Value, na.rm = TRUE)) %>%
    pull(Value)

  comparison_value <- df %>%
    filter(Year == comparison_year()) %>%
    #summarise(Value = sum(Value, na.rm = TRUE)) %>%
    pull(Value)

  yoy_change <- if (comparison_value == 0 || is.na(comparison_value)) NA
  else ((current_value - comparison_value) / comparison_value) * 100

  formatted_value <- paste0("£", format(current_value, big.mark = ",", scientific = FALSE))
  box(
    class = "value-box",
    title = NULL,
    width = 12,
    solidHeader = TRUE,
    div(
      style = "display: flex; flex-direction: column; justify-content: space-between; height: 100%; padding: 5px;",
      div(
        style = "flex: 1; margin-bottom: 5px;",
        h5(class = "value-box-title", paste(category, "in", current_year())),
        div(
          style = "display: flex; align-items: baseline; margin-bottom: 5px;",
          h3(HTML(formatted_value), style = "margin: 0;"),
          span(class = "value-box-units", unit)
        ),
        div(
          style = "display: flex; align-items: center; margin-bottom: 5px;",
          create_yoy_arrow(yoy_change),
          span(
            class = "value-box-yoy",
            ifelse(
              is.na(yoy_change),
              "Comparison not available",
              sprintf(
                "%+.2f%% %s vs. %s",
                yoy_change,
                as.character(current_year()),
                as.character(comparison_year()))
              ),
            style = ifelse(
              yoy_change > 0,
              "color: #2b9c93; margin-left: 5px;",
              "color: #002d54; margin-left: 5px;"
            )
          )
        )
      ),
      div(
        style = "margin-top: 10px;",
        plotOutput(ns("sparkline"), height = "30px", width = "100%")
      )
    )
  )
})
  

    output$sparkline <- renderPlot({
      small_line_plot(data(), "#28a745")
    })
    
    
    observe({
      print(head(data()))
    })
    
  
    
  })
}





