# File: module_timelapse_bar_chart.R

timelapseBarChartUI <- function(id) {
  ns <- NS(id)
  tagList(
    tags$style(HTML("
      .chart-container {
        position: relative;
      }
      .chart-controls {
        display: flex;
        align-items: center;
      }
      .chart-controls .form-group {
        margin-bottom: 0;
        margin-left: 10px;
      }
      .chart-controls .btn {
        margin-left: 10px;
      }
      .year-label {
        margin-right: 10px;
        font-weight: bold;
      }
    ")),
    div(class = "chart-container",
        fluidRow(
          column(8, htmlOutput(ns("title"))),
          column(4, div(class = "chart-controls",
                        div(class = "year-label", "Year:"),
                        sliderInput(ns("year"), NULL, min = 1998, max = 2023, value = 1998, step = 1, sep = "", ticks = TRUE, animate = animationOptions(interval = 1000, loop = FALSE), width = '200px'),
                        actionButton(ns("playPause"), "", icon = icon("play"), class = "btn btn-primary")
          ))
        ),
        highchartOutput(ns("chart"), height = "500px")
    ),
    htmlOutput(ns("footer")),
    div(
      class = "note",
      style = "margin-top: 20px; padding: 10px; border-top: 1px solid #ddd;",
      HTML(
        "<strong>Note:</strong><ul>
          <li>Press the play button at the top right of the screen to see the timelapse of the data. You can manually adjust the year by adjusting the slider.</li>
          <li>To remove a series from the chart, deselect the variable from the sidebar menu.</li>
          <li>You can see data values for a specific year by hovering your mouse over the line.</li>
        </ul>"
      )
    )
  )
}

timelapseBarChartServer <- function(id, chart_data, title, yAxisTitle, xAxisTitle, footer, x_col, y_col, unit = "") {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    max_value <- reactive({
      all_data <- chart_data()
      if (is.null(all_data) || nrow(all_data) == 0) return(0)
      max_val <- max(all_data[[y_col]], na.rm = TRUE)
      return(max_val)
    })
    
    getData <- function(year, nbr) {
      data <- chart_data()
      if (is.null(data) || nrow(data) == 0) return(data.frame())
      data %>%
        filter(!!sym(x_col) == year) %>%
        arrange(desc(!!sym(y_col))) %>%
        slice(1:nbr) %>%
        mutate(!!sym(y_col) := as.numeric(!!sym(y_col)))
    }
    
    getSubtitle <- function(year, data) {
      if (nrow(data) == 0) return("")
      total_value <- sum(data[[y_col]], na.rm = TRUE)
      paste0("<span style='font-size: 80px'>", year, "</span><br><span style='font-size: 22px'>Total: <b>", round(total_value, 2), "</b> ", unit, "</span>")
    }
    
    getDataList <- function(data, colors) {
      if (nrow(data) == 0) return(list())
      first_col_name <- names(data)[1]
      lapply(1:nrow(data), function(i) {
        list(
          name = data[[first_col_name]][i],
          y = data[[y_col]][i],
          color = colors[[data[[first_col_name]][i]]]
        )
      })
    }
    
    current_year <- reactiveVal(1998)
    nbr <- 20
    
    current_data <- reactive({
      getData(current_year(), nbr)
    })
    
    current_subtitle <- reactive({
      getSubtitle(current_year(), current_data())
    })
    
    current_colors <- reactive({
      assign_colors(chart_data(), preset_colors)
    })
    
    current_data_list <- reactive({
      getDataList(current_data(), current_colors())
    })
    
    updateYear <- function() {
      year <- current_year()
      if (year < 2022) {
        current_year(year + 1)
      } else {
        current_year(1998)
        session$sendCustomMessage(type = 'resetPlayButton', message = NULL)
      }
    }
    
    observe({
      if (!is.null(input$playPause) && input$playPause %% 2 == 1) {
        isolate({
          updateYear()
        })
        invalidateLater(1000, session)
      }
    })
    
    observe({
      updateSliderInput(session, "year", value = current_year())
    })
    
    output$title <- renderUI({
      HTML(paste0("<div style='font-size: 20px; font-weight: bold;'>", title, "</div>"))
    })
    
    output$footer <- renderUI({
      HTML(footer)
    })
    
    output$chart <- renderHighchart({
      data <- current_data()
      if (nrow(data) == 0) return(NULL)
      first_col_name <- names(data)[1]
      highchart() %>%
        hc_chart(type = "bar", zoomType = "xy", animation = list(duration = 1000)) %>%
        hc_xAxis(type = "category", categories = data[[first_col_name]]) %>%
        hc_yAxis(opposite = TRUE, tickPixelInterval = 150, title = list(text = yAxisTitle), max = max_value() + 1) %>%
        hc_plotOptions(series = list(
          animation = FALSE,
          groupPadding = 0,
          pointPadding = 0.1,
          borderWidth = 0,
          colorByPoint = FALSE,
          dataSorting = list(enabled = TRUE, matchByName = TRUE),
          dataLabels = list(enabled = TRUE, format = '{point.y:.2f}')
        )) %>%
        hc_series(list(
          name = as.character(current_year()),
          data = current_data_list()
        )) %>%
        hc_tooltip(
          useHTML = TRUE,
          headerFormat = "<b>{point.key}</b><br/>",
          pointFormatter = JS(sprintf("function() {
            var value = this.y;
            var formattedValue;
            if (value >= 1000) {
              formattedValue = value.toLocaleString(undefined, {minimumFractionDigits: 0, maximumFractionDigits: 0});
            } else {
              formattedValue = value.toLocaleString(undefined, {minimumFractionDigits: 1, maximumFractionDigits: 2});
            }
            return this.series.name + ': ' + formattedValue + ' %s';
          }", unit))
        ) %>%
        hc_legend(enabled = FALSE) %>%
        hc_responsive(rules = list(
          list(
            condition = list(maxWidth = 550),
            chartOptions = list(
              xAxis = list(visible = FALSE),
              plotOptions = list(
                series = list(
                  dataLabels = list(
                    list(enabled = TRUE, y = 8),
                    list(enabled = TRUE, format = '{point.name}', y = -8, style = list(fontWeight = 'normal', opacity = 0.7))
                  )
                )
              )
            )
          )
        ))
    })
    
    observeEvent(input$year, {
      current_year(input$year)
    })
  })
}
