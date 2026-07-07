
library(tidyverse)
library(here)
library(plotly)


nitrogenline_ChartUI <- function(id, n_type = NULL) {
  ns <- NS(id)
  
  base_note <- HTML(paste0("<p><strong>Nitrogen balance (kg N surplus/ha)</strong> is the difference between total nitrogen input and output.</p>
                    <p><strong>Nitrogen use efficiency (% N output / N input)</strong> is the ratio of nitrogen outputs to inputs.</p>
                    <p>Line shows the median and shaded area shows the 95% confidence interval.</p>",
                           ' <p> <div style="font-size: 16px; font-weight: bold; margin-top: 8px;">
       <a href="',
                           "https://www.gov.scot/collections/scottish-agriculture-greenhouse-gas-emissions-and-nitrogen-use",
                           '" target="_blank">
         Source: Scottish agriculture greenhouse gas emissions and nitrogen use balance:  ', fbs_current_year, '
       </a>
     </div></p> ',
                    "<strong>Note:</strong><ul>
    <li>Zoom into the graph by double-clicking on the graph or clicking and dragging over the area you wish to focus on.</li>
    <li>You can see data values for a specific year by hovering your mouse over the line.</li>
  </ul>"))
  
  # nbalance_note <- HTML("<p><strong>Nitrogen balance (kg N surplus/ha)</strong>is the difference between total nitrogen input and output.\nLine shows the median and shaded area shows the 95% confidence interval.</p>")
  # 
  # nue_note <- HTML("<p><strong>Nitrogen use efficiency (% N output / N input)</strong>is the ratio of nitrogen outputs to inputs.\nLine shows the median and shaded area shows the 95% confidence interval.</p>")
  # 
  # 
  # note_content <- if (n_type == "n_balance") {nbalance_note}
  # else if (n_type == "nue") {nue_note}
  # else NULL
  
  tagList(
    htmlOutput(ns("title")),
    withSpinner(plotlyOutput(ns("nitrogen_line_chart")),
      color = getOption("spinner.color", default = "#374f66")),
    # note_content,
    htmlOutput(ns("footer")),
    div(
      class = "note",
      style = "margin-top: 20px; padding: 10px; border-top: 1px solid #ddd;",
      base_note
    )
  )
}

t1<-list(family = 'Arial',
         size = 16,
         color = 'rgb(82, 82, 82)')

line_color <- "rgb(0,100,80)"
fill_color <- "rgba(0,100,80,0.2)"

nitrogenline_ChartServer <- function(id, chart_data, title, yaxistitle, yrange) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    output$title <- renderUI({
      HTML(paste0("<div style='font-size: 20px; font-weight: bold;'>", title, "</div>"))
    })

    output$nitrogen_line_chart <- renderPlotly({
      # message("renderPlotly called")
      data <- chart_data()
      # message("nrow: ", nrow(data))
      # print(head(data))

   p <- plot_ly(
     data = data,
     x = ~year,
     y = ~Upper,
     type = 'scatter',
     mode = 'lines',
     line = list(color = 'transparent'),
     showlegend = FALSE,
     text = paste("<br>Year:", data$year, "<br>Upper CI:", janitor::round_half_up(data$Upper, 2)),
     hoverinfo = 'text'
   ) %>%
     add_trace(
       y = ~Lower,
       type = 'scatter',
       mode = 'lines',
       fill = 'tonexty',
       fillcolor = fill_color,
       line = list(color = 'transparent'),
       showlegend = FALSE,
       text = paste("<br>Year:", data$year, "<br>Lower CI", janitor::round_half_up(data$Lower, 2)),
       hoverinfo = 'text'
     ) %>%
     add_trace(
       x = ~year,
       y = ~Median,
       type = 'scatter',
       mode = 'lines',
       line = list(color=line_color),
       text = paste("<br>Year:", data$year, "<br>Median:", janitor::round_half_up(data$Median,2)),
       hoverinfo = 'text'
     ) %>% 
     layout(yaxis = list(title = list(text = yaxistitle, font = t1),
                         tickfont = t1,
                         range=yrange,
                         zeroline = FALSE
     ),
            xaxis = list(
              title = list(text = "", font = t1, standoff=15),
              tickfont = t1,
              ticklabelposition = "outside",
              ticklen=10,
              tickcolor = "rgba(0,0,0,0)",
              showgrid = FALSE 
              # pushes tick labels slightly away from axis
              
            ),
            showlegend = FALSE)
   
 

          })
  
      })
}


