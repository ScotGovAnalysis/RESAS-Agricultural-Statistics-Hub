
library(tidyverse)
library(here)
library(plotly)


load("Data/n_balance.Rda")
load("Data/nue.Rda")


n_balance<-table_5_df %>% 
  mutate(n_type="n_balance") %>% 
  pivot_longer(
    cols = `2019-20`:fbs_current_year,
    names_to = "year",
    values_to="value")


nue<-table_6_df %>% 
  mutate(n_type="nue") %>% 
  pivot_longer(
    cols = `2019-20`:fbs_current_year,
    names_to = "year",
    values_to="value")

nitrogen_data<-dplyr::bind_rows(n_balance, nue) %>% 
  rename(farm_type=`Farm type`) %>% 
  filter(Measure=="Average (median)")

nitrogenline_ChartUI <- function(id, in_out_type = NULL) {
  ns <- NS(id)
  
  base_note <- HTML("<strong>Note:</strong><ul>
    <li>Zoom into the graph by clicking and dragging over the area you wish to focus on.</li>
    <li>You can see data values for a specific year by hovering your mouse over the line.</li>
  </ul>")
  
  tagList(
    htmlOutput(ns("title")),
    plotlyOutput(ns("nitrogen_line_chart")),
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

nitrogenline_ChartServer <- function(id, chart_data, title, yaxistitle) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    output$title <- renderUI({
      HTML(paste0("<div style='font-size: 20px; font-weight: bold;'>", title, "</div>"))
    })


    output$nitrogen_line_chart <- renderPlotly({
      message("renderPlotly called")
      data <- chart_data()
      message("nrow: ", nrow(data))
      print(head(data))

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
                         tickfont = t1),
            xaxis = list(
              title = list(text = "Year", font = t1, standoff=15),
              tickfont = t1,
              ticklabelposition = "outside",
              ticklen=10,
              tickcolor = "rgba(0,0,0,0)"# pushes tick labels slightly away from axis
              
            ),
            showlegend = FALSE)

          })
  
      })
}


