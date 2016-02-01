#library(shiny)
#library(dplyr)
#library(ggplot2)
#library(plotly)

#load(forecast.panel.rds)
forecast.panel <- readRDS("/home/onno/open-fp/forecast_panel_frontpage.rds")

# Define server logic required to draw a histogram
shinyServer(function(input, output) {

  output$forecastPlot <- renderPlotly({
    
    gg.data <- forecast.panel %>%
      filter(region == input$region) %>%
      filter(variable == input$variable) %>%
      filter(issued.year == 2016) %>%
      filter(target.year == 2016 | target.year == 2017) %>%
      filter(is.na(point.forecast) == FALSE) %>%
      select(target.year, point.forecast) %>%
      group_by(target.year)
    
    gg <- ggplot(gg.data) + 
      geom_boxplot(aes(x = target.year, y = point.forecast, group = target.year)) +
      xlab('target period') +
      scale_y_continuous("point forecasts")
    
    p <- ggplotly(gg)
    p <- plotly::config(p, staticPlot = F, showLink=F, sendData=F, displaylogo=F, displayModeBar = F)
    p
    
  })

})