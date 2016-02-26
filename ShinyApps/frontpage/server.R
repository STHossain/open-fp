#library(shiny)
#library(dplyr)
#library(ggplot2)
#library(plotly)


current.year <- Sys.Date() %>% format("%Y") %>% as.numeric() 

#load(forecast.panel.rds)
forecast.panel <- readRDS("/home/onno/open-fp/forecast_panel_frontpage.rds")

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  output$forecastPlot <- renderPlotly({
    
    last.issued.year <- forecast.panel %>% filter(region == input$region) %>% summarise(max(issued.year)) %>% as.numeric()
    
    last.issued.quarter <- forecast.panel %>% 
      filter(region == input$region &
               issued.year == last.issued.year) %>% 
      summarise(max(issued.quarter)) %>% 
      as.numeric()
    
    gg.data <- forecast.panel %>%
      filter(region == input$region &
               variable == input$variable &
               issued.year == last.issued.year &
               issued.quarter == last.issued.quarter) %>%
      filter(target.year == current.year | target.year == (current.year+1))
    
    gg <- ggplot(gg.data) + 
      geom_boxplot(aes(x = target.year, y = point.forecast, group = target.year)) +
      xlab('target period') +
      scale_y_continuous("point forecasts")
    
    plotly::config(ggplotly(gg), staticPlot = F, showLink=F, sendData=F, displaylogo=F, displayModeBar = F)
    
  })

})