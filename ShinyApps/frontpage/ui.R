library(shiny)
library(plotly)


mycss <- "
#plot-container {
position: relative;
}
#loading-spinner {
position: absolute;
left: 50%;
top: 50%;
z-index: -1;
margin-top: -33px;  /* half of the spinner's height */
margin-left: -33px; /* half of the spinner's width */
}
#plot.recalculating {
z-index: -2;
"

# Define UI for application that plots random distributions 
shinyUI(fluidPage(
  #tags$head(tags$style(HTML(mycss))),
  # Application title
  headerPanel("Macroeconomic Outlook"),
  
  # Sidebar with a slider input for number of observations
  sidebarPanel(
    
    helpText("Boxplot of point forecasts up to two years ahead."),
    
    selectInput("variable", 
                label = "Choose a variable to display",
                choices = list("GDP growth", "Inflation", "Unemployment"),
                selected = "GDP growth"),
    
    selectInput("region", 
                label = "Choose a region to display",
                #choices = list("Euro", "US"),
                choices = list("Euro"),
                selected = "Euro")
  ),
  
  # Show a plot of the generated distribution
  mainPanel(
    
    #div(id = "plot-container",
    #    tags$img(src = "http://www.ajaxload.info/images/exemples/35.gif",
    #             id = "loading-spinner"),
        plotlyOutput("forecastPlot", width = "100%", height = "300px")
    #)
    
  )
))
