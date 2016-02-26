setwd("~/ShinyApps/submit/")
fieldsMandatory <- c("forecaster.id")

labelMandatory <- function(label) {
  tagList(
    label,
    span("*", class = "mandatory_star")
  )
  }
 
appCSS <- ".mandatory_star { color: red; }"

shinyApp(
  ui = fluidPage(
    shinyjs::useShinyjs(),
    shinyjs::inlineCSS(appCSS),
    #titlePanel("Submit forecasts"),
    div(
      id = "form",
      textInput("forecaster.id", "Forecaster ID", ""),
      selectInput("variable", 
                  labelMandatory("Macroeconomic variable"),
                  choices = list("GDP growth", "Inflation", "Unemployment"),
                  selected = "GDP growth"
                  ),
      selectInput("variable", 
                  labelMandatory("Region"),
                  choices = list("Euro"),
                  selected = "Euro"
                  ),
      selectInput("Year",
                  labelMandatory("Target year"),
                  choices = c(2016:2018),
                  selected = 2016
                  ),
      selectInput("Quarter",
                  labelMandatory("Target quarter"),
                  choices = c(1:4),
                  selected = 2
      ),
      #selectInput("")
      #checkboxInput("used_shiny", "I've built a Shiny app in R before", FALSE),
      #sliderInput("r_num_years", "Number of years using R", 0, 25, 2, ticks = FALSE),
      #selectInput("os_type", "Operating system used most frequently",
      #            c("",  "Windows", "Mac", "Linux")),
      actionButton("submit", "Submit", class = "btn-primary")
    )
  ),
  
  server = function(input, output, session) {
    observe({
      mandatoryFilled <- vapply(fieldsMandatory,
                                function(x) {
                                  !is.null(input[[x]]) && input[[x]] != ""
                                },
                                logical(1))
      mandatoryFilled <- all(mandatoryFilled)
      
      shinyjs::toggleState(id = "submit", condition = mandatoryFilled)
    })
    output$choose_dataset <- renderUI({
      selectInput("dataset", "Data set", as.list(data_sets))
    })
  }
)