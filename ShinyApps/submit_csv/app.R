setwd("~/ShinyApps/submit_csv/")

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
    titlePanel("Submit forecasts via .csv"),
    
    div(
      id = "form",
      textInput("forecaster.id", labelMandatory("Forecaster ID"), ""),
      fileInput('file1', 'Choose file to upload',
                accept = c(
                  'text/csv',
                  'text/comma-separated-values',
                  'text/tab-separated-values',
                  'text/plain',
                  '.csv',
                  '.tsv'
                )
      ),
      actionButton("submit", "Submit", class = "btn-primary")
      )
    ),
  
  server = function(input, output, session) {
    responses.dir <- file.path("/home/onno/Submissions/")
    humanTime <- function() format(Sys.time(), "%Y%m%d-%H%M%OS")
    
    saveData <- function(data) {
      #fileName <- sprintf("%s_%s.csv",
      #                    humanTime(),
      #                    digest::digest(data))
      #
      file.name <- "test2.csv"
      write_csv(x = data, path = "/home/onno/Submissions/test2.csv")#file.path(responsesDir, fileName))
    }
    
    observe({
      mandatoryFilled <- vapply(fieldsMandatory,
                                function(x) {
                                  !is.null(input[[x]]) && input[[x]] != ""
                                  },
                                logical(1))
      mandatoryFilled <- all(mandatoryFilled)
      
      shinyjs::toggleState(id = "submit", condition = mandatoryFilled)
      })
    
    observeEvent(input$submit, {
      write_csv(x = input$submit, path = "/home/onno/Submissions/test2.csv")
      #saveData()
      })
    }
  )