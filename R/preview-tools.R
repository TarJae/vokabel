### tools to help users preview their quizzes ###


# shiny preview -----------------------------------------------------------

#' Tools for previewing quizzes
#'
#' Launch a viewer to preview the structure of the questions in a quiz.
#'
#' @param quiz an object of class 'quiz' to preview
#' @param launch_browser launch in a web browser?
#'
#' @return Called for side effect
#' @export
#' @author Joseph Marlo
#'
#' @examplesIf interactive()
#' quiz <- create_quiz(
#'   create_question(
#'     'Lorem ipsum dolor sit amet, consectetur adipiscing elit. Select nulla.',
#'     add_choice('auctor'),
#'     add_choice('nulla', correct = TRUE)
#'   ),
#'   create_question(
#'     'Mauris congue aliquet dui, ut dapibus lorem porttitor sed. Select 600.',
#'     add_choice('600', correct = TRUE),
#'     add_choice('800')
#'   )
#' )
#' preview_app(quiz)
#' @describeIn preview_app Preview a quiz with full operability
preview_app <- function(quiz, launch_browser = TRUE) {
  verify_quiz_structure(quiz)
  
  ui <- shiny::fluidPage(
    htmltools::div(
      style = "max-width: 500px",
      quiz_ui(quiz),
      shiny::checkboxInput('show', 'Show output'),
      shiny::conditionalPanel(
        condition = "input.show == true",
        shiny::verbatimTextOutput('quizSummary'),
        shiny::verbatimTextOutput('statsCorrect'),
        shiny::verbatimTextOutput('statsTotal')
      )
    )
  )
  
  server <- function(input, output, session) {
    
    # 
    quiz_summary <- quiz_server(quiz)
    
    # Display the full summary
    output$quizSummary <- shiny::renderPrint({
      quiz_summary()$summary
    })
    
    output$statsCorrect <- shiny::renderText({
      paste("Correct Answers:", quiz_summary()$correct_answers)
    })
    
    output$statsTotal <- shiny::renderText({
      paste("Total Questions:", quiz_summary()$total_questions)
    })

  }
  
  shiny::shinyApp(ui, server, options = list(launch.browser = !isFALSE(launch_browser)))
}


# html preview ------------------------------------------------------------

# these are used the print methods

#' @noRd
#' @keywords internal
#' @describeIn preview_app Quick preview a quiz 
preview_quiz <- function(quiz, launch_browser = FALSE){
  verify_quiz_structure(quiz)
  viewer <- ifelse(
    isTRUE(launch_browser), 
    getOption("browser", utils::browseURL),
    getOption("viewer", utils::browseURL)
  )
  
  panels <- shiny::fluidPage(
    base::do.call(
      shiny::tabsetPanel, 
      c(id = 't', 
        purrr::map2(quiz@questions, seq_along(quiz@questions), function(q, i) {
          shiny::tabPanel(
            title = glue::glue('Question {i}'), 
            q@prompt
          )
        })
      )
    )
  )
  htmltools::html_print(panels, viewer = viewer)
  cli::cli_alert_warning('Some items like `shiny::renderPlot` may not show correctly in the viewer')
  return(invisible(panels))
}

#' @param question an object of class 'quizQuestion' to preview
#' @noRd
#' @keywords internal
#' @describeIn preview_app Quick preview a single question
preview_question <- function(question, launch_browser = FALSE){
  verify_question_structure(question)
  viewer <- ifelse(
    isTRUE(launch_browser), 
    getOption("browser", utils::browseURL),
    getOption("viewer", utils::browseURL)
  )
  
  htmltools::html_print(question@prompt, viewer = viewer)
  cli::cli_alert_warning('Some items like sliders may not show correctly in the viewer')
  return(invisible(question@prompt))
}
