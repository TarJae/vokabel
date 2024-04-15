### shiny module that implements the state machine ###

#' Run a quiz in 'shiny'
#'
#' @description A 'shiny' module to implement a quiz. These are the core functions to implement the quiz with a 'shiny' application.
#'
#' @param quiz an object of class `quiz`. See [create_quiz()]
#' @author Joseph Marlo
#' @export
#' 
#' @seealso [create_quiz()] [preview_app()]
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
#' 
#' ui <- shiny::fluidPage(
#'   htmltools::div(
#'     style = "max-width: 700px",
#'     quiz_ui(quiz)
#'    )
#'  )
#' server <- function(input, output, session) {
#'  quiz_server(quiz)
#' }
#' shinyApp(ui, server)
#' @describeIn quiz_ui UI side function
quiz_ui <- function(quiz){
  
  verify_quiz_structure(quiz)
  ns <- quiz@options$ns

  htmltools::tagList(
    add_external_resources(),
    htmltools::div(
      id = ns('quiz-container'),
      class = 'quiz-container',
      shiny::uiOutput(outputId = ns('UI_quiz'))
    )
  )
}

#' @param quiz an object of class `quiz`. See [create_quiz()]
#' @export
#' @return a reactive object showing the current results of the quiz
#' 
#' @describeIn quiz_ui Server side function
quiz_server <- function(quiz){
  
  verify_quiz_structure(quiz)
  ns <- quiz@options$ns
  id <- ns(NULL)
  if (!isTRUE(stringr::str_count(id, '-') <= 1)) cli::cli_abort('namespace id is invalid. Too many hyphens. Remove any hyphens in your id. Otherwise, are you trying to nest modules more than two deep?')
  id <- stringr::str_remove_all(id, "^.*?-") # remove any prefixes due to parent modules; this is fragile but works
  
  shiny::moduleServer(id, function(input, output, session){

    # add css class to the quiz container
    if (is_truthy(quiz@options$class)) shinyjs::addClass(id = 'quiz-container', class = quiz@options$class)
    
    # add headers to question texts
    quiz <- sm_ui_format_prompts(quiz)
    
    # set the current state and potential values
    store <- sm_create_reactive_store(quiz)
    
    # reset quiz
    shiny::observeEvent(input$restart_button, {
      
      # reset the state to the first question
      store <- sm_set_state(store, variable = 'current-state', value = 'quiz-question-1')
      
      # remove any responses
      store$questions <- quiz@questions
      store <- sm_set_state(store, variable = 'quiz-skipped', value = FALSE)
      store$is_correct <- rep(NA, length(quiz@questions))
    })
    
    # skip quiz / finish quiz
    shiny::observeEvent(input$skip_button, {
      store <- sm_set_state(store, variable = 'current-state', value = 'quiz-complete')
      store <- sm_set_state(store, variable = 'quiz-skipped', value = TRUE)
    })
    
    # control state behavior
    shiny::observeEvent(store$state, {
      
      # scroll to top of quiz container
      scroll_to_div(ns = ns, id = 'quiz-container')
      
      # state behavior
      if (store$state == 'quiz-complete'){
        # determine the UI
        store$ui_html <- sm_ui_quiz_complete(
          store,
          ns = ns,
          messages = quiz@options$messages
        )

      } else {
        # determine the UI
        store$ui_html <- sm_ui_question(store, ns = ns)
      }
    })
    
    
##To evaluate the answer right after clicking radio button, mimicking the same action as with submit button: 
    
<<<<<<< HEAD
    #commeneted out
    # observeEvent(input$answers, {
    #   # Ensure an answer has been selected (ignoreNULL is set to TRUE by default in this context)
    #   if (!is.null(input$answers)) {
    #     # Disable the radio buttons immediately to prevent changes while processing
    #     shinyjs::disable(id = ns('answers'))
    #     
    #     # Scroll to top of the quiz container if needed
    #     scroll_to_div(ns = ns, id = 'quiz-container')
    #     
    #     # Record the answer chosen by the user
    #     store <- sm_set_state(store, variable = 'current-response', value = input$answers)
    #     
    #     # Check if the answer is correct
    #     is_correct <- sm_is_current_correct(store)
    #     store <- sm_set_state(store, variable = 'current-correct', is_correct)
    #     
    #     # Add UI indicator based on correctness
    #     if (is_correct) {
    #       add_checkmark(ns = ns, id = 'quiz-container', element = 'h3')
    #     } else {
    #       add_red_x(ns = ns, id = 'quiz-container', element = 'h3')
    #     }
    #     
    #     # Delay further actions by 1000 ms, then proceed based on the quiz logic
    #     shinyjs::delay(800, {
    #       if (is_correct || !sm_logic_end_on_first_wrong(store)) {
    #         new_state <- sm_get_state(store, variable = 'next-state')
    #         store <- sm_set_state(store, variable = 'current-state', value = new_state)
    #       } else {
    #         store <- sm_set_state(store, variable = 'current-state', value = 'quiz-complete')
    #       }
    #       
    #       # Optionally re-enable the radio buttons if moving to the next question
    #       shinyjs::enable(id = ns('answers'))
    #     })
    #   }
    # }, ignoreNULL = TRUE)  # Only trigger after a selection has been made
    

    # on button submit, record answer and change the state
    shiny::observeEvent(input$submit_button, {

      # disable submit button to prevent double clicks
      shinyjs::disable(id = 'submit_button')

      # scroll to top of quiz container
      scroll_to_div(ns = ns, id = 'quiz-container')

      # record answers
      store <- sm_set_state(store, variable = 'current-response', value = input$answers)

      # is the answer correct and record it
      is_correct <- sm_is_current_correct(store)
      store <- sm_set_state(store, 'current-correct', is_correct)

      # grade it
      delay_in_ms <- 1000
      if (is_correct){
        # add UI indicator
        add_checkmark(ns = ns, id = 'quiz-container', element = 'h3')

        # change the state
        shinyjs::delay(delay_in_ms, {
          new_state <- sm_get_state(store, variable = 'next-state')
          store <- sm_set_state(store, variable = 'current-state', value = new_state)
        })

      } else {
        # add UI indicator
        add_red_x(ns = ns, id = 'quiz-container', element = 'h3')

        # change the state
        # depending on options, go to next question otherwise end here
        shinyjs::delay(delay_in_ms, {
          if (sm_logic_end_on_first_wrong(store)){
            store <- sm_set_state(store, variable = 'current-state', value = 'quiz-complete')
          } else {
=======
    observeEvent(input$answers, {
      # Ensure an answer has been selected (ignoreNULL is set to TRUE by default in this context)
      if (!is.null(input$answers)) {
        # Disable the radio buttons immediately to prevent changes while processing
        shinyjs::disable(id = ns('answers'))
        
        # Scroll to top of the quiz container if needed
        scroll_to_div(ns = ns, id = 'quiz-container')
        
        # Record the answer chosen by the user
        store <- sm_set_state(store, variable = 'current-response', value = input$answers)
        
        # Check if the answer is correct
        is_correct <- sm_is_current_correct(store)
        store <- sm_set_state(store, variable = 'current-correct', is_correct)
        
        # Add UI indicator based on correctness
        if (is_correct) {
          add_checkmark(ns = ns, id = 'quiz-container', element = 'h3')
        } else {
          add_red_x(ns = ns, id = 'quiz-container', element = 'h3')
        }
        
        # Delay further actions by 1000 ms, then proceed based on the quiz logic
        shinyjs::delay(800, {
          if (is_correct || !sm_logic_end_on_first_wrong(store)) {
>>>>>>> 07a501f5119de27b32d36bdf8f8be534199dc632
            new_state <- sm_get_state(store, variable = 'next-state')
            store <- sm_set_state(store, variable = 'current-state', value = new_state)
          } else {
            store <- sm_set_state(store, variable = 'current-state', value = 'quiz-complete')
          }
          
          # Optionally re-enable the radio buttons if moving to the next question
          shinyjs::enable(id = ns('answers'))
        })
      }
<<<<<<< HEAD
    })

=======
    }, ignoreNULL = TRUE)  # Only trigger after a selection has been made
    
# #Commented out button submit
#     # on button submit, record answer and change the state
#     shiny::observeEvent(input$submit_button, {
#       
#       # disable submit button to prevent double clicks
#       shinyjs::disable(id = 'submit_button')
#       
#       # scroll to top of quiz container
#       scroll_to_div(ns = ns, id = 'quiz-container')
#       
#       # record answers
#       store <- sm_set_state(store, variable = 'current-response', value = input$answers)
#       
#       # is the answer correct and record it
#       is_correct <- sm_is_current_correct(store)
#       store <- sm_set_state(store, 'current-correct', is_correct)
#       
#       # grade it
#       delay_in_ms <- 1000
#       if (is_correct){
#         # add UI indicator
#         add_checkmark(ns = ns, id = 'quiz-container', element = 'h3')
#         
#         # change the state
#         shinyjs::delay(delay_in_ms, {
#           new_state <- sm_get_state(store, variable = 'next-state')
#           store <- sm_set_state(store, variable = 'current-state', value = new_state)
#         })
#         
#       } else {
#         # add UI indicator
#         add_red_x(ns = ns, id = 'quiz-container', element = 'h3')
#         
#         # change the state
#         # depending on options, go to next question otherwise end here
#         shinyjs::delay(delay_in_ms, {
#           if (sm_logic_end_on_first_wrong(store)){
#             store <- sm_set_state(store, variable = 'current-state', value = 'quiz-complete')
#           } else {
#             new_state <- sm_get_state(store, variable = 'next-state')
#             store <- sm_set_state(store, variable = 'current-state', value = new_state)
#           }
#         })
#       }
#     })
#     
>>>>>>> 07a501f5119de27b32d36bdf8f8be534199dc632
    # render the UI
    output$UI_quiz <- shiny::renderUI(store$ui_html)

    # return the quiz summary
    return(shiny::reactive(sm_summary(store, quiz)))
  })
}

#' External resources to include in the app 
#'
#' Examples include `shinyjs::useShinyjs` or `fontawesome::fa_html_dependency`. These objects are raised to the head of html document.
#'
#' @return an invisible object of class shiny.tag
#' @noRd
#' @author Joseph Marlo
add_external_resources <- function(){
  htmltools::tags$head(
    shinyjs::useShinyjs(),
    fontawesome::fa_html_dependency(),
    htmltools::includeCSS(system.file('vokabel.css', package = "vokabel")),
  )
}
