### fully working example app ###

devtools::load_all()


# content -----------------------------------------------------------------

ns_quiz <- shiny::NS('quiz')

quiz <- create_quiz(
  create_question(
    'Select C',
    add_choice('A'),
    add_choice('B'),
    add_choice('C', correct = TRUE)
  ),
  create_question(
    'Lorem ipsum dolor sit amet, consectetur adipiscing elit. Sed id ornare augue, fringilla molestie metus. Donec eget tortor tincidunt, sagittis dui volutpat, finibus est. Select nulla.',
    add_choice('Nulla vel'),
    add_choice('auctor nulla'),
    add_choice('nulla', correct = TRUE)
  )
  
)


# app ---------------------------------------------------------------------

preview_app(quiz)