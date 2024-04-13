### fully working example app ###

devtools::load_all()


# content -----------------------------------------------------------------

ns_quiz <- shiny::NS('quiz')

quiz <- create_quiz(
  create_question(
    'Select B',
    add_choice('A'),
    add_choice('B'),
    add_choice('C', correct = TRUE)
  ),
  create_question(
    'Lorem ipsum dolor sit amet, consectetur adipiscing elit. Sed id ornare augue, fringilla molestie metus. Donec eget tortor tincidunt, sagittis dui volutpat, finibus est. Select nulla.',
    add_choice('Nulla vel'),
    add_choice('auctor nulla'),
    add_choice('nulla', correct = TRUE)
  ),
  create_question(
    'Molestie metus. Maecenas tincidunt maximus viverra. Sed non gravida quam. Phasellus at iaculis leo. Mauris congue aliquet dui, ut dapibus lorem porttitor sed.',
    add_choice(500),
    add_choice('600', correct = TRUE),
    add_choice('six hundred'),
    type = 'multiple',
    label = 'Select 600'
  )
  
)


# app ---------------------------------------------------------------------

preview_app(quiz)