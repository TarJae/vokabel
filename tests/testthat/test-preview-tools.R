question <- vokabel::create_question(
  'Lorem ipsum dolor sit amet, consectetur adipiscing elit. Select nulla.',
  vokabel::add_choice('auctor'),
  vokabel::add_choice('nulla', correct = TRUE)
)
quiz <- vokabel::create_quiz(
  question,
  vokabel::create_question(
    'Mauris congue aliquet dui, ut dapibus lorem porttitor sed. Select 600.',
    vokabel::add_choice('600', correct = TRUE),
    vokabel::add_choice('800')
  )
)

p_app <- vokabel:::preview_app(quiz)

test_that("preview_app function work", {
  expect_s3_class(p_app, 'shiny.appobj')
})

if (interactive()){
  p_quiz <- suppressMessages(vokabel:::preview_quiz(quiz))
  p_question <- suppressMessages(vokabel:::preview_question(question))
  test_that("preview_quiz and question functions work", {
    expect_s3_class(p_quiz, 'shiny.tag.list')
    expect_s3_class(p_question, 'shiny.tag')
  })
}
