quiz <- vokabel::create_quiz(
  vokabel::create_question(
    'Lorem ipsum dolor sit amet, consectetur adipiscing elit. Select nulla.',
    vokabel::add_choice('auctor'),
    vokabel::add_choice('nulla', correct = TRUE)
  ),
  vokabel::create_question(
    'Mauris congue aliquet dui, ut dapibus lorem porttitor sed. Select 600.',
    vokabel::add_choice('600', correct = TRUE),
    vokabel::add_choice('800')
  )
)
store <- vokabel:::sm_create_reactive_store(quiz)
state1 <- vokabel:::sm_get_state(store)
state2 <- vokabel:::sm_get_state(store, 'next-state')
state3 <- vokabel:::sm_get_state(store, 'current-question')
correct <- vokabel:::sm_check_is_each_correct(store)
score <- vokabel:::sm_score_quiz(store)

quiz_complete <- vokabel:::sm_quiz_is_complete(store)
store2 <- vokabel:::sm_set_state(store, 'current-state', 'quiz-complete')
state21 <- vokabel:::sm_get_state(store2)
quiz_complete2 <- vokabel:::sm_quiz_is_complete(store2)

test_that('state machine works', {
  expect_type(store, 'list')
  
  expect_type(state1, 'character')
  expect_equal(state1, 'quiz-question-1')
  
  expect_type(state2, 'character')
  expect_equal(state2, 'quiz-question-2')
  
  expect_s3_class(state3, 'shiny.tag')
  
  expect_false(any(correct))
  
  expect_equal(score, 0)
  
  expect_false(quiz_complete)
  expect_equal(state21, 'quiz-complete')
  expect_true(quiz_complete2)
})
