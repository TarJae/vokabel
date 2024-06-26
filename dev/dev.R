## this is a meta script to log the commands that create the package ##

# package setup
renv::activate()
install.packages('devtools')
usethis::create_package('.')
usethis::git_vaccinate()

# depends
usethis::use_package("R", type = 'depends', min_version = '4.1.0') # b/c lambda function

# imports
usethis::use_package("base64enc")
usethis::use_package("magick")
usethis::use_package("here")
usethis::use_package("shinyjs", min_version = '1.7.4')
usethis::use_package("shiny", min_version = '1.7.4')
usethis::use_package("fontawesome", min_version = '0.5.0')
usethis::use_package("htmltools", min_version = '0.5.5')
usethis::use_package("reactable", min_version = '0.4.4')
usethis::use_package("shinyjs", min_version = '2.1.0')
usethis::use_package('purrr', min_version = '1.0.1')
usethis::use_package('renv', min_version = NULL)
usethis::use_package('glue', min_version = '1.6.2')
usethis::use_package('cli', min_version = '3.6.1')
usethis::use_package('scales', min_version = '1.2.1')
usethis::use_package('methods', min_version = '4.0.0')
usethis::use_package('stringr', min_version = '1.5.0')
usethis::use_package('stringi', min_version = '1.7.12')
usethis::use_package("sortable", min_version = '0.5.0', type = 'Suggests')
usethis::use_package('renv', type = 'Suggests')
usethis::use_package('testthat', min_version = TRUE, type = 'Suggests')
usethis::use_package('pkgload', type = 'Suggests')
# renv::snapshot()

# create function files
usethis::use_r('constructors')
usethis::use_r('creators')
usethis::use_r('state-machine')
usethis::use_r('shiny-module')
usethis::use_r('preview-tools')
usethis::use_r('example-app')
usethis::use_r('utils-ui')
usethis::use_r('utils')

# create tests
usethis::use_testthat()
usethis::use_test('state-machine')
usethis::use_test('preview-tools')
usethis::use_test('creators')

# vignettes
usethis::use_vignette('get_started', title = 'Get Started')
usethis::use_vignette('custom_questions', title = 'Customizing Questions')
usethis::use_vignette('random_questions', title = 'Creating Randomized Questions')
usethis::use_vignette('nested_modules', title = 'Quiz in a Module')
usethis::use_vignette('deploying', title = 'Deploying to shinyapps.io')
usethis::use_vignette('styling', title = 'Styling a Quiz using CSS')

# other
usethis::use_github_action_check_release("R-CMD-check.yaml")
usethis::use_github_actions_badge(name = "R-CMD-check.yaml")
usethis::use_lifecycle_badge(stage = 'experimental')
usethis::use_mit_license()

# pkgdown website
usethis::use_pkgdown()
pkgdown::build_site_github_pages(new_process = FALSE, install = FALSE)
usethis::use_github_action("pkgdown")
# go to repo Settings -> Pages -> Source: Deploy from a branch & Branch = 'gh-pages'

# testing
devtools::load_all()
devtools::document()
devtools::test()


# build package
usethis::use_build_ignore('dev')
usethis::use_build_ignore('.github')
devtools::check()
devtools::build()
