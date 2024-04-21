### UI-specific utility functions ###


#' Convert Image to Base64 URL
#'
#' This function reads an image file from the package's `www` directory and converts it to a base64 URL using the magick package.
#' It allows for dynamic selection of images stored within the package, making it versatile for use in web applications, especially Shiny apps.
#'
#' @param image_name The name of the image file to be converted to a base64 URL.
#' @param package_name The name of the package where the image is stored, with a default value of "vokabel".
#' @importFrom magick image_read image_info image_write
#' @importFrom base64enc base64encode
#' @return A string containing the base64 URL of the image.
#' @export
#' @examples
get_image_base64 <- function(image_name, package_name = "vokabel") {
  # Construct the path to the image within the specified package
  img_path <- system.file("www", image_name, package = package_name)
  
  # Read the image file using magick
  img <- magick::image_read(img_path)
  
  # Convert the image to base64 encoding
  img_base64 <- magick::image_write(img, format = 'png', path = NULL)
  img_url <- paste0("data:image/png;base64,", base64enc::base64encode(img_base64))
  
  return(img_url)
}



#' Scroll the webpage to a certain div
#'
#' @param ns namespace of the Shiny module
#' @param id id of the div
#'
#' @return called for side effect
#' @author Joseph Marlo
#' @noRd
#' @keywords internal
scroll_to_div <- function(ns = NULL, id = 'quiz-container'){
  if (!is.null(ns)) id <- ns(id)
  js <- glue::glue("$('#{id}')[0].scrollIntoView()")
  shinyjs::runjs(js)
}

#' Add a green checkmark to a div
#'
#' @param ns namespace of the Shiny module
#' @param id id of the div to add the checkmark to
#' @param element element within the div to place the checkmark
#'
#' @return called for side effect
#' @author Joseph Marlo
#' @noRd
#' @keywords internal
add_checkmark <- function(ns = NULL, id = 'quiz-container', element = 'h3'){
  
  # construct selector
  if (!missing(ns)) id <- paste0("#", ns(id))
  if (!missing(element)) selector <- paste(id, element)
  div_selector <- paste0('$("', selector, '")')
  
  # construct javascript
  js <- paste0(
    'if (',
    div_selector,
    '.children().length===0){',
    div_selector,
    '.append("\t" + \'<span class="glyphicon glyphicon-ok" style="color:green; font-size: 0.9em;"></span>\')}'
  )
  
  # run js
  shinyjs::runjs(js)
}

#' Add a red X to a div
#'
#' @param ns namespace of the Shiny module
#' @param id id of the div to add the X to
#' @param element element within the div to place the X
#'
#' @return called for side effect
#' @author Joseph Marlo
#' @noRd
#' @keywords internal
add_red_x <- function(ns = NULL, id = 'quiz-container', element = 'h3'){
  
  # construct selector
  if (!missing(ns)) id <- paste0("#", ns(id))
  if (!missing(element)) selector <- paste(id, element)
  div_selector <- paste0('$("', selector, '")')
  
  # construct javascript
  js <- paste0(
    'if (',
    div_selector,
    '.children().length===0){',
    div_selector,
    '.append("\t" + \'<span class="glyphicon glyphicon-remove" style="color:red; font-size: 0.9em;"></span>\')}'
  )
  
  # run js
  shinyjs::runjs(js)
}

#' Add ending messages to the quiz
#'
#' @param text Message to display
#'
#' @return called for side effect
#' @author Joseph Marlo
#' @noRd
#' @keywords internal
#' @describeIn add_message_correct Message to display when user is correct
add_message_correct <- function(text){
  # this relies on bootstrap css
  htmltools::div(
    class = 'alert alert-success',
    htmltools::p(text)
  )
}

#' @keywords internal
#' @describeIn add_message_correct Message to display when user is wrong
add_message_wrong <- function(text){
  # this relies on bootstrap css
  htmltools::div(
    class = 'alert alert-danger',
    htmltools::p(text)
  )
}

#' @keywords internal
#' @describeIn add_message_correct Message to display when user skipped the quiz
add_message_skipped <- function(text){
  # this relies on bootstrap css
  htmltools::div(
    class = 'alert alert-warning',
    htmltools::p(text)
  )
}


#' Add confetti celebration animation
#' 
#' Requires confetti.css to be in the www/css folder
#'
#' @return div containing divs of class "confetti-piece"
#' @author Joseph Marlo
#' @seealso \url{https://codepen.io/zer0kool/pen/KjZWRW}
#' @noRd
#' @keywords internal
add_confetti <- function(){

  confetti_pieces <- htmltools::div(
    class = 'confetti',
    htmltools::div(class = 'confetti-piece'),
    htmltools::div(class = 'confetti-piece'),
    htmltools::div(class = 'confetti-piece'),
    htmltools::div(class = 'confetti-piece'),
    htmltools::div(class = 'confetti-piece'),
    htmltools::div(class = 'confetti-piece'),
    htmltools::div(class = 'confetti-piece'),
    htmltools::div(class = 'confetti-piece'),
    htmltools::div(class = 'confetti-piece'),
    htmltools::div(class = 'confetti-piece')
  )
  
  # remove after 8 seconds to prevent re-animations when coming back to the page
  shinyjs::delay(
    10*800,
    shinyjs::hide(selector = '.confetti')
  )
  
  return(confetti_pieces)
}

#' Create a bootstrap progress bar
#'
#' @param percent The percent to fill the progress bar to
#' @param bg_color Background color
#'
#' @return html
#' @author Joseph Marlo
#' @seealso \url{https://getbootstrap.com/docs/4.0/components/progress/}
#' @noRd
#' @keywords internal
add_progress_bar <- function(percent, bg_color) {
  if (percent < 0 | percent > 1) cli::cli_abort('`percent` must be [0, 1]')
  percent <- round(percent * 100, digits = 0)
  if (is.null(bg_color)) bg_color <- "#609963"
    
  htmltools::div(
    class = 'progress',
    htmltools::div(
      class = 'progress-bar',
      role  = 'progressbar',
      style = glue::glue("width: {percent}%; background: {bg_color};"),
      `aria-valuenow` = percent,
      `aria-valuemin` = "0",
      `aria-valuemax` = "100"
    )
  )
}
