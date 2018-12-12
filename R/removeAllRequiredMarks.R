
#' This function will remove all marks being set in markRequiredInput
#' @export
#' @import shiny
#' @examples
#' removeAllReuiredMarks()
removeAllRequiredMarks <- function(){
  removeUI(selector=".required-asterix, .required-error-message",
           multiple=TRUE)
}

