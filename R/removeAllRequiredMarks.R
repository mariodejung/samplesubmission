
#' This function will remove all marks being set in markRequiredInput
#' @export
#' @import shiny
#' @examples
#' \dontrun{
#' removeAllReuiredMarks()
#' }
removeAllRequiredMarks <- function(){
  removeUI(selector=".required-asterix, .required-error-message",
           multiple=TRUE)
}

