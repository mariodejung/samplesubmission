#' creates a status object (named list)
#' comfort function
#' @param input_element_id id of a input element
#' @param message message string
#' @param valid validation status
#' @return anmed list containing the inputs
#' @examples
#' Validated("surename", "Mandatory field", FALSE )
Validated <- function(input_element_id, message=NULL, valid=FALSE) {
  if(is.null(message)) {
    my_message <- sprintf('"%s" is not set', input_element_id)
  } else {
    my_message <- message
  }
  return(list(
    input_element_id=input_element_id,
    valid=valid,
    message=my_message
  ))
}

