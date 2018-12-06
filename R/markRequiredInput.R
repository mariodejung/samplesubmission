
#' Marks an input field as required
#' appends an asterix to the label und a message unter the label
#' @export
#' @param inputId the id of an input field
#' @param message a messaged showed under the input field label
#' @examples
#' markRequiredInput("surename", "field is mandatory")
markRequiredInput <- function(inputId, message=''){

  jquery <- paste0("label[for='", inputId, "']")

  insertUI(selector=jquery,
           where="beforeEnd",
           ui=tags$span(class="required-asterix","*"))


  insertUI(selector=jquery,
           where="afterEnd",
           ui=tags$p(class="required-error-message",message))

}
