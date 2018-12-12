
#' performs a validation on the whole web-form
#' @param inputs isolated input list
#' @return list of validation results
#' @seealso Validated
#' @examples
#' validateInputValues(isolate(reactiveValuesToList(input)))
validateInputValues <- function (inputs) {
  #user_info_fields <- NULL
  #list of named lists whith the following members

  #input_element_id
  #valid (locgical)
  #message

  results <- c(
    validateUserInformation(inputs, names(user_info_fields)),
    validateDataBases(inputs),
    validateBarCodes(inputs),
    validateFastaInputs(inputs),
    validateGeneralSettings(inputs)
  )


  ##TODO: validate other input field
  return(results)
}


