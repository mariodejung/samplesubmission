
#' performs a validation on the whole web-form
#' @param inputs isolated input list
#' @return list of validation results
#' @seealso Validated
#' @examples
#' \dontrun{
#' validateInputValues(isolate(reactiveValuesToList(input)))
#' }
validateInputValues <- function (inputs) {
  
  #list of named lists whith the following members

  #input_element_id
  #valid (locgical)
  #message

  results <- c(
    samplesubmission:::validateUserInformation(inputs),
    samplesubmission:::validateDataBases(inputs),
    samplesubmission:::validateBarCodes(inputs, pattern=paste0("^", inputs$group, "_\\d{3,5}$")),
    samplesubmission:::validateFastaInputs(inputs),
    samplesubmission:::validateGeneralSettings(inputs)
  )


  ##TODO: validate other input field
  return(results)
}


