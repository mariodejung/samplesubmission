#' performs a validation on user infos fields
#' @param inputs isolated input list
#' @return list of validation results
#' @seealso Validated
#' @examples
#' \dontrun{
#' validateUserInformation(isolate(reactiveValuesToList(input)))
#' }
validateUserInformation <- function(inputs) {
  
  user_info_fields <- get('user_info_fields')
  user_inputs_info_ids <- names(user_info_fields)

  results <- list()
  ##TODO avoid loops
  for(inputId in user_inputs_info_ids){

    if(inputs[[inputId]] == '') {
      results[[length(results) + 1]] <- Validated(inputId,
                                                  valid=FALSE,
                                                  message='Field is mandatory')
    } else {
      results[[length(results) + 1]] <- Validated(inputId,
                                                  valid=TRUE,
                                                  message='Field is OK')
    }
  }
  #email validation
  if(!grepl("\\<[A-Z0-9._%+-]+@[A-Z0-9.-]+\\.[A-Z]{2,}\\>", inputs$email, ignore.case=TRUE)){
    results[[length(results) + 1]] <- Validated("email",
                                                valid=FALSE,
                                                message='Wrong email format')
  }
  
  if(inputs$group == ''){
    results[[length(results) + 1]] <- Validated("group", "Please specify a group")
  }

  return(results)
}
