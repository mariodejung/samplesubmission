#' performs a validation on miscellaneous settings
#' @param inputs isolated input list
#' @return list of validation results
#' @seealso Validated
#' @examples
#' validateUserInformation(isolate(reactiveValuesToList(input)))
validateGeneralSettings <- function(inputs) {
  results <- list()
  
  #band_weight_kda
  
  weight <- as.numeric(inputs$band_weight_kda)
  
  if(is.na(weight)){
    results[[length(results) + 1]] <- Validated("band_weight_kda",
                                                valid=FALSE,
                                                message='Please enter a valid(numeric) value')
  }else if(weight==0){
    results[[length(results) + 1]] <- Validated("band_weight_kda",
                                                valid=FALSE,
                                                message='Please specify band molecular wight')
  }
  
  
  
  return(results)
}
