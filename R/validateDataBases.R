#' performs a validation on databases input fields
#' @param inputs isolated input list
#' @return list of validation results
#' @seealso Validated
#' @examples
#' validateDataBases(isolate(reactiveValuesToList(input)))
validateDataBases <- function(inputs) {
  results <- list()
  if(is.null(inputs$species_dbs) && inputs$custom_species_db == ''){
    results[[length(results) + 1]] <- Validated("species_dbs",
                                                valid=FALSE,
                                                message='No species database chosen')
  }
  else{
    results[[length(results) + 1]] <- Validated("species_dbs",
                                                valid=TRUE,
                                                message=paste(inputs$species_dbs))
  }
  return(results)
}
