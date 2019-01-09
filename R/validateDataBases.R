#' performs a validation on databases input fields
#' @param inputs isolated input list
#' @return list of validation results
#' @seealso Validated
#' @examples
#' \dontrun{
#' validateDataBases(isolate(reactiveValuesToList(input)))
#' }
validateDataBases <- function(inputs) {
  
  
  results <- list()
  if((is.null(inputs$species_dbs) || inputs$species_dbs=="--none--")  && inputs$custom_species_db == ''){
    results[[length(results) + 1]] <- Validated("species_dbs",
                                                valid=FALSE,
                                                message='No species database chosen')
  }else{
    results[[length(results) + 1]] <- Validated("species_dbs",
                                                valid=TRUE,
                                                message=paste(inputs$species_dbs))
  }
  
  if(inputs$background_dbs!="--none--" && any(inputs$species_dbs %in% inputs$background_dbs)){
    results[[length(results) + 1]] <- Validated("background_dbs",
                                                valid=FALSE,
                                                message="Same species as in species DB chosen")
  }
  

# Sequence ----------------------------------------------------------------

  if(validateFastaSequences(inputs$fasta_sequence)){
    results[[length(results) + 1]] <- Validated("fasta_sequence",
                                                valid=TRUE,
                                                message="Fasta sequence is valid or empty")
  }else{
    results[[length(results) + 1]] <- Validated("fasta_sequence",
                                                valid=FALSE,
                                                message="Does not meet the requirements")
  }
  
  return(results)
}
