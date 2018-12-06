#' validates input fields related to uploaded fasta files
#' @param inputs isolated inputs
#' @param input_ids input fields ids
#' @return list of validation results
#' @seealso Validated
#' @examples
#' validateFastaInputs(isolate(reactiveValuesToList(input)))
validateFastaInputs <- function(inputs, input_ids= c("custom_species_db_file", "custom_background_db_file")){

  results <- list()

  for(input_id in input_ids) {
    fasta_file <- inputs[[input_id]]
    if(!is.null(fasta_file)) {

      valid_res <- validateFastaFiles(fasta_file[["datapath"]])
      if(!is.null(valid_res)){

        if(valid_res) {
          results[[length(results)+1]] <- Validated(input_id,
                                                    valid=TRUE,
                                                    message='Fasta file is OK')
        } else {
          results[[length(results) + 1]] <- Validated(input_id,
                                                      valid=FALSE,
                                                      message='File is not a FASTA File or is empty')
        }
      }
    }
    else{
      results[[length(results) + 1]] <-
        Validated(input_id, 'No file(s) uploaded', TRUE)
    }
  }
  return(results)
}
