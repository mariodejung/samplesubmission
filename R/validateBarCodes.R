#' performs a validation check on the barcodes fields
#' @param inputs isolated input list
#' @param pattern regex pattern to be matched the barcodes
#' @return list of validation results
#' @seealso Validated
#' @examples
#' validateBarCodes(isolate(reactiveValuesToList(input)))
validateBarCodes <- function(inputs, pattern="^IMB_\\w+_\\d{3,5}$") {
  results <- list()

  #barcode_acid
  #barcode_trypsin
  #barcode_protease
  

  if(inputs$barcode_trypsin == '' && inputs$barcode_protease == '') {
    results[[length(results) + 1]] <- Validated("barcode_trypsin",
                                                valid=FALSE,
                                                message='At least one enzyme needed')
    results[[length(results) + 1]] <- Validated("barcode_protease",
                                                valid=FALSE,
                                                message='At least one enzyme needed')
    
  }else{
    for(barcode in c("barcode_acid", "barcode_trypsin", "barcode_protease")){
      if(inputs[barcode]!='' && !grepl(pattern, inputs[barcode])){
        results[[length(results) + 1]] <- Validated(barcode,
                                                    valid=FALSE,
                                                    message='Don\'t meet the required format')
      }
    }
  }

  return(results)
}
