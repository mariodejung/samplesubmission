#' performs a validation check on the barcodes fields
#' @param inputs isolated input list
#' @return list of validation results
#' @seealso Validated
#' @examples
#' validateBarCodes(isolate(reactiveValuesToList(input)))
validateBarCodes <- function(inputs) {
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
  }

  return(results)
}
