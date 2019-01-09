#' summarize the barcodes into one named list
#' names are the anzyme names, values are the barcodes respectively 
#' @param inputs isolated input list
#' @examples
#' \dontrun{
#' summarizeBarcodes(isolate(reactiveValuesToList(input)))
#' }
summarizeBarcodes <- function(allInputs){

  enzymes = c()
  barcodes= c()
  if(allInputs$barcode_protease!=''){
    enzymes <- allInputs$name_protease
    barcodes <- allInputs$barcode_protease
  } 
  
  if(allInputs$barcode_trypsin!=''){
    enzymes <- c(enzymes, "Trypsin/P")
    barcodes <- c(barcodes, allInputs$barcode_trypsin)
  } 
  if(allInputs$barcode_acid!=''){
    enzymes <- c(enzymes, "Acid")
    barcodes <- c(barcodes, allInputs$barcode_acid)
  } 
  
  barcodes <- as.list(barcodes)
  names(barcodes) <- enzymes
  return(barcodes)

}
