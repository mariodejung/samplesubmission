
#' tries to determine an appropriate regex for Protein ID extraction from fasta headers
#' on the basis of file names 
#' @param fileName one or more(vector) file names
#' @param default_pattern regex wich is used in case of no recognition
#' @return a vector of regex
#' @examples
#' getProtIdRegexFromFileName(isolate(reactiveValuesToList(input)))
getProtIdRegexFromFileName <- function(file_name, default_pattern= ">([^ ]+)") {
  
  db_names_to_patterns <- list(uniprot=">\\w+\\|([^ ]+)\\|",
                               ensemble=">([^ ]+)",
                               ncbi=">([^ ]+)")
  
  sub_patterns <- c()
  
  for(f in file_name){
    sub_pattern <- NULL
    for(db_name in names(db_names_to_patterns)){
      if(grepl(db_name, basename(f), ignore.case=TRUE)){
        sub_pattern <- db_names_to_patterns[[db_name]]
      }
    }
    if(is.null(sub_pattern)) sub_pattern <- default_pattern
    sub_patterns <- c(sub_patterns, sub_pattern)
  }
  return(sub_patterns)
}
