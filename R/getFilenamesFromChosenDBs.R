#' extracts databases file paths on the basis of selected databases in the form
#' @param inputs isolated input list
#' @param available_dbs a dataframe with the DBs informations
#' @return file paths corresponding to the choosen DBs in the form
#' @examples
#' getFilenamesFromChosenDBs(isolate(reactiveValuesToList(input)))
getFilenamesFromChosenDBs <- function(input, available_dbs) {
  file_names <- c()
  if(!is.null(input$species_dbs) && exists("available_dbs")) {
    db_indecies <- (unique(match(input$species_dbs, available_dbs$species_names)))
    file_names <- available_dbs$absolute_path[db_indecies]
    #print(file_names)
  }
  return(file_names)
}




