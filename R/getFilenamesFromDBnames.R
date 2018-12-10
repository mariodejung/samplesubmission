#' extracts databases file paths on the basis of selected databases in the form
#' @param db_names names of fasta databases
#' @return file paths corresponding to the databases names
#' @examples
#' getFilenamesFromDBnames("Human")
getFilenamesFromDBnames <- function(db_names) {
  file_names <- c()
  if(exists("available_dbs")) {
    db_indecies <- (unique(match(db_names, available_dbs$species_names)))
    file_names <- available_dbs$absolute_path[db_indecies]
    #print(file_names)
  }
  return(file_names)
}




