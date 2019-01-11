#' opens a new browser window or tab where the html will be loaded
#' and tries to print it
#' @export
#' @param path server path to a file
#' @param name_replacement optional, new name for the html file
#' @return html path relative to www directory 
#' @import shinyjs
#' @examples
#' \dontrun{
#' showAndPrintHtmlFile("form.html", "tmp")
#' }
showAndPrintHtmlFile <- function(path, name_replacement=NULL){

  new_dir <- file.path("www", "tmp")
  if(!dir.exists(new_dir)){
    dir.create(new_dir, recursive=TRUE)
  }

  new_path <- new_dir
  if(!is.null(name_replacement) && typeof(name_replacement) == "character"){
    new_path <- file.path(new_path, paste0(name_replacement, ".html"))
  }
  file.copy(path, new_path)
  #print(new_path)
  js_path <- sub(pattern="^www.", "", new_path)
  js$winopenAndPrint(js_path)
  return(js_path)

}
