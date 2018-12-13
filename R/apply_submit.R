#' Perform a try to submit the filled form
#' contains validation und writing the datas if validation is valid
#' @param input inputs passed-through server function
#' @param output output passed-through server function
#' @param output_dir directory path, where all orders are in
#' @examples
#' \dontrun{
#' apply_submit(input, output, "orders_dir")
#' }
apply_submit <- function(input,output, output_dir){

  allInputs <- isolate(reactiveValuesToList(input))
  
  #create output directory name
  directory_name <- format(Sys.time(), "%Y%m%d_%H%M%S")
  directory_name <-
    paste(directory_name, allInputs$firstname, allInputs$surname, allInputs$group,
          sep="_")
  directory_name <- gsub("\\s+", "_", directory_name)
  
  barcodes <- paste0(allInputs[grep("barcode", names(allInputs))],
                     collapse="_", sep="_")
  directory_name <- paste0(directory_name, barcodes, collapse="_", sep="_")
  
  out_directory <- file.path(output_dir, directory_name)

  
  allInputs$timestamp <- directory_name

  output$message <- renderText("Validating user inputs...")

  validation_results <- validateInputValues(allInputs)

  result_valid=sapply(validation_results, function(result) result$valid)

  if(all(result_valid)) {

    submitCompletedForm(allInputs, out_directory)
    output$message <- renderText("Validation succeeded!")

  } else {
    output$message <-
      renderText(paste(collapse="\n",
                       sapply(validation_results[!result_valid],
                              function(result) {
                                sprintf("%s: %s",
                                        result$input_element_id, result$message)
                              })))


    removeAllRequiredMarks()
    sapply(validation_results[!result_valid],
           function(result) {
             markRequiredInput(result$input_element_id, result$message)
           })

  }

}
