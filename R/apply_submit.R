#' Perform a try to submit the filled form
#' contains validation und writing the datas if validation is valid
#' @param input inputs passed-through server function
#' @param output output passed-through server function
#' @param output_dir directory path, where all orders are in
#' @examples
#' apply_submit(input, output, "orders_dir")
apply_submit <- function(input,output, output_dir){

  #create output directory name
  directory_name <- format(Sys.time(), "%Y%m%d_%H%M%S")
  directory_name <-
    paste(directory_name, input$firstname, input$surname, input$group,
          sep="_")
  directory_name <- gsub("\\s+", "_", directory_name)
  out_directory <- file.path(output_dir, directory_name)

  allInputs <- isolate(reactiveValuesToList(input))
  allInputs$timestamp <- directory_name

  output$message <- renderText("Validate user inputs...")

  validation_results <- validateInputValues(allInputs)

  result_valid=sapply(validation_results, function(result) result$valid)

  if(all(result_valid)) {

    submitCompletedForm(allInputs, out_directory)

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
