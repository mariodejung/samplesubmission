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
    paste(directory_name,
          #allInputs$firstname,
          #allInputs$surname,
          allInputs$group,
          sep="_")
  directory_name <- gsub("\\s+", "_", directory_name)
  
  barcodes <- paste(allInputs[grep("barcode", names(allInputs))], collapse="_", sep="")
  directory_name <- paste(directory_name, barcodes, sep="_")
  
  out_directory <- file.path(output_dir, directory_name)

  
  allInputs$timestamp <- directory_name

  output$message <- renderText("Validating user inputs...")

  validation_results <- samplesubmission:::validateInputValues(allInputs)

  result_valid=sapply(validation_results, function(result) result$valid)

  if(all(result_valid)) {

    web_path <- samplesubmission:::submitCompletedForm(allInputs, out_directory)[2]
    output$message <- renderText(paste0("Validation succeeded. ",
                                        "Please accept popups to view the document",
                                        collapse="\n", sep="\n"))
    output$link_placeholder <- renderUI(tagList(shiny::tags$a(href=web_path,
                                                              "Show Form",
                                                              class="button",
                                                              target="_blank",
                                                              class="btn btn-default shiny-bound-input")))
    if(getOption("samplesubmission.send_email", FALSE)){
      mailR::send.mail(from="\"CF Proteomics\" <proteomics@imb.de>",
                       to=c(allInputs$email),
                       subject=sprintf("Band Identification: $s", allInputs$timestemp),
                       body='check attached files\n',
                       smtp=list(from="\"CF Proteomics\" <proteomics@imb.de>",
                                 host.name="mailgate.zdv.uni-mainz.de",
                                 port=25),
                       authenticate=FALSE,
                       attach.files=file.path("www", web_path),
                       send=TRUE)
    }
    
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
