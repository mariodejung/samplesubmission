
# devtools::document(); shiny::runApp('inst/samplesubmission')

server <- function(input, output, session) {

  observeEvent(input$submit,{
    samplesubmission:::apply_submit(input, output, orders_directory)

  })

  observeEvent(input$testMark,{
    markRequiredInput("surname", "Bitte ausfüllen")

  })

  #I have no idea why "paste" works here. Observe several inputs ##Mario looks dirty but you paste together all inputs into a single string. This string is changing ALLWAYS when any of the inputs change!
  observeEvent({paste(input$species_dbs, input$custom_species_db_file)},
               {
                 #print(typeof(input$custom_species_db_file))
                 file_names <- samplesubmission:::getFilenamesFromDBnames(input$species_dbs)

                 if(!is.null(input$custom_species_db_file)){
                   file_names <- c(file_names, input$custom_species_db_file[["datapath"]])
                 }

                 prot_ids <- unlist(sapply(file_names, function(f){
                   extrProtIdsFromFasta(f)
                 }, USE.NAMES=FALSE))
                 #print(str(prot_ids))
                 target_proteins <- isolate(input$target_protein)
                 #print(target_proteins)
                 shiny::updateSelectizeInput(session, inputId="target_protein",
                                             choices=prot_ids, server=TRUE,
                                             selected=target_proteins)

               })

  observeEvent({input$name_protease},
               {

                 if(input$name_protease == "undefined"){
                   disable("barcode_protease")
                   updateTextInput(session,"barcode_protease",value="")
                 } else {
                   enable("barcode_protease")
                 }

               })
  
}
