#' gather all necessary inputs and uploaded files,
#' enriches them if necessary and puts them in the output dir
#' @param allInputs all isolated inputs
#' @param out_directory directory where the outputs well be written to
#' @return file path to the completed form html and the corresponding web path
#' @import yaml
#' @import png
#' @import pdftools
#' @import rmarkdown
#' @examples
#' \dontrun{
#' submitCompletedForm(isolate(reactiveValuesToList(input), "custormer_XY_date_AB")
#' }
submitCompletedForm <- function(allInputs, out_directory) {
  if(!dir.exists(out_directory)) dir.create(out_directory, recursive=TRUE)

  user_inputs_file_name <- get("user_inputs_file_name")
  
# Copy uploaded image -----------------------------------------------------
  if(!is.null(allInputs$gel_picture)) {
    allInputs$gel_picture$new_datapath <-
      normalizePath(file.path(out_directory,
                              paste("Gel_picture",
                                    sub(".+\\.(.+)", "\\1", allInputs$gel_picture$name),
                                    sep=".")))
    file.copy(allInputs$gel_picture$datapath, allInputs$gel_picture$new_datapath)
    #extract fist page from pdf as image
    if(grepl("\\.pdf$", allInputs$gel_picture$new_datapath)){
      img <- pdf_render_page(allInputs$gel_picture$new_datapath, page = 1, dpi= 144)
      allInputs$gel_picture$new_datapath <- sub("pdf$", "png",
                                                allInputs$gel_picture$new_datapath)
      png::writePNG(img, allInputs$gel_picture$new_datapath)
    }
    #TODO perhaps magick package should be used to cover most image formates
    if(grepl("\\.tif+$", allInputs$gel_picture$new_datapath)){
      img <- readTIFF(allInputs$gel_picture$new_datapath, native=TRUE)
      allInputs$gel_picture$new_datapath <- sub("tif+$", "jpeg",
                                                allInputs$gel_picture$new_datapath)
      jpeg::writeJPEG(img, allInputs$gel_picture$new_datapath, quality=0.8)
    }
  }
  
# Copy custom DBs, will not used--------------------------------------------------

  db_out_directory <- file.path(out_directory, "Custom_DB")
  if(!dir.exists(db_out_directory)) dir.create(db_out_directory)
  if(!is.null(allInputs$custom_species_db_file)) {

    allInputs$custom_species_db_file$new_datapath <-
      normalizePath(file.path(db_out_directory,
                              allInputs$custom_species_db_file$name))
    file.copy(allInputs$custom_species_db_file$datapath,
              allInputs$custom_species_db_file$new_datapath)
  }
  
  if(!is.null(allInputs$custom_background_db_file)) {
    db_out_directory <- file.path(out_directory, "Custom_BG_DB")
    if(!dir.exists(db_out_directory)) dir.create(db_out_directory)
    allInputs$custom_background_db_file$new_datapath <-
      normalizePath(file.path(db_out_directory,
                              allInputs$custom_background_db_file$name))
    file.copy(allInputs$custom_background_db_file$datapath,
              allInputs$custom_background_db_file$new_datapath)
  }
  
# Custom sequence processing ----------------------------------------------

  if(allInputs$fasta_sequence!='') {
    fasta_sequence <- normalizeFastaSeq(allInputs$fasta_sequence)
    ##TODO check content (fasta format)
    path <- normalizePath(file.path(db_out_directory, "custom_seq.fasta"))

    print("faste seq write....")
    write(fasta_sequence, file=path)
    print("faste seq written")
    allInputs$fasta_sequence <- list(content="Custom Sequence(s) uploaded",
                                     path=path)
    allInputs$target_protein <- c(allInputs$target_protein,
                                  extrProtIdsFromFasta(path))
    
  }else {
    allInputs$fasta_sequence <- list(content="No custom sequence(s) uploaded",
                                     path=NULL)
  }
  
# databases summary -------------------------------------------------------

  allInputs$species_dbs <- list(names=allInputs$species_dbs,
                                paths=samplesubmission:::getFilenamesFromDBnames(allInputs$species_dbs))
  allInputs$background_dbs <- list(names=allInputs$background_dbs,
                                paths=samplesubmission:::getFilenamesFromDBnames(allInputs$background_dbs))

# enzyme barcode summary ---------------------------------------------------------
 
  allInputs$barcodes <- samplesubmission:::summarizeBarcodes(allInputs)
  
# write file --------------------------------------------------------------

  yaml::write_yaml(allInputs,file.path(out_directory, user_inputs_file_name))

  form_output_file <- file.path(out_directory, "CompletedForm.html")

  rmarkdown::render(input="BandIdent_CompletedForm.Rmd",
                    knit_root_dir=out_directory, output_dir=out_directory,
                    output_file=form_output_file)

  web_path <- showAndPrintHtmlFile(form_output_file, allInputs$timestamp)
  return(c(form_output_file, web_path) )
}
