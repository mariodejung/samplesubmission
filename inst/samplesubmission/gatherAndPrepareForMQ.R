
###############################################################################
#
# Script for automatic summary and preparation of a band identification order 
#
# Searches for raw files with a determined minimal size and age range, and for ther
# appropriate approved submissions. If all files for a approved submission are
# available they will be sumarized (moved/copied) into a new directory.  
# Additionally the script creates the configuration files for MaxQuant containing 
# all neccessary settings
#
# developer: Timur Horn
# version: 0.1.0
# date: 2019-01-10
#
# 
###############################################################################


library(stringr)
library(readr)
library(samplesubmission)

submissions_dir <- file.path("outputs")
raw_files_dir <- file.path("Y:", "CFP", "2018")
summary_dir <- file.path("MQ")
mqpar_template_path <- "mqpar.tmp.xml"

flag_file_name <- "approved.txt"

raw_file_age <- 30 # in days
file_size_filter <- 5000000 # in bytes
barcode_extract_pattern <- "[A-Z]{3}_[A-Z]{2}_\\d{3,5}"


current_time = Sys.time()

# search for raw files ----------------------------------------------------

raw_files <- list.files(raw_files_dir,
                        pattern=paste0(barcode_extract_pattern, ".*\\.raw$"),
                        ignore.case=TRUE, full.names=TRUE)
#raw_files_info <- file.info(raw_files, extra_cols=FALSE)
#filter files
indices <- which(file.size(raw_files)>file_size_filter &
                   difftime(current_time, file.mtime(raw_files), units="days") < raw_file_age)
raw_files <- raw_files[indices]
bar_codes <- str_extract(raw_files, barcode_extract_pattern)
indices <- which(!is.na(bar_codes))
bar_codes <- bar_codes[indices]
raw_files <- raw_files[indices]

# search for submission_dirs --------------------------------------------------
for(barcode in bar_codes){
  submission_dir <- list.files(path=submissions_dir, pattern=barcode, full.names=TRUE)
  submission_dir <- submission_dir[file.exists(file.path(submission_dir, flag_file_name))]
  if(length(submission_dir)<1){
    print(sprintf("No approved submission_dir for barcode %s found", barcode))
  }else if(length(submission_dir)>1){
    warning(sprintf("Multiple approved submission directories for barcode %s found", barcode))
  }else{
    print(sprintf("Barcode %s correspond to %s", barcode, submission_dir))
    subm_info <- read_yaml(file.path(submission_dir, "user_inputs.yaml"))
    subm_barcodes <- samplesubmission:::summarizeBarcodes(subm_info)
    
    if(all(subm_barcodes %in% bar_codes)){
      indices <- which(bar_codes %in% subm_barcodes)
      subm_raw_files <- raw_files[indices]
      #create summary dir
      subm_summary_dir <- file.path(summary_dir, basename(submission_dir))
      dir.create(subm_summary_dir, recursive=TRUE)
      print(sprintf("Copying %s to %s", submission_dir, subm_summary_dir))
      if(all(file.copy(submission_dir, subm_summary_dir, recursive=TRUE))){
        #unlink(submission_dir, recursive=TRUE)
        file.rename(file.path(submission_dir, flag_file_name),
                    file.path(submission_dir, paste(flag_file_name, "done", sep="_") ))
        dir_name <- basename(submission_dir)
        file.rename(file.path(subm_summary_dir, dir_name),
                    file.path(subm_summary_dir, "submission") )
      }
      file.copy(subm_raw_files, subm_summary_dir)
      enzymes <- names(subm_barcodes)
      # create mqpars -----------------------------------------------------------
      #gather fasta files
      fastas_in_subm <- list.files(subm_summary_dir, "\\.fasta$",
                                   recursive=TRUE,
                                   full.names=TRUE)
      fasta_paths <- unique(c(subm_info$species_dbs$paths,
                              subm_info$background_dbs$paths,
                              #subm_info$custom_background_db_file,
                              #subm_info$custom_species_db_file,
                              fastas_in_subm))
      parameterGroups <- list()
      raw_fs <- c()
      for(enzyme in enzymes[-grep("Acid", enzymes)]){
        raw_fs <- c(raw_fs, list.files(subm_summary_dir,
                                       paste0(subm_barcodes[[enzyme]], ".*\\.raw"),
                                       full.names=TRUE))
        parameterGroups[length(parameterGroups)+1] <- list(
          list(mode=0, enzyme=c(enzyme))
        )
      }
      #create protease(s) mqpar
      writeLines(fillMqparTemplate(parameterGroups,
                                    fasta_paths,
                                    raw_fs,
                                    fixedFolder="proteases"),
                  file.path(subm_summary_dir,"mqpar__proteases.xml"))
      
      if("Acid" %in% enzymes){
        fasta_paths <- normalizePath(file.path(subm_summary_dir,
                                               "detected_sequences.fasta"))
        raw_fs <- c(raw_fs, list.files(subm_summary_dir,
                                       paste0(subm_barcodes[["Acid"]], ".*\\.raw"),
                                       full.names=TRUE))
        
        parameterGroups[length(parameterGroups)+1] <- list(
          list(mode=4, enzyme=NULL)
        )
        writeLines(fillMqparTemplate(parameterGroups,
                                      fasta_paths,
                                      raw_fs,
                                      fixedFolder="Acid_Digestion"),
                    file.path(subm_summary_dir, "mqpar_acid.xml"))
      }
      
    }else{
      warning(sprintf("Cannot find all raw files for $s", subm_info$timestamp))
    }
  }
  
}
