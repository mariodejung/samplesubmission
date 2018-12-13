#' Creates a csv file with containing a summary of all fasta files found in the directory
#' tries to extract species names from the file names
#' @export
#' @param fasta_files_path directory path vector for searching the fasta files
#' @param out_file file name (or path) for the summary output file. If NULL no file will be written
#' @param file_name_pattern file name pattern to filter the file names 
#' @return data frame with the summary
#' @import readr
#' @import stringr
#' @importFrom  utils write.csv2
#' @examples 
#' \dontrun{
#' createSummaryFileOfFastaFiles()
#' createSummaryFileOfFastaFiles("fasta_dir", "summary.csv", "(HUMAN|YEAST).*\\.fasta$")
#' }
createSummaryFileOfFastaFiles <- function (fasta_files_path=".",
                                           out_file="Available_species_DBs.csv",
                                           file_name_pattern="\\.fasta$"){
  
  # print(fasta_files_path)
  # print(out_file)
  # print(file_name_pattern)
  
  # fasta_files_path <- file.path("E:","Fasta_UNIPROT")
  # out_file <- "Available_species_DBs.csv"
  # file_name_pattern <- "Daph.*Uniprot.*fasta$"
  
  fasta_paths <- list.files(path=fasta_files_path,
                            pattern=file_name_pattern,
                            full.names= TRUE)
  
  prot_count <- c()
  prot_names <- c()
  
  species_names <- sub(pattern="([A-Za-z]+)_([A-Za-z]+)_(\\([A-Za-z]+\\))?.*",
                       replacement="\\1 \\2 \\3",
                       basename(fasta_paths))
  
  species_names <- paste(species_names,
                         stringr::str_extract(basename(fasta_paths), "(?<=strain_)\\w+"),
                         sep=" ", collapse=NULL)
  
  #remove "NA"
  species_names <- sub(pattern="\\s*NA$","",species_names)
  
  lat_names = sub("^(\\w)\\w+\\s+([A-Za-z]+).*", "\\1.\\2", species_names)
  
  f <- function(x, pos) length(x[grepl('^>', x)])
  
  for(fasta_file in fasta_paths){
    
    
    prot_count <- c(prot_count,sum(unlist(
      readr::read_lines_chunked(fasta_file,
                                readr::ListCallback$new(f),
                                chunk_size=50000))))
    
    # prot_names <- readr::read_lines_chunked(fasta_file, 
    #                             readr::DataFrameCallback$new(f), 
    #                             chunk_size=50000)
    # prot_count <- c(prot_count, length(prot_names))
    
  }
  
  out_table <- data.frame(species_names,
                          lat_names,
                          absolute_path=normalizePath(fasta_paths),
                          fasta_paths,
                          prot_count)
  if(!is.null(out_file)){
    write.csv2(out_table, out_file, row.names=FALSE)
  }
  
  return(out_table)
}