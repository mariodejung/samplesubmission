#' tries to extract the protein IDs (located in fasta headers) from fasta file
#' @export
#' @param fasta_file path to a fasta file
#' @param sub_pattern optionally, pattern to match the protein ID
#' @return a character vector of protein IDs
#' @examples
#' extrProtIdsFromFasta("HUMAN_DB.fasta", ">([^ ]+)")
extrProtIdsFromFasta <- function(fasta_file,sub_pattern=NULL){

  if(is.null(sub_pattern)){
    #try to determinate source DB and a respective pattern
    db_names_to_patterns <- list(uniprot=">\\w+\\|([^ ]+)\\|",
                                 ensemble=">([^ ]*)",
                                 ncbi=">([^ ]*)",
                                 default=">([^ ]+)")

    for(db_name in names(db_names_to_patterns)){
      if(grepl(db_name,basename(fasta_file), ignore.case=TRUE)){

        sub_pattern <- db_names_to_patterns[[db_name]]
      }
    }

    if(is.null(sub_pattern)) sub_pattern <- db_names_to_patterns$default
  }


  f <- function(x, pos) x[grepl('^>', x)]


  header_lines <- unique(unlist(
    readr::read_lines_chunked(fasta_file,
                              readr::ListCallback$new(f),
                              chunk_size=500000)))

  sub_pattern <- paste0(sub_pattern, ".*")
  #print(length(header_lines))
  prot_ids <- sub(pattern=sub_pattern, ("\\1"), header_lines)

  #print(prot_ids)

  return(prot_ids)

}



