#' tries to extract the protein IDs (located in fasta headers) from fasta file
#' @export
#' @param fasta_file path to a fasta file
#' @param sub_pattern optionally, pattern to match the protein ID
#' @return a character vector of protein IDs
#' @importFrom readr read_lines_chunked
#' @examples
#' \dontrun{
#' extrProtIdsFromFasta("HUMAN_DB.fasta", ">([^ ]+)")
#' }
extrProtIdsFromFasta <- function(fasta_file, sub_pattern=NULL){

  if(is.na(fasta_file) || is.null(fasta_file)) return(NULL)
  
  if(is.null(sub_pattern)){
    #try to determinate a appropriate pattern
    sub_pattern <- getProtIdRegexFromFileName(fasta_file)
  }

  f <- function(x, pos) x[grepl('^>', x)]

  header_lines <- unique(unlist(
    readr::read_lines_chunked(fasta_file,
                              readr::ListCallback$new(f),
                              chunk_size=500000)))

  sub_pattern <- paste0(sub_pattern, ".*")
  #print(length(header_lines))
  prot_ids <- sub(pattern=sub_pattern, ("\\1"), header_lines)

  return(prot_ids)

}



