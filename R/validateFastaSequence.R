#' validates a fasta sequence string
#' @param seqs string containing one or more fasta sequences
#' @return logical, whether the file is a fasta file or NULL if file_path is NULL
#' @import readr
#' @export
#' @examples
#' validateFastaSequences(">Protein ID with spaces \n KDFSHGZWVBSZWHA  ADFHDJF\n")
validateFastaSequences <- function(seqs) {

  if(is.null(seqs) || seqs==''){
    return(TRUE)
  }else{
    tmp=tempfile()
    write_file(seqs, path=tmp)
    return(validateFastaFiles(tmp))
  }
}
