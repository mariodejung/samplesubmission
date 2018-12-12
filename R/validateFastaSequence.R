#' validates a fasta sequence string
#' @param sequence string containing one or more fasta sequences
#' @return logical, whether the file is a fasta file or NULL if file_path is NULL
#' @import readr
#' @examples
#' validateFastaSequences(seqs)
validateFastaSequences <- function(seqs) {

  if(is.null(seqs) || seqs==''){
    return(TRUE)
  }else{
    tmp=tempfile()
    write_file(seqs, path=tmp)
    return(validateFastaFiles(tmp))
  }
}
