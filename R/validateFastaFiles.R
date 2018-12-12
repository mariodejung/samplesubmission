#' validates whether the files are valid fasta files
#' uses read.fasta from seqinr to do that
#' @export
#' @param file_path fasta file paths
#' @return logical, whether the file is a fasta file or NULL if file_path is NULL
#' @import seqinr
#' @examples
#' validateFastaFiles("fasta_DB.fasta")
validateFastaFiles <- function(file_path) {

  if(!is.null(file_path)){

    result <- tryCatch({
      fastaDB <- lapply(file_path,
                        function(x) read.fasta(file=x, as.string=T, seqtype='AA'))
      TRUE
    },
    error=function(cond) {

      return(FALSE)
    },
    warning=function(cond) {

      return(FALSE)
    })

    return(result)

  }
  else{
    return(NULL)
  }
}
