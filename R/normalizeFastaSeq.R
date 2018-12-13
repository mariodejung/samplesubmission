#' replaces whitespaces in the headers
#' replaces whitespaces with empty string in other lines
#' @param sequence string containing sequences in fasta format
#' @return modificated fasta string
#' @export
#' @examples
#' normalizeFastaSeq(">Protein ID with spaces \n KDFSHGZWVBSZWHA  ADFHDJF\n")

normalizeFastaSeq <- function(sequence) {
  normalizedFatsta <- paste0(sapply(unlist(strsplit(sequence, "\n+")), function(line){
     if(startsWith(line, ">")) return(gsub("\\s+", "_", line))
      return(gsub("\\s+", "", line))
   }), collapse="\n")
  return(normalizedFatsta)
}

