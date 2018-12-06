
###############################################################################
#
# This script creates a table file with a summury of availible species DBs
#
# developer: Timur Horn
# version: 0.0.1
# date: 2018.16.11
#
#
###############################################################################


library(readr)
library(stringr)

fasta_files_path <- file.path(".")
summary_out_file <- file.path(".","Available_species_DBs.csv")

#find all fasta files

fasta_paths <- list.files(path=fasta_files_path, pattern="\\.fasta$")

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

write.csv2(out_table,"Available_species_DBs.csv",row.names=FALSE)


