% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/createSummaryFileOfFastaFiles.R
\name{createSummaryFileOfFastaFiles}
\alias{createSummaryFileOfFastaFiles}
\title{Creates a csv file with containing a summary of all fasta files found in the directory
tries to extract species names from the file names}
\usage{
createSummaryFileOfFastaFiles(fasta_files_path = ".",
  out_file = "Available_species_DBs.csv",
  file_name_pattern = "\\\\.fasta$")
}
\arguments{
\item{fasta_files_path}{directory path vector for searching the fasta files}

\item{out_file}{file name (or path) for the summary output file. If NULL no file will be written}

\item{file_name_pattern}{file name pattern to filter the file names}
}
\value{
data frame with the summary
}
\description{
Creates a csv file with containing a summary of all fasta files found in the directory
tries to extract species names from the file names
}
\examples{
\dontrun{
createSummaryFileOfFastaFiles()
createSummaryFileOfFastaFiles("fasta_dir", "summary.csv", "(HUMAN|YEAST).*\\\\.fasta$")
}
}
