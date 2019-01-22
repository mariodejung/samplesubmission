context("Fasta sequence functions")
library(readr)

test_that("validateFastaSequences", {
  expect_equal(validateFastaSequences(""), TRUE)
  expect_equal(validateFastaSequences("ab"), FALSE)
  expect_equal(validateFastaSequences(">ABC\nasdfadf"), TRUE)
  expect_equal(validateFastaSequences(">ABC\nasdfadf\n>CBA"), TRUE)
  expect_equal(validateFastaSequences("ABC\nasdfadf\n>CBA"), TRUE)
})

test_that("normalize Fasta Sequence", {
  expect_equal(normalizeFastaSeq(">Protein ID with spaces \n KDFSHGZWVBSZWHA  ADFHDJF\n"),
                                 ">Protein_ID_with_spaces_\nKDFSHGZWVBSZWHAADFHDJF\n")
  expect_equal(normalizeFastaSeq(">ProteinID_1\n LLLMSKK "),
                                 ">ProteinID_1\nLLLMSKK\n")
})

test_that("Protein ID extraction", {
  f <- tempfile()
  write_file(">ID1\nasdakdf\n>ID2\nasdff", path=f)
  expect_equal(extrProtIdsFromFasta(f), c("ID1", "ID2"))
  write_file(">ID3_4 FUNC1\nasdakdf\n>ID5|FUNC1\nasdff", path=f)
  expect_equal(extrProtIdsFromFasta(f), c("ID3_4", "ID5|FUNC1"))
})



