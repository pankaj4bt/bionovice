test_that("validate_file works for non-existent files", {
  result <- validate_file("/non/existent/path.txt")
  
  expect_false(result$valid)
  expect_true("File does not exist" %in% result$issues)
  expect_true(is.na(result$file_size))
  expect_false(result$readable)
})

test_that("validate_file works for FASTA files", {
  # Create temporary FASTA file
  temp_file <- tempfile(fileext = ".fasta")
  writeLines(c(">seq1", "ATCGATCG", ">seq2", "GGCCTTAA"), temp_file)
  
  result <- validate_file(temp_file)
  
  expect_true(result$valid)
  expect_equal(result$format, "fasta")
  expect_true(result$readable)
  expect_true(result$file_size > 0)
  expect_equal(length(result$issues), 0)
  
  unlink(temp_file)
})

test_that("validate_file detects invalid FASTA format", {
  # Create invalid FASTA file (no headers)
  temp_file <- tempfile(fileext = ".fasta")
  writeLines(c("ATCGATCG", "GGCCTTAA"), temp_file)
  
  result <- validate_file(temp_file)
  
  expect_false(result$valid)
  expect_true(any(grepl("No FASTA headers", result$issues)))
  
  unlink(temp_file)
})

test_that("validate_file works for FASTQ files", {
  # Create temporary FASTQ file
  temp_file <- tempfile(fileext = ".fastq")
  fastq_content <- c(
    "@read1",
    "ATCGATCG",
    "+",
    "IIIIIIII",
    "@read2", 
    "GGCCTTAA",
    "+",
    "HHHHHHHH"
  )
  writeLines(fastq_content, temp_file)
  
  result <- validate_file(temp_file)
  
  expect_true(result$valid)
  expect_equal(result$format, "fastq")
  expect_equal(length(result$issues), 0)
  
  unlink(temp_file)
})

test_that("validate_file detects invalid FASTQ format", {
  # Create invalid FASTQ file (wrong number of lines)
  temp_file <- tempfile(fileext = ".fastq")
  writeLines(c("@read1", "ATCGATCG", "+"), temp_file)
  
  result <- validate_file(temp_file)
  
  expect_false(result$valid)
  expect_true(any(grepl("4-line format", result$issues)))
  
  unlink(temp_file)
})

test_that("validate_file works for VCF files", {
  # Create temporary VCF file
  temp_file <- tempfile(fileext = ".vcf")
  vcf_content <- c(
    "##fileformat=VCFv4.2",
    "##INFO=<ID=DP,Number=1,Type=Integer,Description=\"Total Depth\">",
    "#CHROM\tPOS\tID\tREF\tALT\tQUAL\tFILTER\tINFO",
    "chr1\t100\t.\tA\tT\t60\tPASS\tDP=30"
  )
  writeLines(vcf_content, temp_file)
  
  result <- validate_file(temp_file)
  
  expect_true(result$valid)
  expect_equal(result$format, "vcf")
  expect_equal(length(result$issues), 0)
  
  unlink(temp_file)
})

test_that("validate_file works for CSV files", {
  # Create temporary CSV file
  temp_file <- tempfile(fileext = ".csv")
  writeLines(c("name,age,city", "John,25,NYC", "Jane,30,LA"), temp_file)
  
  result <- validate_file(temp_file)
  
  expect_true(result$valid)
  expect_equal(result$format, "csv")
  expect_equal(length(result$issues), 0)
  
  unlink(temp_file)
})

test_that("validate_file can use expected_format parameter", {
  # Create CSV file but validate as FASTA
  temp_file <- tempfile(fileext = ".csv")
  writeLines(c("name,age", "John,25"), temp_file)
  
  result <- validate_file(temp_file, expected_format = "fasta")
  
  expect_false(result$valid)
  expect_equal(result$format, "fasta")
  expect_true(any(grepl("No FASTA headers", result$issues)))
  
  unlink(temp_file)
})

test_that("validate_file handles empty files", {
  temp_file <- tempfile(fileext = ".fasta")
  file.create(temp_file)
  
  result <- validate_file(temp_file)
  
  expect_false(result$valid)
  expect_true(any(grepl("empty", result$issues, ignore.case = TRUE)))
  
  unlink(temp_file)
})