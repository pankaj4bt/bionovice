test_that("extract_file_metadata works for non-existent files", {
  result <- extract_file_metadata("/non/existent/path.txt")
  
  expect_null(result$file_info)
  expect_null(result$format_info)
  expect_null(result$content_preview)
  expect_null(result$statistics)
})

test_that("extract_file_metadata works for FASTA files", {
  # Create temporary FASTA file
  temp_file <- tempfile(fileext = ".fasta")
  fasta_content <- c(
    ">sequence1 description",
    "ATCGATCGATCG",
    "AAATTTCCCGGG",
    ">sequence2", 
    "GGCCTTAAGGCC"
  )
  writeLines(fasta_content, temp_file)
  
  result <- extract_file_metadata(temp_file)
  
  # Check basic file info
  expect_false(is.null(result$file_info))
  expect_true(result$file_info$size_bytes > 0)
  expect_true(result$file_info$readable)
  expect_true(grepl("bytes|KB|MB|GB", result$file_info$size_human))
  
  # Check format info
  expect_equal(result$format_info$extension, "fasta")
  expect_equal(result$format_info$detected_format, "fasta")
  expect_equal(result$format_info$num_sequences, 2)
  
  # Check sequence metadata
  expect_true(!is.null(result$format_info$sequence_lengths))
  expect_equal(result$format_info$sequence_lengths$min, 12)
  expect_equal(result$format_info$sequence_lengths$max, 24)
  
  # Check content preview
  expect_false(is.null(result$content_preview))
  expect_true(length(result$content_preview) <= 10)
  
  # Check statistics
  expect_false(is.null(result$statistics))
  expect_equal(result$statistics$total_lines, 5)
  
  unlink(temp_file)
})

test_that("extract_file_metadata works for FASTQ files", {
  # Create temporary FASTQ file
  temp_file <- tempfile(fileext = ".fastq")
  fastq_content <- c(
    "@read1",
    "ATCGATCGATCG",
    "+",
    "IIIIIIIIIIII",
    "@read2",
    "GGCCTTAAGGCC", 
    "+",
    "HHHHHHHHHHHH"
  )
  writeLines(fastq_content, temp_file)
  
  result <- extract_file_metadata(temp_file)
  
  # Check format info
  expect_equal(result$format_info$extension, "fastq")
  expect_equal(result$format_info$num_reads_sampled, 2)
  
  # Check read length info
  expect_false(is.null(result$format_info$read_lengths))
  expect_equal(result$format_info$read_lengths$min, 12)
  expect_equal(result$format_info$read_lengths$max, 12)
  expect_true(result$format_info$read_lengths$all_same)
  
  # Check quality info
  expect_false(is.null(result$format_info$quality_encoding))
  expect_false(is.null(result$format_info$quality_range))
  
  unlink(temp_file)
})

test_that("extract_file_metadata works for VCF files", {
  # Create temporary VCF file
  temp_file <- tempfile(fileext = ".vcf")
  vcf_content <- c(
    "##fileformat=VCFv4.2",
    "##INFO=<ID=DP,Number=1,Type=Integer,Description=\"Total Depth\">",
    "##INFO=<ID=AF,Number=A,Type=Float,Description=\"Allele Frequency\">",
    "##FORMAT=<ID=GT,Number=1,Type=String,Description=\"Genotype\">",
    "#CHROM\tPOS\tID\tREF\tALT\tQUAL\tFILTER\tINFO\tFORMAT\tsample1\tsample2",
    "chr1\t100\t.\tA\tT\t60\tPASS\tDP=30\tGT\t0/1\t1/1"
  )
  writeLines(vcf_content, temp_file)
  
  result <- extract_file_metadata(temp_file)
  
  # Check format info
  expect_equal(result$format_info$extension, "vcf")
  expect_equal(result$format_info$vcf_version, "v4.2")
  expect_equal(result$format_info$num_info_fields, 2)
  expect_equal(result$format_info$num_format_fields, 1)
  expect_equal(result$format_info$num_samples, 2)
  
  unlink(temp_file)
})

test_that("extract_file_metadata handles different preview options", {
  # Create temporary file
  temp_file <- tempfile(fileext = ".txt")
  content <- paste("Line", 1:20)
  writeLines(content, temp_file)
  
  # Test with preview
  result_with_preview <- extract_file_metadata(temp_file, include_preview = TRUE, max_preview_lines = 5)
  expect_equal(length(result_with_preview$content_preview), 5)
  
  # Test without preview
  result_no_preview <- extract_file_metadata(temp_file, include_preview = FALSE)
  expect_null(result_no_preview$content_preview)
  
  unlink(temp_file)
})

test_that("sequence type detection works correctly", {
  # Test DNA
  temp_file <- tempfile(fileext = ".fasta")
  writeLines(c(">dna_seq", "ATCGATCGATCG"), temp_file)
  result <- extract_file_metadata(temp_file)
  expect_equal(result$format_info$sequence_type, "DNA")
  unlink(temp_file)
  
  # Test RNA  
  temp_file <- tempfile(fileext = ".fasta")
  writeLines(c(">rna_seq", "AUCGAUCGAUCG"), temp_file)
  result <- extract_file_metadata(temp_file)
  expect_equal(result$format_info$sequence_type, "RNA")
  unlink(temp_file)
  
  # Test Protein
  temp_file <- tempfile(fileext = ".fasta")
  writeLines(c(">protein_seq", "MKTVRQERLKS"), temp_file)
  result <- extract_file_metadata(temp_file)
  expect_equal(result$format_info$sequence_type, "Protein")
  unlink(temp_file)
})

test_that("file size formatting works correctly", {
  # This tests the internal format_file_size function through the main function
  temp_file <- tempfile(fileext = ".txt")
  
  # Create small file
  writeLines("test", temp_file)
  result <- extract_file_metadata(temp_file)
  expect_true(grepl("bytes", result$file_info$size_human))
  
  unlink(temp_file)
})

test_that("compression detection works", {
  # Test gzip detection
  temp_file <- tempfile(fileext = ".fasta.gz")
  file.create(temp_file)
  result <- extract_file_metadata(temp_file)
  expect_equal(result$format_info$compression, "gzip")
  unlink(temp_file)
  
  # Test no compression
  temp_file <- tempfile(fileext = ".fasta")
  file.create(temp_file)
  result <- extract_file_metadata(temp_file)
  expect_equal(result$format_info$compression, "none")
  unlink(temp_file)
})