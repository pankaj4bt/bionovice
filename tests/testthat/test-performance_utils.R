test_that("analyze_folder_parallel works with sequential processing", {
  # Create temporary test directory
  temp_dir <- tempdir()
  test_dir <- file.path(temp_dir, "parallel_test")
  dir.create(test_dir, showWarnings = FALSE)
  
  # Create test files
  test_files <- c("test1.fasta", "test2.fastq", "test3.vcf")
  for (file in test_files) {
    file.create(file.path(test_dir, file))
  }
  
  # Test sequential processing
  result <- analyze_folder_parallel(test_dir, parallel = FALSE)
  
  # Check structure
  expect_s3_class(result, "data.frame")
  expect_true("FileName" %in% names(result))
  expect_true("Extension" %in% names(result))
  expect_true("Recommendation" %in% names(result))
  expect_equal(nrow(result), 3)
  
  # Check that all files are processed
  expect_true(all(test_files %in% result$FileName))
  
  # Check summary attributes
  expect_false(is.null(attr(result, "summary")))
  expect_equal(attr(result, "summary")$total_files, 3)
  expect_equal(attr(result, "summary")$processing_mode, "sequential")
  
  unlink(test_dir, recursive = TRUE)
})

test_that("analyze_folder_parallel works with metadata inclusion", {
  temp_dir <- tempdir()
  test_dir <- file.path(temp_dir, "metadata_test")
  dir.create(test_dir, showWarnings = FALSE)
  
  # Create a FASTA file with content
  fasta_file <- file.path(test_dir, "test.fasta")
  writeLines(c(">seq1", "ATCG", ">seq2", "GGCC"), fasta_file)
  
  result <- analyze_folder_parallel(test_dir, 
                                   parallel = FALSE, 
                                   include_metadata = TRUE)
  
  # Check that metadata columns are present
  expect_true("FileSize" %in% names(result))
  expect_true("Valid" %in% names(result))
  expect_true("Modified" %in% names(result))
  
  # Check FASTA-specific metadata
  expect_true("NumSequences" %in% names(result))
  expect_equal(result$NumSequences[1], 2)
  
  unlink(test_dir, recursive = TRUE)
})

test_that("analyze_folder_parallel handles recursive scanning", {
  temp_dir <- tempdir()
  test_dir <- file.path(temp_dir, "recursive_test")
  sub_dir <- file.path(test_dir, "subdir")
  dir.create(test_dir, showWarnings = FALSE)
  dir.create(sub_dir, showWarnings = FALSE)
  
  # Create files in main and subdirectory
  file.create(file.path(test_dir, "main.fasta"))
  file.create(file.path(sub_dir, "sub.fastq"))
  
  # Test non-recursive (should find 1 file)
  result_non_recursive <- analyze_folder_parallel(test_dir, recursive = FALSE)
  expect_equal(nrow(result_non_recursive), 1)
  
  # Test recursive (should find 2 files)
  result_recursive <- analyze_folder_parallel(test_dir, recursive = TRUE)
  expect_equal(nrow(result_recursive), 2)
  
  unlink(test_dir, recursive = TRUE)
})

test_that("analyze_folder_parallel handles file pattern filtering", {
  temp_dir <- tempdir()
  test_dir <- file.path(temp_dir, "pattern_test")
  dir.create(test_dir, showWarnings = FALSE)
  
  # Create files with different extensions
  files <- c("data.fastq", "data.fasta", "data.txt", "reads.fq")
  for (file in files) {
    file.create(file.path(test_dir, file))
  }
  
  # Test pattern filtering for FASTQ files
  result <- analyze_folder_parallel(test_dir, 
                                   file_pattern = "\\.(fastq|fq)$")
  
  expect_equal(nrow(result), 2)
  expect_true(all(grepl("\\.(fastq|fq)$", result$FileName)))
  
  unlink(test_dir, recursive = TRUE)
})

test_that("analyze_folder_parallel handles empty folders", {
  temp_dir <- tempdir()
  empty_dir <- file.path(temp_dir, "empty_parallel")
  dir.create(empty_dir, showWarnings = FALSE)
  
  expect_warning(result <- analyze_folder_parallel(empty_dir), "No files found")
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 0)
  expect_true("FileName" %in% names(result))
  
  unlink(empty_dir, recursive = TRUE)
})

test_that("analyze_folder_parallel handles non-existent folders", {
  non_existent <- file.path(tempdir(), "does_not_exist_parallel")
  expect_error(analyze_folder_parallel(non_existent), 
               "The specified folder does not exist")
})

test_that("extract_quick_stats works for different formats", {
  # Test FASTA quick stats
  temp_file <- tempfile(fileext = ".fasta")
  writeLines(c(">seq1", "ATCG", ">seq2", "GGCC"), temp_file)
  
  stats <- extract_quick_stats(temp_file, "fasta")
  expect_equal(stats$NumSequences, 2)
  expect_equal(stats$SequenceType, "DNA")
  
  unlink(temp_file)
  
  # Test FASTQ quick stats
  temp_file <- tempfile(fileext = ".fastq")
  writeLines(c("@read1", "ATCG", "+", "IIII", "@read2", "GGCC", "+", "HHHH"), temp_file)
  
  stats <- extract_quick_stats(temp_file, "fastq")
  expect_equal(stats$NumReads, 2)
  expect_equal(stats$ReadLength, 4)
  
  unlink(temp_file)
})

test_that("detect_sequence_type_quick works correctly", {
  expect_equal(detect_sequence_type_quick("ATCGATCG"), "DNA")
  expect_equal(detect_sequence_type_quick("AUCGAUCG"), "RNA")
  expect_equal(detect_sequence_type_quick("MKTVRQER"), "Protein")
  expect_equal(detect_sequence_type_quick(""), "unknown")
  expect_equal(detect_sequence_type_quick(NA), "unknown")
})

test_that("benchmark_analysis works with valid folder", {
  temp_dir <- tempdir()
  test_dir <- file.path(temp_dir, "benchmark_test")
  dir.create(test_dir, showWarnings = FALSE)
  
  # Create some test files
  files <- c("test1.fasta", "test2.fastq")
  for (file in files) {
    file.create(file.path(test_dir, file))
  }
  
  # Test benchmark with sequential only (to avoid parallel overhead in tests)
  result <- benchmark_analysis(test_dir, methods = "sequential")
  
  expect_s3_class(result, "data.frame")
  expect_true("Method" %in% names(result))
  expect_true("TimeTaken" %in% names(result))
  expect_true("FilesProcessed" %in% names(result))
  expect_true("FilesPerSecond" %in% names(result))
  expect_equal(nrow(result), 1)
  expect_equal(result$FilesProcessed[1], 2)
  
  unlink(test_dir, recursive = TRUE)
})

test_that("benchmark_analysis handles edge cases", {
  # Test with non-existent folder
  expect_error(benchmark_analysis("/non/existent/path"), "Folder does not exist")
  
  # Test with empty folder
  temp_dir <- tempdir()
  empty_dir <- file.path(temp_dir, "empty_benchmark")
  dir.create(empty_dir, showWarnings = FALSE)
  
  expect_error(benchmark_analysis(empty_dir), "No files found")
  
  unlink(empty_dir, recursive = TRUE)
})