test_that("analyze_folder integrates scan_folder and recommend_packages", {
  # Create temporary test directory
  temp_dir <- tempdir()
  test_dir <- file.path(temp_dir, "analyze_test")
  dir.create(test_dir, showWarnings = FALSE)
  
  # Create test files
  test_files <- c("test.fastq", "data.bam", "genome.fasta")
  for (file in test_files) {
    file.create(file.path(test_dir, file))
  }
  
  # Test analyze_folder
  result <- analyze_folder(test_dir)
  
  # Check structure
  expect_s3_class(result, "data.frame")
  expect_named(result, c("FileName", "Extension", "Recommendation"))
  expect_equal(nrow(result), 3)
  
  # Check that files are properly identified
  expect_true(all(test_files %in% result$FileName))
  expect_true("fastq" %in% result$Extension)
  expect_true("bam" %in% result$Extension)
  expect_true("fasta" %in% result$Extension)
  
  # Check that recommendations are provided
  expect_true(all(nchar(result$Recommendation) > 0))
  expect_false(any(is.na(result$Recommendation)))
  
  # Clean up
  unlink(test_dir, recursive = TRUE)
})

test_that("analyze_folder handles edge cases", {
  # Test with empty folder
  temp_dir <- tempdir()
  empty_dir <- file.path(temp_dir, "empty_analyze")
  dir.create(empty_dir, showWarnings = FALSE)
  
  expect_warning(result <- analyze_folder(empty_dir), "The folder is empty")
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 0)
  expect_named(result, c("FileName", "Extension", "Recommendation"))
  
  unlink(empty_dir, recursive = TRUE)
  
  # Test with non-existent folder
  non_existent <- file.path(tempdir(), "does_not_exist_analyze")
  expect_error(analyze_folder(non_existent), "The specified folder does not exist")
})

test_that("analyze_folder preserves original functionality", {
  # This test ensures the main workflow still works as expected
  temp_dir <- tempdir()
  test_dir <- file.path(temp_dir, "workflow_test")
  dir.create(test_dir, showWarnings = FALSE)
  
  # Create mixed file types
  mixed_files <- c("reads.fastq", "alignment.sam", "variants.vcf", "annotation.gff", "data.csv")
  for (file in mixed_files) {
    file.create(file.path(test_dir, file))
  }
  
  result <- analyze_folder(test_dir)
  
  # Verify each file type gets appropriate recommendation
  fastq_rec <- result$Recommendation[result$Extension == "fastq"]
  sam_rec <- result$Recommendation[result$Extension == "sam"]
  vcf_rec <- result$Recommendation[result$Extension == "vcf"]
  gff_rec <- result$Recommendation[result$Extension == "gff"]
  csv_rec <- result$Recommendation[result$Extension == "csv"]
  
  expect_true(grepl("ShortRead", fastq_rec))
  expect_true(grepl("Rsamtools", sam_rec))
  expect_true(grepl("VariantAnnotation", vcf_rec))
  expect_true(grepl("rtracklayer", gff_rec))
  expect_true(grepl("read.csv", csv_rec))
  
  unlink(test_dir, recursive = TRUE)
})