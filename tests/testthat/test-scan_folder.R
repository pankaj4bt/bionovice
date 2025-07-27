test_that("scan_folder works correctly", {
  # Create temporary test directory with test files
  temp_dir <- tempdir()
  test_dir <- file.path(temp_dir, "test_bionovice")
  dir.create(test_dir, showWarnings = FALSE)
  
  # Create test files with different extensions
  test_files <- c("test.fastq", "data.bam", "genome.fasta", "variants.vcf", "annotations.gtf")
  for (file in test_files) {
    file.create(file.path(test_dir, file))
  }
  
  # Test scan_folder function
  result <- scan_folder(test_dir)
  
  # Verify results
  expect_s3_class(result, "data.frame")
  expect_named(result, c("FileName", "Extension"))
  expect_equal(nrow(result), 5)
  expect_true(all(test_files %in% result$FileName))
  
  # Check specific extensions
  expect_true("fastq" %in% result$Extension)
  expect_true("bam" %in% result$Extension)
  expect_true("fasta" %in% result$Extension)
  expect_true("vcf" %in% result$Extension)
  expect_true("gtf" %in% result$Extension)
  
  # Clean up
  unlink(test_dir, recursive = TRUE)
})

test_that("scan_folder handles empty folder", {
  temp_dir <- tempdir()
  empty_dir <- file.path(temp_dir, "empty_test")
  dir.create(empty_dir, showWarnings = FALSE)
  
  # Test with warning
  expect_warning(result <- scan_folder(empty_dir), "The folder is empty")
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 0)
  expect_named(result, c("FileName", "Extension"))
  
  unlink(empty_dir, recursive = TRUE)
})

test_that("scan_folder handles non-existent folder", {
  non_existent <- file.path(tempdir(), "does_not_exist")
  expect_error(scan_folder(non_existent), "The specified folder does not exist")
})

test_that("scan_folder handles files without extensions", {
  temp_dir <- tempdir()
  test_dir <- file.path(temp_dir, "no_ext_test")
  dir.create(test_dir, showWarnings = FALSE)
  
  # Create files without extensions
  file.create(file.path(test_dir, "file1"))
  file.create(file.path(test_dir, "file2"))
  
  result <- scan_folder(test_dir)
  expect_equal(nrow(result), 2)
  expect_true(all(result$Extension == ""))
  
  unlink(test_dir, recursive = TRUE)
})