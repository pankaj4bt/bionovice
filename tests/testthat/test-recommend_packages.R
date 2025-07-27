test_that("recommend_packages provides correct recommendations", {
  # Create test data frame
  test_files <- data.frame(
    FileName = c("test.fastq", "data.bam", "genome.fasta", "variants.vcf", "annotations.gtf"),
    Extension = c("fastq", "bam", "fasta", "vcf", "gtf"),
    stringsAsFactors = FALSE
  )
  
  result <- recommend_packages(test_files)
  
  # Check structure
  expect_s3_class(result, "data.frame")
  expect_named(result, c("FileName", "Extension", "Recommendation"))
  expect_equal(nrow(result), 5)
  
  # Check specific recommendations
  expect_true(grepl("ShortRead", result$Recommendation[result$Extension == "fastq"]))
  expect_true(grepl("Rsamtools", result$Recommendation[result$Extension == "bam"]))
  expect_true(grepl("Biostrings", result$Recommendation[result$Extension == "fasta"]))
  expect_true(grepl("VariantAnnotation", result$Recommendation[result$Extension == "vcf"]))
  expect_true(grepl("rtracklayer", result$Recommendation[result$Extension == "gtf"]))
})

test_that("recommend_packages handles unknown extensions", {
  test_files <- data.frame(
    FileName = c("test.unknown", "data.xyz"),
    Extension = c("unknown", "xyz"),
    stringsAsFactors = FALSE
  )
  
  expect_warning(result <- recommend_packages(test_files), "Unknown extension")
  expect_true(all(grepl("Unknown extension", result$Recommendation)))
})

test_that("recommend_packages handles empty input", {
  empty_df <- data.frame(
    FileName = character(0),
    Extension = character(0),
    stringsAsFactors = FALSE
  )
  
  result <- recommend_packages(empty_df)
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 0)
  expect_true("Recommendation" %in% names(result))
})

test_that("recommend_packages handles compressed files", {
  test_files <- data.frame(
    FileName = c("test.fastq.gz", "data.vcf.gz", "file.tar.gz"),
    Extension = c("fastq.gz", "vcf.gz", "tar.gz"),
    stringsAsFactors = FALSE
  )
  
  result <- recommend_packages(test_files)
  
  expect_true(grepl("ShortRead.*compressed", result$Recommendation[result$Extension == "fastq.gz"]))
  expect_true(grepl("VariantAnnotation.*compressed", result$Recommendation[result$Extension == "vcf.gz"]))
  expect_true(grepl("untar", result$Recommendation[result$Extension == "tar.gz"]))
})

test_that("recommend_packages handles various bioinformatics formats", {
  bio_formats <- data.frame(
    FileName = c("single_cell.h5ad", "expression.loom", "matrix.mtx", "sequences.ab1"),
    Extension = c("h5ad", "loom", "mtx", "ab1"),
    stringsAsFactors = FALSE
  )
  
  result <- recommend_packages(bio_formats)
  
  expect_true(grepl("zellkonverter", result$Recommendation[result$Extension == "h5ad"]))
  expect_true(grepl("loomR", result$Recommendation[result$Extension == "loom"]))
  expect_true(grepl("Matrix", result$Recommendation[result$Extension == "mtx"]))
  expect_true(grepl("sangerseqR", result$Recommendation[result$Extension == "ab1"]))
})