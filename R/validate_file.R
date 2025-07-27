#' Validate File Format and Structure
#'
#' This function validates if files match their expected format based on extension
#' and performs basic structure validation.
#'
#' @param file_path A character string specifying the path to the file.
#' @param expected_format Optional character string specifying the expected format.
#'   If NULL, format is inferred from file extension.
#'
#' @return A list containing validation results:
#'   - \code{valid}: Logical indicating if the file passed validation
#'   - \code{format}: Detected or expected format
#'   - \code{issues}: Character vector of any issues found
#'   - \code{file_size}: File size in bytes
#'   - \code{readable}: Whether the file is readable
#'
#' @examples
#' \dontrun{
#' # Validate a FASTA file
#' result <- validate_file("sequences.fasta")
#' print(result)
#' }
#'
#' @export
validate_file <- function(file_path, expected_format = NULL) {
  # Initialize result structure
  result <- list(
    valid = FALSE,
    format = NULL,
    issues = character(0),
    file_size = NA,
    readable = FALSE
  )
  
  # Check if file exists
  if (!file.exists(file_path)) {
    result$issues <- c(result$issues, "File does not exist")
    return(result)
  }
  
  # Get file info
  file_info <- file.info(file_path)
  result$file_size <- file_info$size
  result$readable <- file.access(file_path, 4) == 0
  
  if (!result$readable) {
    result$issues <- c(result$issues, "File is not readable")
    return(result)
  }
  
  # Determine format
  if (is.null(expected_format)) {
    extension <- tools::file_ext(file_path)
    result$format <- tolower(extension)
  } else {
    result$format <- tolower(expected_format)
  }
  
  # Perform format-specific validation
  validation_result <- switch(result$format,
    "fasta" = validate_fasta_format(file_path),
    "fastq" = validate_fastq_format(file_path),
    "vcf" = validate_vcf_format(file_path),
    "csv" = validate_csv_format(file_path),
    "tsv" = validate_tsv_format(file_path),
    # Default case
    list(valid = TRUE, issues = character(0))
  )
  
  result$valid <- validation_result$valid && length(result$issues) == 0
  result$issues <- c(result$issues, validation_result$issues)
  
  return(result)
}

#' Validate FASTA file format
#' @param file_path Path to FASTA file
#' @keywords internal
validate_fasta_format <- function(file_path) {
  issues <- character(0)
  
  tryCatch({
    # Read first few lines
    lines <- readLines(file_path, n = 100, warn = FALSE)
    
    if (length(lines) == 0) {
      issues <- c(issues, "File is empty")
      return(list(valid = FALSE, issues = issues))
    }
    
    # Check for FASTA header
    if (!any(grepl("^>", lines))) {
      issues <- c(issues, "No FASTA headers found (lines starting with '>')")
    }
    
    # Check for reasonable sequence content
    seq_lines <- lines[!grepl("^>", lines)]
    if (length(seq_lines) > 0) {
      # Check if sequences contain valid nucleotide/amino acid characters
      combined_seq <- paste(seq_lines, collapse = "")
      if (!grepl("^[ACGTUNRYSWKMBDHV-]*$", combined_seq, ignore.case = TRUE)) {
        issues <- c(issues, "Sequences contain invalid characters for FASTA format")
      }
    }
    
  }, error = function(e) {
    issues <<- c(issues, paste("Error reading file:", e$message))
  })
  
  return(list(valid = length(issues) == 0, issues = issues))
}

#' Validate FASTQ file format
#' @param file_path Path to FASTQ file
#' @keywords internal
validate_fastq_format <- function(file_path) {
  issues <- character(0)
  
  tryCatch({
    lines <- readLines(file_path, n = 100, warn = FALSE)
    
    if (length(lines) == 0) {
      issues <- c(issues, "File is empty")
      return(list(valid = FALSE, issues = issues))
    }
    
    # FASTQ should have 4 lines per read
    if (length(lines) %% 4 != 0 && length(lines) >= 4) {
      issues <- c(issues, "File does not appear to follow FASTQ 4-line format")
    }
    
    # Check for FASTQ headers
    header_lines <- seq(1, min(length(lines), 100), by = 4)
    headers <- lines[header_lines]
    if (!all(grepl("^@", headers))) {
      issues <- c(issues, "Missing FASTQ headers (lines starting with '@')")
    }
    
    # Check for quality lines
    if (length(lines) >= 4) {
      qual_lines <- seq(4, min(length(lines), 100), by = 4)
      qual_data <- lines[qual_lines]
      if (any(nchar(qual_data) == 0)) {
        issues <- c(issues, "Empty quality lines found")
      }
    }
    
  }, error = function(e) {
    issues <<- c(issues, paste("Error reading file:", e$message))
  })
  
  return(list(valid = length(issues) == 0, issues = issues))
}

#' Validate VCF file format
#' @param file_path Path to VCF file
#' @keywords internal
validate_vcf_format <- function(file_path) {
  issues <- character(0)
  
  tryCatch({
    lines <- readLines(file_path, n = 100, warn = FALSE)
    
    if (length(lines) == 0) {
      issues <- c(issues, "File is empty")
      return(list(valid = FALSE, issues = issues))
    }
    
    # Check for VCF version header
    if (!any(grepl("^##fileformat=VCF", lines))) {
      issues <- c(issues, "Missing VCF file format header")
    }
    
    # Check for column header line
    header_line <- lines[grepl("^#CHROM", lines)]
    if (length(header_line) == 0) {
      issues <- c(issues, "Missing VCF column header line")
    } else {
      # Check required columns
      required_cols <- c("#CHROM", "POS", "ID", "REF", "ALT", "QUAL", "FILTER", "INFO")
      header_parts <- strsplit(header_line[1], "\t")[[1]]
      if (!all(required_cols %in% header_parts)) {
        issues <- c(issues, "Missing required VCF columns")
      }
    }
    
  }, error = function(e) {
    issues <<- c(issues, paste("Error reading file:", e$message))
  })
  
  return(list(valid = length(issues) == 0, issues = issues))
}

#' Validate CSV file format
#' @param file_path Path to CSV file
#' @keywords internal
validate_csv_format <- function(file_path) {
  issues <- character(0)
  
  tryCatch({
    # Try to read the file as CSV
    data <- read.csv(file_path, nrows = 5, header = TRUE)
    
    if (nrow(data) == 0) {
      issues <- c(issues, "File appears to be empty or has no data rows")
    }
    
    if (ncol(data) == 1) {
      issues <- c(issues, "File may not be properly comma-separated")
    }
    
  }, error = function(e) {
    issues <<- c(issues, paste("Error parsing CSV:", e$message))
  })
  
  return(list(valid = length(issues) == 0, issues = issues))
}

#' Validate TSV file format
#' @param file_path Path to TSV file
#' @keywords internal
validate_tsv_format <- function(file_path) {
  issues <- character(0)
  
  tryCatch({
    # Try to read the file as TSV
    data <- read.table(file_path, sep = "\t", nrows = 5, header = TRUE)
    
    if (nrow(data) == 0) {
      issues <- c(issues, "File appears to be empty or has no data rows")
    }
    
    if (ncol(data) == 1) {
      issues <- c(issues, "File may not be properly tab-separated")
    }
    
  }, error = function(e) {
    issues <<- c(issues, paste("Error parsing TSV:", e$message))
  })
  
  return(list(valid = length(issues) == 0, issues = issues))
}