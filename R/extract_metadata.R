#' Extract File Metadata
#'
#' This function extracts detailed metadata from bioinformatics files including
#' file size, creation time, format-specific information, and basic statistics.
#'
#' @param file_path A character string specifying the path to the file.
#' @param include_preview Logical, whether to include a preview of file content.
#'   Default is TRUE.
#' @param max_preview_lines Integer, maximum number of lines to include in preview.
#'   Default is 10.
#'
#' @return A list containing file metadata:
#'   - \code{file_info}: Basic file information (size, dates, permissions)
#'   - \code{format_info}: Format-specific metadata
#'   - \code{content_preview}: Sample of file content (if requested)
#'   - \code{statistics}: Basic file statistics
#'
#' @examples
#' \dontrun{
#' # Extract metadata from a FASTA file
#' metadata <- extract_file_metadata("sequences.fasta")
#' print(metadata)
#' }
#'
#' @export
extract_file_metadata <- function(file_path, include_preview = TRUE, max_preview_lines = 10) {
  # Initialize result structure
  result <- list(
    file_info = NULL,
    format_info = NULL,
    content_preview = NULL,
    statistics = NULL
  )
  
  # Check if file exists
  if (!file.exists(file_path)) {
    warning("File does not exist: ", file_path)
    return(result)
  }
  
  # Get basic file information
  file_info <- file.info(file_path)
  result$file_info <- list(
    path = normalizePath(file_path),
    size_bytes = file_info$size,
    size_human = format_file_size(file_info$size),
    created = file_info$ctime,
    modified = file_info$mtime,
    accessed = file_info$atime,
    readable = file.access(file_path, 4) == 0,
    writable = file.access(file_path, 2) == 0
  )
  
  # Extract format-specific information
  extension <- tolower(tools::file_ext(file_path))
  result$format_info <- extract_format_metadata(file_path, extension)
  
  # Add content preview if requested
  if (include_preview && result$file_info$readable) {
    result$content_preview <- get_file_preview(file_path, max_preview_lines)
  }
  
  # Calculate basic statistics
  result$statistics <- calculate_file_statistics(file_path, extension)
  
  return(result)
}

#' Extract format-specific metadata
#' @param file_path Path to file
#' @param extension File extension
#' @keywords internal
extract_format_metadata <- function(file_path, extension) {
  format_info <- list(
    extension = extension,
    detected_format = extension,
    compression = detect_compression(file_path)
  )
  
  # Add format-specific metadata
  format_info <- switch(extension,
    "fasta" = c(format_info, extract_fasta_metadata(file_path)),
    "fastq" = c(format_info, extract_fastq_metadata(file_path)),
    "vcf" = c(format_info, extract_vcf_metadata(file_path)),
    "bam" = c(format_info, extract_bam_metadata(file_path)),
    "sam" = c(format_info, extract_sam_metadata(file_path)),
    "bed" = c(format_info, extract_bed_metadata(file_path)),
    "gtf" = c(format_info, extract_gtf_metadata(file_path)),
    "gff" = c(format_info, extract_gff_metadata(file_path)),
    format_info  # Default case
  )
  
  return(format_info)
}

#' Extract FASTA-specific metadata
#' @param file_path Path to FASTA file
#' @keywords internal
extract_fasta_metadata <- function(file_path) {
  metadata <- list()
  
  tryCatch({
    # Count sequences and get basic stats
    lines <- readLines(file_path, warn = FALSE)
    header_lines <- grepl("^>", lines)
    
    metadata$num_sequences <- sum(header_lines)
    metadata$num_lines <- length(lines)
    
    if (metadata$num_sequences > 0) {
      # Analyze sequence lengths
      seq_starts <- which(header_lines)
      seq_lengths <- numeric(metadata$num_sequences)
      
      for (i in seq_along(seq_starts)) {
        start_line <- seq_starts[i] + 1
        end_line <- if (i < length(seq_starts)) seq_starts[i + 1] - 1 else length(lines)
        
        if (start_line <= length(lines) && start_line <= end_line) {
          seq_text <- paste(lines[start_line:end_line], collapse = "")
          seq_lengths[i] <- nchar(gsub("[^A-Za-z]", "", seq_text))
        }
      }
      
      metadata$sequence_lengths <- list(
        min = min(seq_lengths, na.rm = TRUE),
        max = max(seq_lengths, na.rm = TRUE),
        mean = mean(seq_lengths, na.rm = TRUE),
        median = median(seq_lengths, na.rm = TRUE)
      )
      
      # Detect sequence type (DNA/RNA/Protein)
      sample_seq <- paste(lines[!header_lines][1:min(10, sum(!header_lines))], collapse = "")
      metadata$sequence_type <- detect_sequence_type(sample_seq)
    }
    
  }, error = function(e) {
    metadata$error <<- e$message
  })
  
  return(metadata)
}

#' Extract FASTQ-specific metadata
#' @param file_path Path to FASTQ file
#' @keywords internal
extract_fastq_metadata <- function(file_path) {
  metadata <- list()
  
  tryCatch({
    lines <- readLines(file_path, n = 1000, warn = FALSE)
    
    if (length(lines) >= 4) {
      num_complete_reads <- floor(length(lines) / 4)
      metadata$num_reads_sampled <- num_complete_reads
      
      # Sample quality scores
      qual_lines <- seq(4, min(length(lines), num_complete_reads * 4), by = 4)
      quality_data <- lines[qual_lines]
      
      if (length(quality_data) > 0) {
        # Analyze quality encoding
        qual_chars <- unlist(strsplit(paste(quality_data, collapse = ""), ""))
        qual_ascii <- utf8ToInt(paste(qual_chars, collapse = ""))
        
        metadata$quality_encoding <- detect_quality_encoding(qual_ascii)
        metadata$quality_range <- list(
          min_ascii = min(qual_ascii),
          max_ascii = max(qual_ascii)
        )
      }
      
      # Sample read lengths
      seq_lines <- seq(2, min(length(lines), num_complete_reads * 4), by = 4)
      read_lengths <- nchar(lines[seq_lines])
      
      metadata$read_lengths <- list(
        min = min(read_lengths),
        max = max(read_lengths),
        mean = mean(read_lengths),
        all_same = length(unique(read_lengths)) == 1
      )
    }
    
  }, error = function(e) {
    metadata$error <<- e$message
  })
  
  return(metadata)
}

#' Extract VCF-specific metadata  
#' @param file_path Path to VCF file
#' @keywords internal
extract_vcf_metadata <- function(file_path) {
  metadata <- list()
  
  tryCatch({
    lines <- readLines(file_path, n = 1000, warn = FALSE)
    
    # Extract header information
    header_lines <- lines[grepl("^##", lines)]
    metadata$num_header_lines <- length(header_lines)
    
    # Extract VCF version
    version_line <- header_lines[grepl("^##fileformat=VCF", header_lines)]
    if (length(version_line) > 0) {
      metadata$vcf_version <- gsub("^##fileformat=VCF", "", version_line[1])
    }
    
    # Count INFO and FORMAT fields
    metadata$num_info_fields <- sum(grepl("^##INFO=", header_lines))
    metadata$num_format_fields <- sum(grepl("^##FORMAT=", header_lines))
    
    # Find column header and count samples
    col_header <- lines[grepl("^#CHROM", lines)]
    if (length(col_header) > 0) {
      cols <- strsplit(col_header[1], "\t")[[1]]
      standard_cols <- c("#CHROM", "POS", "ID", "REF", "ALT", "QUAL", "FILTER", "INFO", "FORMAT")
      metadata$num_samples <- length(cols) - length(standard_cols)
    }
    
  }, error = function(e) {
    metadata$error <<- e$message
  })
  
  return(metadata)
}

#' Simple metadata extraction for other formats
#' @param file_path Path to file
#' @keywords internal
extract_bam_metadata <- function(file_path) {
  return(list(format_note = "BAM files require specialized tools for metadata extraction"))
}

#' @keywords internal
extract_sam_metadata <- function(file_path) {
  metadata <- list()
  tryCatch({
    lines <- readLines(file_path, n = 100, warn = FALSE)
    metadata$num_header_lines <- sum(grepl("^@", lines))
  }, error = function(e) {
    metadata$error <- e$message
  })
  return(metadata)
}

#' @keywords internal  
extract_bed_metadata <- function(file_path) {
  metadata <- list()
  tryCatch({
    data <- read.table(file_path, nrows = 100, sep = "\t", header = FALSE, stringsAsFactors = FALSE)
    metadata$num_columns <- ncol(data)
    metadata$num_intervals_sampled <- nrow(data)
  }, error = function(e) {
    metadata$error <- e$message
  })
  return(metadata)
}

#' @keywords internal
extract_gtf_metadata <- function(file_path) {
  metadata <- list()
  tryCatch({
    lines <- readLines(file_path, n = 1000, warn = FALSE)
    data_lines <- lines[!grepl("^#", lines)]
    metadata$num_features_sampled <- length(data_lines)
    metadata$num_comment_lines <- sum(grepl("^#", lines))
  }, error = function(e) {
    metadata$error <- e$message
  })
  return(metadata)
}

#' @keywords internal
extract_gff_metadata <- function(file_path) {
  return(extract_gtf_metadata(file_path))  # Similar format
}

#' Helper functions
#' @keywords internal
format_file_size <- function(bytes) {
  if (bytes >= 1024^3) {
    return(paste(round(bytes / 1024^3, 2), "GB"))
  } else if (bytes >= 1024^2) {
    return(paste(round(bytes / 1024^2, 2), "MB"))
  } else if (bytes >= 1024) {
    return(paste(round(bytes / 1024, 2), "KB"))
  } else {
    return(paste(bytes, "bytes"))
  }
}

#' @keywords internal
detect_compression <- function(file_path) {
  if (grepl("\\.gz$", file_path)) return("gzip")
  if (grepl("\\.bz2$", file_path)) return("bzip2") 
  if (grepl("\\.xz$", file_path)) return("xz")
  if (grepl("\\.zip$", file_path)) return("zip")
  return("none")
}

#' @keywords internal
detect_sequence_type <- function(sequence) {
  sequence <- toupper(gsub("[^A-Z]", "", sequence))
  
  # Count nucleotide characters
  dna_chars <- sum(gregexpr("[ACGT]", sequence)[[1]] > 0)
  rna_chars <- sum(gregexpr("[ACGU]", sequence)[[1]] > 0)
  protein_chars <- sum(gregexpr("[DEFHIKLMNPQRSVWY]", sequence)[[1]] > 0)
  
  total_chars <- nchar(sequence)
  
  if (total_chars == 0) return("unknown")
  
  dna_prop <- dna_chars / total_chars
  rna_prop <- rna_chars / total_chars  
  protein_prop <- protein_chars / total_chars
  
  if (dna_prop > 0.9) return("DNA")
  if (rna_prop > 0.9) return("RNA") 
  if (protein_prop > 0.1) return("Protein")
  
  return("unknown")
}

#' @keywords internal
detect_quality_encoding <- function(ascii_values) {
  min_val <- min(ascii_values)
  max_val <- max(ascii_values)
  
  if (min_val >= 33 && max_val <= 126) {
    if (min_val >= 64) return("Illumina 1.8+")
    if (min_val >= 59) return("Solexa")
    return("Phred+33")
  }
  
  return("Unknown")
}

#' @keywords internal
get_file_preview <- function(file_path, max_lines) {
  tryCatch({
    lines <- readLines(file_path, n = max_lines, warn = FALSE)
    return(lines)
  }, error = function(e) {
    return(paste("Error reading file:", e$message))
  })
}

#' @keywords internal
calculate_file_statistics <- function(file_path, extension) {
  stats <- list()
  
  tryCatch({
    lines <- readLines(file_path, warn = FALSE)
    stats$total_lines <- length(lines)
    stats$empty_lines <- sum(lines == "")
    stats$avg_line_length <- mean(nchar(lines))
    stats$max_line_length <- max(nchar(lines))
    
  }, error = function(e) {
    stats$error <- e$message
  })
  
  return(stats)
}