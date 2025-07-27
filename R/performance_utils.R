#' Analyze Folder with Parallel Processing
#'
#' Enhanced version of analyze_folder with parallel processing support for better
#' performance when dealing with large numbers of files.
#'
#' @param folder_path A character string specifying the path to the folder containing files.
#' @param parallel Logical, whether to use parallel processing. Default is FALSE.
#' @param num_cores Integer, number of cores to use for parallel processing. 
#'   If NULL, uses detectCores() - 1.
#' @param include_metadata Logical, whether to include detailed metadata extraction.
#'   Default is FALSE for better performance.
#' @param recursive Logical, whether to scan subdirectories recursively. Default is FALSE.
#' @param file_pattern Optional character string, regex pattern to filter files.
#'
#' @return A data frame with file analysis results, optionally including metadata.
#'
#' @examples
#' \dontrun{
#' # Basic usage
#' result <- analyze_folder_parallel("path/to/folder")
#' 
#' # With parallel processing and metadata
#' result <- analyze_folder_parallel("path/to/folder", 
#'                                   parallel = TRUE, 
#'                                   include_metadata = TRUE)
#' 
#' # Recursive with file filtering
#' result <- analyze_folder_parallel("path/to/folder",
#'                                   recursive = TRUE,
#'                                   file_pattern = "\\.(fastq|fq)$")
#' }
#'
#' @export
analyze_folder_parallel <- function(folder_path, 
                                   parallel = FALSE, 
                                   num_cores = NULL,
                                   include_metadata = FALSE,
                                   recursive = FALSE,
                                   file_pattern = NULL) {
  
  # Check if folder exists
  if (!dir.exists(folder_path)) {
    stop("The specified folder does not exist. Please provide a valid folder path.")
  }
  
  # Get file list
  files <- list.files(path = folder_path, 
                     full.names = TRUE, 
                     recursive = recursive)
  
  # Apply file pattern filter if provided
  if (!is.null(file_pattern)) {
    files <- files[grepl(file_pattern, files)]
  }
  
  # Check if folder contains any files
  if (length(files) == 0) {
    warning("No files found matching criteria.")
    result_cols <- c("FileName", "Extension", "Recommendation")
    if (include_metadata) {
      result_cols <- c(result_cols, "FileSize", "Valid", "SequenceType", "NumFeatures")
    }
    return(data.frame(matrix(ncol = length(result_cols), nrow = 0,
                           dimnames = list(NULL, result_cols))))
  }
  
  # Setup parallel processing if requested
  if (parallel) {
    if (!requireNamespace("parallel", quietly = TRUE)) {
      warning("Package 'parallel' not available. Falling back to sequential processing.")
      parallel <- FALSE
    } else {
      if (is.null(num_cores)) {
        num_cores <- max(1, parallel::detectCores() - 1)
      }
      message(sprintf("Using %d cores for parallel processing", num_cores))
    }
  }
  
  # Create analysis function for each file
  analyze_single_file <- function(file_path) {
    tryCatch({
      # Get basic file info
      file_name <- basename(file_path)
      extension <- tools::file_ext(file_name)
      
      # Get recommendation
      temp_df <- data.frame(FileName = file_name, 
                           Extension = extension, 
                           stringsAsFactors = FALSE)
      recommendation_df <- recommend_packages(temp_df)
      
      result <- list(
        FileName = file_name,
        FilePath = file_path,
        Extension = extension,
        Recommendation = recommendation_df$Recommendation[1]
      )
      
      # Add metadata if requested
      if (include_metadata) {
        file_info <- file.info(file_path)
        result$FileSize <- file_info$size
        result$Modified <- file_info$mtime
        
        # Quick validation
        validation <- validate_file(file_path)
        result$Valid <- validation$valid
        result$Issues <- paste(validation$issues, collapse = "; ")
        
        # Format-specific quick stats
        if (extension %in% c("fasta", "fastq", "vcf", "gtf", "gff")) {
          metadata <- extract_quick_stats(file_path, extension)
          result <- c(result, metadata)
        }
      }
      
      return(result)
      
    }, error = function(e) {
      return(list(
        FileName = basename(file_path),
        FilePath = file_path,
        Extension = tools::file_ext(basename(file_path)),
        Recommendation = paste("Error:", e$message),
        Error = TRUE
      ))
    })
  }
  
  # Process files
  if (parallel && length(files) > 1) {
    cl <- parallel::makeCluster(num_cores)
    on.exit(parallel::stopCluster(cl))
    
    # Export necessary functions to cluster
    parallel::clusterEvalQ(cl, library(bionovice))
    parallel::clusterExport(cl, c("analyze_single_file", "include_metadata"), 
                           envir = environment())
    
    results <- parallel::parLapply(cl, files, analyze_single_file)
  } else {
    if (length(files) > 10) {
      message(sprintf("Processing %d files sequentially...", length(files)))
    }
    results <- lapply(files, analyze_single_file)
  }
  
  # Convert results to data frame
  df_results <- do.call(rbind, lapply(results, function(x) {
    data.frame(x, stringsAsFactors = FALSE)
  }))
  
  # Add summary information
  attr(df_results, "summary") <- list(
    total_files = length(files),
    processing_mode = if (parallel) "parallel" else "sequential",
    timestamp = Sys.time()
  )
  
  return(df_results)
}

#' Extract Quick Statistics for Performance
#' @param file_path Path to file
#' @param extension File extension
#' @keywords internal
extract_quick_stats <- function(file_path, extension) {
  stats <- list()
  
  tryCatch({
    # Read limited number of lines for quick analysis
    lines <- readLines(file_path, n = 1000, warn = FALSE)
    
    switch(extension,
      "fasta" = {
        headers <- sum(grepl("^>", lines))
        stats$NumSequences <- headers
        if (headers > 0) {
          # Quick sequence type detection from first sequence
          seq_line <- lines[which(grepl("^>", lines))[1] + 1]
          if (length(seq_line) > 0 && !is.na(seq_line)) {
            stats$SequenceType <- detect_sequence_type_quick(seq_line)
          }
        }
      },
      "fastq" = {
        reads <- floor(length(lines) / 4)
        stats$NumReads <- reads
        if (reads > 0) {
          # Get read length from first read
          first_seq <- lines[2]
          if (!is.na(first_seq)) {
            stats$ReadLength <- nchar(first_seq)
          }
        }
      },
      "vcf" = {
        header_lines <- sum(grepl("^##", lines))
        data_lines <- sum(!grepl("^#", lines))
        stats$HeaderLines <- header_lines
        stats$VariantLines <- data_lines
      },
      "gtf" = ,
      "gff" = {
        comment_lines <- sum(grepl("^#", lines))
        feature_lines <- sum(!grepl("^#", lines))
        stats$CommentLines <- comment_lines
        stats$FeatureLines <- feature_lines
      }
    )
    
  }, error = function(e) {
    stats$StatsError <- e$message
  })
  
  return(stats)
}

#' Quick sequence type detection for performance
#' @param sequence Single sequence string
#' @keywords internal
detect_sequence_type_quick <- function(sequence) {
  if (is.na(sequence) || nchar(sequence) == 0) return("unknown")
  
  sequence <- toupper(gsub("[^A-Z]", "", sequence))
  if (nchar(sequence) == 0) return("unknown")
  
  # Quick check with just first 50 characters
  sample_seq <- substr(sequence, 1, min(50, nchar(sequence)))
  
  # Count character types
  has_u <- grepl("U", sample_seq)
  has_protein <- grepl("[DEFHIKLMNPQRSVWY]", sample_seq)
  has_only_dna <- grepl("^[ACGT]*$", sample_seq)
  
  if (has_only_dna) return("DNA")
  if (has_u && !has_protein) return("RNA")
  if (has_protein) return("Protein")
  
  return("unknown")
}

#' Benchmark File Analysis Performance
#'
#' Utility function to benchmark the performance of file analysis operations.
#'
#' @param folder_path Path to test folder
#' @param methods Vector of methods to test: c("sequential", "parallel", "metadata")
#' @param num_cores Number of cores for parallel testing
#'
#' @return Data frame with benchmark results
#'
#' @examples
#' \dontrun{
#' benchmark <- benchmark_analysis("path/to/test/folder")
#' print(benchmark)
#' }
#'
#' @export
benchmark_analysis <- function(folder_path, 
                              methods = c("sequential", "parallel", "metadata"),
                              num_cores = 2) {
  
  if (!dir.exists(folder_path)) {
    stop("Folder does not exist")
  }
  
  files <- list.files(folder_path)
  if (length(files) == 0) {
    stop("No files found in folder")
  }
  
  results <- data.frame(
    Method = character(),
    TimeTaken = numeric(),
    FilesProcessed = integer(),
    FilesPerSecond = numeric(),
    stringsAsFactors = FALSE
  )
  
  message(sprintf("Benchmarking with %d files", length(files)))
  
  # Sequential processing
  if ("sequential" %in% methods) {
    message("Testing sequential processing...")
    start_time <- Sys.time()
    result_seq <- analyze_folder_parallel(folder_path, parallel = FALSE)
    end_time <- Sys.time()
    
    time_taken <- as.numeric(end_time - start_time)
    results <- rbind(results, data.frame(
      Method = "Sequential",
      TimeTaken = time_taken,
      FilesProcessed = nrow(result_seq),
      FilesPerSecond = nrow(result_seq) / time_taken
    ))
  }
  
  # Parallel processing
  if ("parallel" %in% methods && length(files) > 1) {
    message("Testing parallel processing...")
    start_time <- Sys.time()
    result_par <- analyze_folder_parallel(folder_path, 
                                         parallel = TRUE, 
                                         num_cores = num_cores)
    end_time <- Sys.time()
    
    time_taken <- as.numeric(end_time - start_time)
    results <- rbind(results, data.frame(
      Method = paste("Parallel", num_cores, "cores"),
      TimeTaken = time_taken,
      FilesProcessed = nrow(result_par),
      FilesPerSecond = nrow(result_par) / time_taken
    ))
  }
  
  # With metadata
  if ("metadata" %in% methods) {
    message("Testing with metadata extraction...")
    start_time <- Sys.time()
    result_meta <- analyze_folder_parallel(folder_path, 
                                          parallel = FALSE,
                                          include_metadata = TRUE)
    end_time <- Sys.time()
    
    time_taken <- as.numeric(end_time - start_time)
    results <- rbind(results, data.frame(
      Method = "With Metadata",
      TimeTaken = time_taken,
      FilesProcessed = nrow(result_meta),
      FilesPerSecond = nrow(result_meta) / time_taken
    ))
  }
  
  # Calculate speedup
  if (nrow(results) > 1) {
    baseline <- results$TimeTaken[results$Method == "Sequential"]
    if (length(baseline) > 0) {
      results$SpeedupVsSequential <- baseline / results$TimeTaken
    }
  }
  
  return(results)
}