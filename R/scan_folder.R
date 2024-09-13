#' Scan Folder and Extract File Extensions
#'
#' This function scans a specified folder and extracts the file names and their extensions.
#'
#' @param folder_path A character string specifying the path to the folder containing files.
#'
#' @return A data frame with two columns:
#'   - \code{FileName}: Names of the files in the folder.
#'   - \code{Extension}: File extensions of the files.
#'
#' @examples
#' # Example usage:
#' # files_df <- scan_folder("path/to/your/folder")
#'
#' @export
scan_folder <- function(folder_path) {
  # Check if the folder exists
  if (!dir.exists(folder_path)) {
    stop("The specified folder does not exist. Please provide a valid folder path.")
  }

  # List all files in the folder
  files <- list.files(path = folder_path, full.names = FALSE)

  # Check if the folder contains any files
  if (length(files) == 0) {
    warning("The folder is empty. No files to analyze.")
    return(data.frame(FileName = character(0), Extension = character(0), stringsAsFactors = FALSE))
  }

  # Extract the file extensions
  extensions <- tools::file_ext(files)

  # Create a data frame with file names and extensions
  files_df <- data.frame(
    FileName = files,
    Extension = extensions,
    stringsAsFactors = FALSE
  )

  return(files_df)
}
