#' Analyze Folder and Get Recommendations
#'
#' This is the main function that users can call to scan a folder and receive recommendations for handling the files.
#'
#' @param folder_path A character string specifying the path to the folder containing files.
#'
#' @return A data frame with file names, extensions, and recommendations.
#'
#' @examples
#' \dontrun{
#' result <- analyze_folder("path/to/your/folder")
#' print(result)
#' }
#'
#' @export
analyze_folder <- function(folder_path) {
  files_df <- scan_folder(folder_path)
  result <- recommend_packages(files_df)
  return(result)
}
