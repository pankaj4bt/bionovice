#' Recommend Packages Based on File Extensions
#'
#' This function provides recommendations for R packages or methods to open or analyze files based on their extensions.
#'
#' @param files_df A data frame containing file names and extensions, typically the output from \code{scan_folder}.
#'
#' @return A data frame with an additional column:
#' \describe{
#'   \item{Recommendation}{Suggested R packages or methods for each file type.}
#' }
#'
#' @examples
#' \dontrun{
#' recommendations_df <- recommend_packages(files_df)
#' }
#'
#' @export
recommend_packages <- function(files_df) {
  recommendations <- sapply(files_df$Extension, function(ext) {
    switch(tolower(ext),
           "fastq" = "Use the ShortRead package to read FASTQ files.",
           "fasta" = "Use the Biostrings package to read FASTA files.",
           "vcf"   = "Use the VariantAnnotation package for VCF files.",
           "bam"   = "Use the Rsamtools package to handle BAM files.",
           "csv"   = "Use read.csv() or the data.table package.",
           "txt"   = "Use read.table() or readLines().",
           "rds"   = "Use readRDS() function.",
           "Unknown extension. Please check the file format.")
  })
  files_df$Recommendation <- recommendations
  return(files_df)
}
