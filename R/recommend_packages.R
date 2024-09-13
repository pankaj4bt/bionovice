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
           # FASTQ files
           "fastq"      = "Use the ShortRead package to read FASTQ files.",
           "fastq.gz"   = "Use the ShortRead package to read compressed FASTQ files.",
           "fq"         = "Use the ShortRead package to read FASTQ files.",
           "fq.gz"      = "Use the ShortRead package to read compressed FASTQ files.",

           # FASTA files
           "fasta"      = "Use the Biostrings package to read FASTA files.",
           "fa"         = "Use the Biostrings package to read FASTA files.",
           "fa.gz"      = "Use the Biostrings package to read compressed FASTA files.",

           # VCF and BCF files
           "vcf"        = "Use the VariantAnnotation package for VCF files.",
           "vcf.gz"     = "Use the VariantAnnotation package for compressed VCF files.",
           "bcf"        = "Use the VariantAnnotation package for BCF files.",
           "bcf.csi"    = "Use VariantAnnotation for BCF index files.",

           # BAM and SAM files
           "bam"        = "Use the Rsamtools package to handle BAM files.",
           "bam.gz"     = "Compressed BAM files; decompress or use Rsamtools directly.",
           "bai"        = "Use the Rsamtools package for BAM index files.",
           "bam.bai"    = "Use Rsamtools for BAM index files.",
           "sam"        = "Use the Rsamtools package to read SAM files.",

           # BED and BED-related files
           "bed"        = "Use the rtracklayer package to read BED files.",
           "bed.gz"     = "Use the rtracklayer package to read compressed BED files.",
           "bedgraph"   = "Use the rtracklayer package to read BedGraph files.",
           "bedpe"      = "Use the rtracklayer package to read BEDPE files.",

           # BigWig and Wiggle files
           "bigwig"     = "Use the rtracklayer package to read BigWig files.",
           "bw"         = "Use the rtracklayer package to read BigWig files.",
           "wig"        = "Use the rtracklayer package to read WIG files.",

           # GFF and GTF files
           "gff"        = "Use the rtracklayer package to read GFF files.",
           "gff3"       = "Use the rtracklayer package to read GFF3 files.",
           "gtf"        = "Use the rtracklayer package to read GTF files.",
           "gff.gz"     = "Use the rtracklayer package to read compressed GFF files.",
           "gtf.gz"     = "Use the rtracklayer package to read compressed GTF files.",

           # HDF5 and related files
           "h5"         = "Use the rhdf5 package to read HDF5 files.",
           "hdf5"       = "Use the rhdf5 package to read HDF5 files.",
           "hdf"        = "Use the rhdf5 package to read HDF5 files.",
           "h5ad"       = "Use the zellkonverter package to read H5AD files.",

           # Single-cell data formats
           "loom"       = "Use the loomR package to read Loom files.",
           "mtx"        = "Use the Matrix package to read Matrix Market files.",
           "mtx.gz"     = "Use the Matrix package to read compressed Matrix Market files.",

           # Alignment and assembly files
           "sra"        = "Use the SRAdb or SRAdbV2 package to access SRA files.",
           "cram"       = "Use the Rsamtools package to read CRAM files.",
           "cram.crai"  = "Use Rsamtools for CRAM index files.",

           # Microarray files
           "cel"        = "Use the affy package to read CEL files.",
           "cdf"        = "Use the affy package to read CDF files.",
           "idat"       = "Use the illuminaio package to read IDAT files.",

           # Phylogenetics files
           "nex"        = "Use the ape package to read Nexus files.",
           "nexus"      = "Use the ape package to read Nexus files.",
           "phy"        = "Use the ape package to read Phylip files.",
           "phylip"     = "Use the ape package to read Phylip files.",
           "tree"       = "Use the ape package to read tree files.",
           "newick"     = "Use the ape package to read Newick tree files.",

           # Mass spectrometry files
           "mzml"       = "Use the mzR package to read mzML files.",
           "mzxml"      = "Use the mzR package to read mzXML files.",
           "mgf"        = "Use the MSnbase package to read MGF files.",

           # Sequence files
           "ab1"        = "Use the sangerseqR package to read AB1 files.",
           "sff"        = "Use the seqinr package to read SFF files.",
           "sff.txt"    = "Use the seqinr package to read SFF text files.",
           "qual"       = "Use the ShortRead package to read QUAL files.",

           # Expression and annotation files
           "gct"        = "Use read.table() to read GCT files.",
           "res"        = "Use read.table() to read RES files.",
           "cls"        = "Use readLines() to read CLS files.",
           "gmt"        = "Use the GSEABase package to read GMT files.",
           "grp"        = "Use readLines() to read gene set files.",
           "saf"        = "Use the edgeR package to read SAF files.",

           # Compressed files
           "gz"         = "Use gzfile() to read GZIP compressed files.",
           "bz2"        = "Use bzfile() to read Bzip2 compressed files.",
           "xz"         = "Use xzfile() to read XZ compressed files.",
           "zip"        = "Use unzip() to extract ZIP archive files.",
           "tar"        = "Use untar() to extract TAR archive files.",
           "tar.gz"     = "Use untar() to extract compressed TAR files.",

           # Image files
           "tif"        = "Use the tiff package to read TIFF images.",
           "tiff"       = "Use the tiff package to read TIFF images.",
           "png"        = "Use the png package to read PNG images.",
           "jpeg"       = "Use the jpeg package to read JPEG images.",

           # Excel and text files
           "csv"        = "Use read.csv() or the data.table package.",
           "tsv"        = "Use read.table() or the data.table package with sep='\\t'.",
           "txt"        = "Use read.table() or readLines().",
           "xls"        = "Use the readxl package to read Excel files.",
           "xlsx"       = "Use the readxl package to read Excel files.",

           # PLINK files
           "ped"        = "Use the snpStats or PLINK package to read PED files.",
           "map"        = "Use the snpStats or PLINK package to read MAP files.",
           "tped"       = "Use the snpStats or PLINK package to read TPED files.",
           "tfam"       = "Use the snpStats or PLINK package to read TFAM files.",
           "bedplk"     = "Use the snpStats or PLINK package to read binary PED files.",

           # R data files
           "rdata"      = "Use load() to read RData files.",
           "rda"        = "Use load() to read RData files.",
           "rds"        = "Use readRDS() function.",

           # Miscellaneous files
           "maf"        = "Use the maftools package to read MAF files.",
           "maf.gz"     = "Use the maftools package to read compressed MAF files.",
           "mtx"        = "Use the Matrix package to read Matrix Market files.",
           "mtx.gz"     = "Use the Matrix package to read compressed Matrix Market files.",
           "snp"        = "Use appropriate SNP analysis packages like SNPRelate.",
           "vcf.idx"    = "Use VariantAnnotation for VCF index files.",
           "fai"        = "Use Rsamtools for FASTA index files.",
           "dict"       = "Sequence dictionary files; use appropriate tools.",
           "csi"        = "Use Rsamtools or VariantAnnotation for index files.",
           "bam.csi"    = "Use Rsamtools for BAM index files.",

           # Genomics data files
           "bw"         = "Use the rtracklayer package to read BigWig files.",
           "bw.gz"      = "Use the rtracklayer package to read compressed BigWig files.",
           "tdf"        = "Use the rtracklayer package to read TDF files.",

           # Other omics data files
           "cdf"        = "Use the affy package to read CDF files.",
           "psl"        = "Use read.table() to read PSL files.",
           "agp"        = "Use read.table() to read AGP files.",
           "ace"        = "Use the ape package to read ACE files.",
           "stockholm"  = "Use the Biostrings package to read Stockholm format files.",

           # Compressed archives
           "rar"        = "Use external tools to extract RAR files.",
           "7z"         = "Use external tools to extract 7z files.",

           # Custom formats
           "custom"     = "Custom file format; please provide a parser.",

           # Default case
           {
             warning(paste("Unknown extension:", ext, "- Please check the file format or consider adding support for it."))
             "Unknown extension. Please check the file format."
           }
    )
  })
  files_df$Recommendation <- recommendations
  return(files_df)
}
