# bionovice package

[![R-CMD-check](https://github.com/pankaj4bt/bionovice/actions/workflows/R-CMD-check.yml/badge.svg)](https://github.com/pankaj4bt/bionovice/actions/workflows/R-CMD-check.yml)
[![test-coverage](https://github.com/pankaj4bt/bionovice/actions/workflows/test-coverage.yml/badge.svg)](https://github.com/pankaj4bt/bionovice/actions/workflows/test-coverage.yml)
[![lint](https://github.com/pankaj4bt/bionovice/actions/workflows/lint.yml/badge.svg)](https://github.com/pankaj4bt/bionovice/actions/workflows/lint.yml)

An R package designed to simplify the process of identifying bioinformatics file formats and recommending appropriate packages or methods to open or analyze them.

## Overview

Working with various file formats is a common challenge in bioinformatics, especially for newcomers to R. This package automates the initial steps by:

- Scanning a specified folder for files.
- Extracting file extensions.
- Providing recommendations on which R packages or methods to use for each file type.
- Validating file formats and structure.
- Extracting detailed metadata from bioinformatics files.

## Installation

Install the package directly from GitHub using the `devtools` package:

```R
# Install devtools if not already installed
install.packages("devtools")

# Install the bionovice package from GitHub
library(devtools)
install_github("pankaj4bt/bionovice")
```

## Basic Usage

### Analyze a folder of bioinformatics files

```R
library(bionovice)

# Basic analysis
result <- analyze_folder("path/to/your/bioinformatics/files")
print(result)

# High-performance analysis with parallel processing
result <- analyze_folder_parallel("path/to/folder", 
                                  parallel = TRUE, 
                                  include_metadata = TRUE,
                                  recursive = TRUE)

# Benchmark performance
benchmark <- benchmark_analysis("path/to/test/folder")
print(benchmark)
```

### Validate individual files

```R
# Validate a FASTA file
validation_result <- validate_file("sequences.fasta")
if (validation_result$valid) {
  print("File is valid!")
} else {
  print(paste("Issues found:", paste(validation_result$issues, collapse = ", ")))
}
```

### Extract detailed metadata

```R
# Extract comprehensive metadata from a file
metadata <- extract_file_metadata("data.fastq")

# View file information
print(metadata$file_info)

# View format-specific information
print(metadata$format_info)

# View content preview
print(metadata$content_preview)
```

## Supported File Formats

The package supports a wide range of bioinformatics file formats including:

- **Sequence files**: FASTA, FASTQ, AB1, SFF
- **Alignment files**: SAM, BAM, CRAM
- **Variant files**: VCF, BCF
- **Annotation files**: GFF, GTF, BED
- **Expression data**: H5AD, Loom, MTX
- **And many more...**

## Functions

- `analyze_folder()`: Main function to scan and analyze all files in a folder
- `analyze_folder_parallel()`: High-performance version with parallel processing and advanced options
- `scan_folder()`: Scan folder and extract file extensions
- `recommend_packages()`: Get package recommendations for file types
- `validate_file()`: Validate file format and structure
- `extract_file_metadata()`: Extract detailed metadata from files
- `benchmark_analysis()`: Benchmark performance of analysis operations

## Performance Features

The package includes several performance optimizations for handling large datasets:

- **Parallel processing**: Use multiple CPU cores for faster analysis
- **Recursive scanning**: Analyze entire directory trees
- **File filtering**: Process only files matching specific patterns
- **Metadata extraction**: Optional detailed analysis with format-specific statistics
- **Benchmarking tools**: Compare performance of different analysis methods

## Contributing

Contributions are welcome! Please feel free to submit issues or pull requests.
