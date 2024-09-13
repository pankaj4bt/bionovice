# bionovice package

[![R-CMD-check](https://github.com/pankaj4bt/bionovice/actions/workflows/R-CMD-check.yml/badge.svg)](https://github.com/pankaj4bt/bionovice/actions/workflows/R-CMD-check.yml)

An R package designed to simplify the process of identifying bioinformatics file formats and recommending appropriate packages or methods to open or analyze them.

## Overview

Working with various file formats is a common challenge in bioinformatics, especially for newcomers to R. This package automates the initial steps by:

- Scanning a specified folder for files.
- Extracting file extensions.
- Providing recommendations on which R packages or methods to use for each file type.

## Installation

Install the package directly from GitHub using the `devtools` package:

```R
# Install devtools if not already installed
install.packages("devtools")

# Install the bionovice
 package from GitHub
library(devtools)
install_github("pankaj4bt/bionovice")


