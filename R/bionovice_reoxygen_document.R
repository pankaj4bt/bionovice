# open the console in bionovice, or any new package directory

# Generate Documentation
# From your package's root directory, run:
library(roxygen2)
library(devtools)
document()

# What This Does:
#
#   Processes the Roxygen2 comments.
# Generates .Rd files in the man/ directory.
# Updates the NAMESPACE file.
# 5. Check the Generated Files
# Documentation Files: Look inside the man/ directory of your package. You should see .Rd files corresponding to each of your documented functions.
#
# NAMESPACE File: Open the NAMESPACE file in your package's root directory. You'll see entries for exported functions and imported packages.
