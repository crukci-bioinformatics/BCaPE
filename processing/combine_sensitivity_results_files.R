# R script for combining sensitivity analysis results contained in multiple files
# recomputing the FDR accounting for multiple testing

# command line arguments
args <- commandArgs(trailing=TRUE)

if (length(args) < 3)
  stop("Usage: combine_sensitivity_results_files.R combined_file results_files")

combinedFilename <- args[1]

source("functions.R")

library(dplyr)

combinedData <- NULL

for (i in 2:length(args))
{
  filename <- args[i]
  cat(filename, "\n")
  data <- readTabDelimitedFile(filename, c("Gene", "Drug", "p.value"), removeOtherColumns = FALSE)
  combinedData <- bind_rows(combinedData, data)
}

combinedData <- arrange(combinedData, Gene, Drug)

combinedData$fdr <- p.adjust(combinedData$p.value, method = "BH")

write_tsv(combinedData, combinedFilename)

