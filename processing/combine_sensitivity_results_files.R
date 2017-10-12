# R script for combining sensitivity analysis results contained in multiple files
# recomputing the FDR accounting for multiple testing

# command line arguments
args <- commandArgs(trailing=TRUE)

if (length(args) < 3)
  stop("Usage: combine_sensitivity_results_files.R combined_file results_files")


library(readr)
library(dplyr)

combinedFilename <- args[1]

combinedData <- NULL

for (i in 2:length(args))
{
	filename <- args[i]
  cat(filename, "\n")
	data <- read_tsv(filename)
	combinedData <- bind_rows(combinedData, data)
}

combinedData$fdr <- p.adjust(combinedData$p.value, method = "BH")

write_tsv(combinedData, combinedFilename)

