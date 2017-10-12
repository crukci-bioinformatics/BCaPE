# R script for splitting a tab-delimited file into chunks each with a given
# number of lines where the header line is retained in each

# command line arguments
args <- commandArgs(trailing=TRUE)

if (length(args) != 4)
  stop("Usage: splitTabDelimitedFile.R file prefix suffix lines")

filename <- args[1]
prefix <- args[2]
suffix <- args[3]
lines <- as.integer(args[4])

library(readr)

i <- 0

writeFile <- function(data, pos)
{
	i <<- i + 1
	write_tsv(data, paste(prefix, i, suffix, sep = "."))
}

read_tsv_chunked(filename, SideEffectChunkCallback$new(writeFile), chunk_size = lines)

