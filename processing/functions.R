library(readr)
library(tidyr)
library(dplyr)

readTabDelimitedFile <- function(filename, requiredColumns, removeOtherColumns = FALSE) {
  data <- read_tsv(filename)
  missingColumns <- setdiff(requiredColumns, colnames(data))
  if (length(missingColumns) > 0)
    stop(paste("Error: missing columns in file ", filename, ": ", paste(missingColumns, collapse = ", "), sep = ""))
  if (removeOtherColumns) data <- select(data, one_of(requiredColumns))
  return(data)
}

