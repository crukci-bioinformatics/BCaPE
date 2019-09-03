library(tidyverse)

copyNumberGainSensitivity <- read_tsv("CopyNumberGainSensitivity.txt")
copyNumberLossSensitivity <- read_tsv("CopyNumberLossSensitivity.txt")

p.values <- c(copyNumberGainSensitivity$p.value, copyNumberLossSensitivity$p.value)
fdr <- p.adjust(p.values, method = "BH")
fdr <- signif(fdr, digits = 3)

copyNumberGainSensitivity$fdr <- fdr[1:nrow(copyNumberGainSensitivity)]
copyNumberLossSensitivity$fdr <- fdr[(nrow(copyNumberGainSensitivity) + 1):length(fdr)]

write_tsv(copyNumberGainSensitivity, "CopyNumberGainSensitivity.adjusted.txt")
write_tsv(copyNumberLossSensitivity, "CopyNumberLossSensitivity.adjusted.txt")

