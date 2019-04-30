library(tidyverse)

copyNumberGainSensitivity <- read_tsv("CopyNumberGainSensitivity.txt")
copyNumberLossSensitivity <- read_tsv("CopyNumberLossSensitivity.txt")

p.values <- c(copyNumberGainSensitivity$p.value, copyNumberLossSensitivity$p.value)
fdr <- p.adjust(p.values, method = "BH")
fdr <- signif(fdr, digits = 3)

copyNumberGainSensitivity$fdr <- fdr[1:nrow(copyNumberGainSensitivity)]
copyNumberLossSensitivity$fdr <- fdr[(nrow(copyNumberGainSensitivity) + 1):length(fdr)]

copyNumberGainSensitivity %>%
  mutate_at(vars(t.statistic, p.value, mean.gain, mean.nogain, fdr), signif, digits = 3) %>%
  write_tsv("CopyNumberGainSensitivity.txt")

copyNumberLossSensitivity %>%
  mutate_at(vars(t.statistic, p.value, mean.loss, mean.noloss, fdr), signif, digits = 3) %>%
  write_tsv("CopyNumberLossSensitivity.txt")

