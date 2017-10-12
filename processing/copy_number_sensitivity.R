# R script for performing sensitivity analysis on gene copy number data
# in relation to drug responsiveness in PDTX models

# command line arguments
args <- commandArgs(trailing=TRUE)

if (length(args) != 4)
  stop("Usage: copy_number_sensitivity.R drug_sensitivity_file copy_number_data_file gain_sensitivity_output_file loss_sensitivity_output_file")

drugSensitivityFilename <- args[1]
copyNumberFilename <- args[2]
gainSensitivityOutputFilename <- args[3]
lossSensitivityOutputFilename <- args[4]


source("functions.R")


# drug sensitivity data
drugSensitivity <- readTabDelimitedFile(drugSensitivityFilename, c("Model", "Drug", "AUC"), removeOtherColumns = TRUE)


# copy number states
copyNumberStates <- readTabDelimitedFile(copyNumberFilename, "Symbol")

copyNumberStates <- copyNumberStates %>%
  gather("Model", "State", -Symbol) %>%
  rename(Gene = Symbol) %>%
  mutate(State = ifelse(State == "NEUT", "NEUTRAL", State)) %>%
  mutate(State = tolower(State))

copyNumberStates <- copyNumberStates %>%
  semi_join(drugSensitivity, by = "Model") %>%
  filter(State %in% c("gain", "neutral", "loss"))


# copy number sensitivity analysis

copyNumberGainSensitivity <- data_frame(
  Gene = character(),
  Drug = character(),
  t.statistic = double(),
  p.value = double(),
  mean.gain = double(),
  n.gain = integer(),
  mean.nogain = double(),
  n.nogain = integer()
)

copyNumberLossSensitivity <- data_frame(
  Gene = character(),
  Drug = character(),
  t.statistic = double(),
  p.value = double(),
  mean.loss = double(),
  n.loss = integer(),
  mean.noloss = double(),
  n.noloss = integer()
)

drugs <- drugSensitivity %>%
  select(Drug) %>%
  arrange(Drug) %>%
  unique %>%
  unlist(use.names = FALSE)

genes <- copyNumberStates %>%
  select(Gene) %>%
  arrange(Gene) %>%
  unique %>%
  unlist(use.names = FALSE)

geneCount <- 0
for (gene in genes)
{
  geneCount <- geneCount + 1
  cat(geneCount, "/", length(genes), "\n", sep = "")

  geneCopyNumberStates <- copyNumberStates %>%
    filter(Gene == gene) %>%
    select(-Gene)

  counts <- geneCopyNumberStates %>% count(State)
  if (nrow(counts) < 2) next

  geneCopyNumberSensitivity <- inner_join(geneCopyNumberStates, drugSensitivity, by = "Model")
  if (nrow(geneCopyNumberSensitivity) == 0) next

  for (drug in drugs)
  {
    geneDrugCopyNumberSensitivity <- geneCopyNumberSensitivity %>% filter(Drug == drug)
    if (nrow(geneDrugCopyNumberSensitivity) == 0) next

    geneDrugCopyNumberSensitivity <- geneDrugCopyNumberSensitivity %>%
      mutate(Gain = (State == "gain")) %>%
      mutate(Loss = (State == "loss"))

    n.gain <- geneDrugCopyNumberSensitivity %>% filter(Gain) %>% nrow
    n.nogain <- geneDrugCopyNumberSensitivity %>% filter(!Gain) %>% nrow
    if (n.gain >= 2 && n.nogain >= 2)
    {
      result <- try(t.test(AUC ~ Gain, data = geneDrugCopyNumberSensitivity))
      if (class(result) != "try-error")
      {
        copyNumberGainSensitivity <- copyNumberGainSensitivity %>% bind_rows(data_frame(
          Gene = gene,
          Drug = drug,
          t.statistic = result$statistic,
          p.value = result$p.value,
          mean.gain = result$estimate["mean in group TRUE"],
          n.gain = n.gain,
          mean.nogain = result$estimate["mean in group FALSE"],
          n.nogain = n.nogain
        ))
      }
    }

    n.loss <- geneDrugCopyNumberSensitivity %>% filter(Loss) %>% nrow
    n.noloss <- geneDrugCopyNumberSensitivity %>% filter(!Loss) %>% nrow
    if (n.loss >= 2 && n.noloss >= 2)
    {
      result <- try(t.test(AUC ~ Loss, data = geneDrugCopyNumberSensitivity))
      if (class(result) != "try-error")
      {
        copyNumberLossSensitivity <- copyNumberLossSensitivity %>% bind_rows(data_frame(
          Gene = gene,
          Drug = drug,
          t.statistic = result$statistic,
          p.value = result$p.value,
          mean.loss = result$estimate["mean in group TRUE"],
          n.loss = n.loss,
          mean.noloss = result$estimate["mean in group FALSE"],
          n.noloss = n.noloss
        ))
      }
    }
  }
}

p.values <- c(copyNumberGainSensitivity$p.value, copyNumberLossSensitivity$p.value)
adjusted.p.values <- p.adjust(p.values, method = "BH")
copyNumberGainSensitivity$fdr <- adjusted.p.values[1:nrow(copyNumberGainSensitivity)]
copyNumberLossSensitivity$fdr <- adjusted.p.values[(nrow(copyNumberGainSensitivity) + 1):length(adjusted.p.values)]


# write results
write_tsv(copyNumberGainSensitivity, gainSensitivityOutputFilename)
write_tsv(copyNumberLossSensitivity, lossSensitivityOutputFilename)


