# R script for performing sensitivity analysis on gene copy number data
# in relation to drug responsiveness in PDTX models

# command line arguments
args <- commandArgs(trailing=TRUE)

if (length(args) != 3)
  stop("Usage: copy_number_loss_sensitivity.R drug_sensitivity_file copy_number_state_file output_file")

drugSensitivityFilename <- args[1]
copyNumberFilename <- args[2]
outputFilename <- args[3]


library(tidyverse)
library(furrr)
library(tictoc)

plan(multiprocess)

availableCores()


# drug sensitivity data
drugSensitivity <- read_tsv(drugSensitivityFilename) %>%
  select(Model, Drug, AUC)


# copy number states
copyNumberStates <- read_tsv(copyNumberFilename)


# copy number loss sensitivity analysis

copyNumberLoss <- copyNumberStates %>%
  gather("Model", "State", -Gene) %>%
  mutate(State = ifelse(State == "NEUT", "NEUTRAL", State)) %>%
  mutate(State = tolower(State)) %>%
  filter(State %in% c("gain", "neutral", "loss")) %>%
  transmute(Gene, Model, Loss = State == "loss") %>%
  semi_join(drugSensitivity, by = "Model")

calculateSensitivity <- function(gene) {

  geneCopyNumberLossSensitivity <- copyNumberLoss %>%
    filter(Gene == gene) %>%
    inner_join(drugSensitivity, by = "Model")

  calculateDrugSensitivity <- function(drug) {

    geneDrugCopyNumberLossSensitivity <- filter(geneCopyNumberLossSensitivity, Drug == drug)

    result <- try(t.test(AUC ~ Loss, data = geneDrugCopyNumberLossSensitivity))

    if (class(result) == "try-error")
      stop("Error in t-test for gene ", gene, " and drug ", drug, "\n")

    tibble(
      Gene = gene,
      Drug = drug,
      t.statistic = result$statistic,
      p.value = result$p.value,
      mean.loss = result$estimate["mean in group TRUE"],
      n.loss = geneDrugCopyNumberLossSensitivity %>% filter(Loss) %>% nrow(),
      mean.noloss = result$estimate["mean in group FALSE"],
      n.noloss = geneDrugCopyNumberLossSensitivity %>% filter(!Loss) %>% nrow()
    )
  }

  geneCopyNumberLossSensitivity %>%
    count(Drug, Loss) %>%
    filter(n >= 2) %>%
    select(-n) %>%
    count(Drug) %>%
    filter(n == 2) %>%
    .$Drug %>%
    map_dfr(calculateDrugSensitivity)
}


tic()

copyNumberLossSensitivity <- copyNumberLoss %>%
  count(Gene, Loss) %>%
  filter(n >= 2) %>%
  select(-n) %>%
  count(Gene) %>%
  filter(n >= 2) %>%
  .$Gene %>%
  future_map_dfr(calculateSensitivity, .progress = TRUE)

toc()


# correction for multiple testing
copyNumberLossSensitivity$fdr <- p.adjust(copyNumberLossSensitivity$p.value, method = "BH")


# write results
copyNumberLossSensitivity %>%
  mutate_at(vars(t.statistic, p.value, mean.loss, mean.noloss, fdr), signif, digits = 3) %>%
  write_tsv(outputFilename)


