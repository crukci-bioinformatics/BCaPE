# R script for performing sensitivity analysis on gene copy number data
# in relation to drug responsiveness in PDTX models

# command line arguments
args <- commandArgs(trailing=TRUE)

if (length(args) != 3)
  stop("Usage: copy_number_gain_sensitivity.R drug_sensitivity_file copy_number_state_file output_file")

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


# copy number gain sensitivity analysis

copyNumberGain <- copyNumberStates %>%
  gather("Model", "State", -Gene) %>%
  mutate(State = ifelse(State == "NEUT", "NEUTRAL", State)) %>%
  mutate(State = tolower(State)) %>%
  filter(State %in% c("gain", "neutral", "loss")) %>%
  transmute(Gene, Model, Gain = State == "gain") %>%
  semi_join(drugSensitivity, by = "Model")

calculateSensitivity <- function(gene) {

  geneCopyNumberGainSensitivity <- copyNumberGain %>%
    filter(Gene == gene) %>%
    inner_join(drugSensitivity, by = "Model")

  calculateDrugSensitivity <- function(drug) {

    geneDrugCopyNumberGainSensitivity <- filter(geneCopyNumberGainSensitivity, Drug == drug)

    result <- try(t.test(AUC ~ Gain, data = geneDrugCopyNumberGainSensitivity))

    if (class(result) == "try-error")
      stop("Error in t-test for gene ", gene, " and drug ", drug, "\n")

    tibble(
      Gene = gene,
      Drug = drug,
      t.statistic = result$statistic,
      p.value = result$p.value,
      mean.gain = result$estimate["mean in group TRUE"],
      n.gain = geneDrugCopyNumberGainSensitivity %>% filter(Gain) %>% nrow(),
      mean.nogain = result$estimate["mean in group FALSE"],
      n.nogain = geneDrugCopyNumberGainSensitivity %>% filter(!Gain) %>% nrow()
    )
  }

  geneCopyNumberGainSensitivity %>%
    count(Drug, Gain) %>%
    filter(n >= 2) %>%
    select(-n) %>%
    count(Drug) %>%
    filter(n == 2) %>%
    .$Drug %>%
    map_dfr(calculateDrugSensitivity)
}


tic()

copyNumberGainSensitivity <- copyNumberGain %>%
  count(Gene, Gain) %>%
  filter(n >= 2) %>%
  select(-n) %>%
  count(Gene) %>%
  filter(n >= 2) %>%
  .$Gene %>%
  future_map_dfr(calculateSensitivity, .progress = TRUE)

toc()


# correction for multiple testing
copyNumberGainSensitivity$fdr <- p.adjust(copyNumberGainSensitivity$p.value, method = "BH")


# write results
copyNumberGainSensitivity %>%
  mutate_at(vars(t.statistic, p.value, mean.gain, mean.nogain, fdr), signif, digits = 3) %>%
  write_tsv(outputFilename)


