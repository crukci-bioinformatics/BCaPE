# R script for performing sensitivity analysis on gene methylation data
# in relation to drug responsiveness in PDTX models

# command line arguments
args <- commandArgs(trailing=TRUE)

if (length(args) != 3)
  stop("Usage: methylation_sensitivity.R drug_sensitivity_file methylation_file output_file")

drugSensitivityFilename <- args[1]
methylationFilename <- args[2]
outputFilename <- args[3]


library(tidyverse)
library(furrr)
library(tictoc)

plan(multiprocess)

availableCores()


# drug sensitivity data
drugSensitivity <- read_tsv(drugSensitivityFilename) %>%
  select(Model, Drug, AUC)


# methylation data
methylation <- read_tsv(methylationFilename)

methylation <- methylation %>%
  gather(key = Model, value = Methylation, -Gene)

methylation <- methylation %>%
  semi_join(drugSensitivity, by = "Model") %>%
  filter(!is.na(Methylation))


# methylation sensitivity analysis

calculateSensitivity <- function(gene) {

  geneMethylationSensitivity <- methylation %>%
    filter(Gene == gene) %>%
    select(-Gene) %>%
    inner_join(drugSensitivity, by = "Model")

  calculateDrugSensitivity <- function(drug) {

    geneDrugMethylationSensitivity <- filter(geneMethylationSensitivity, Drug == drug)

    result <- try(lm(AUC ~ Methylation, data = geneDrugMethylationSensitivity))

    if (class(result) == "try-error")
      stop("Error creating linear model for gene ", gene, " and drug ", drug, "\n")

    coefficients <- summary(result)$coefficients

    if (nrow(coefficients) != 2) return(NULL)
#      stop("Unexpected number of coefficients in linear model for gene ", gene, " and drug ", drug, ": ", nrow(coefficients), "\n")

    tibble(
      Gene = gene,
      Drug = drug,
      p.value = coefficients[2, 4],
      slope = coefficients[2, 1],
      range.auc = diff(range(result$model$AUC)),
      range.methylation = diff(range(result$model$Methylation)),
      n = nrow(geneDrugMethylationSensitivity)
    )
  }

  geneMethylationSensitivity %>%
    count(Drug) %>%
    filter(n >= 3) %>%
    .$Drug %>%
    map_dfr(calculateDrugSensitivity)
}

tic()

methylationSensitivity <- methylation %>%
  distinct(Gene) %>%
  arrange(Gene) %>%
  .$Gene %>%
  future_map_dfr(calculateSensitivity, .progress = TRUE)

toc()


# correction for multiple testing
methylationSensitivity$fdr <- p.adjust(methylationSensitivity$p.value, method = "BH")


# write results
methylationSensitivity %>%
  mutate_at(vars(p.value, slope, range.auc, range.methylation, fdr), signif, digits = 3) %>%
  write_tsv(outputFilename)


