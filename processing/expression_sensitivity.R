# R script for performing sensitivity analysis on gene expression data
# in relation to drug responsiveness in PDTX models

# command line arguments
args <- commandArgs(trailing=TRUE)

if (length(args) != 3)
  stop("Usage: expression_sensitivity.R drug_sensitivity_file expression_file output_file")

drugSensitivityFilename <- args[1]
expressionFilename <- args[2]
outputFilename <- args[3]


library(tidyverse)
library(furrr)
library(tictoc)

plan(multiprocess)

availableCores()


# drug sensitivity data
drugSensitivity <- read_tsv(drugSensitivityFilename) %>%
  select(Model, Drug, AUC)


# expression data
expression <- read_tsv(expressionFilename)

expression <- expression %>%
  gather(key = Model, value = Expression, -Gene)

expression <- expression %>%
  semi_join(drugSensitivity, by = "Model") %>%
  filter(!is.na(Expression))


# sensitivity analysis

calculateSensitivity <- function(gene) {

  geneExpressionSensitivity <- expression %>%
    filter(Gene == gene) %>%
    select(-Gene) %>%
    inner_join(drugSensitivity, by = "Model")

  calculateDrugSensitivity <- function(drug) {

    geneDrugExpressionSensitivity <- filter(geneExpressionSensitivity, Drug == drug)

    result <- try(lm(AUC ~ Expression, data = geneDrugExpressionSensitivity))

    if (class(result) == "try-error")
      stop("Error creating linear model for gene ", gene, " and drug ", drug, "\n")

    coefficients <- summary(result)$coefficients

    if (nrow(coefficients) != 2)
      stop("Unexpected number of coefficients in linear model for gene ", gene, " and drug ", drug, ": ", nrow(coefficients), "\n")

    tibble(
      Gene = gene,
      Drug = drug,
      p.value = coefficients[2, 4],
      slope = coefficients[2, 1],
      range.auc = diff(range(result$model$AUC)),
      range.expression = diff(range(result$model$Expression)),
      n = nrow(geneDrugExpressionSensitivity)
    )
  }

  geneExpressionSensitivity %>%
    count(Drug) %>%
    filter(n >= 3) %>%
    .$Drug %>%
    map_dfr(calculateDrugSensitivity)
}

tic()

expressionSensitivity <- expression %>%
  distinct(Gene) %>%
  arrange(Gene) %>%
  .$Gene %>%
  future_map_dfr(calculateSensitivity, .progress = TRUE)

toc()


# correction for multiple testing
expressionSensitivity$fdr <- p.adjust(expressionSensitivity$p.value, method = "BH")


# write results
expressionSensitivity %>%
  mutate_at(vars(p.value, slope, range.auc, range.expression, fdr), signif, digits = 3) %>%
  write_tsv(outputFilename)


