# R script for performing sensitivity analysis using mutations within genes
# in relation to drug responsiveness in PDTX models

# command line arguments
args <- commandArgs(trailing=TRUE)

if (length(args) != 3)
  stop("Usage: mutation_sensitivity.R drug_sensitivity_file mutation_file output_file")

drugSensitivityFilename <- args[1]
mutationsFilename <- args[2]
outputFilename <- args[3]


library(tidyverse)
library(furrr)
library(tictoc)

plan(multiprocess)

availableCores()


# drug sensitivity data
drugSensitivity <- read_tsv(drugSensitivityFilename) %>%
  select(Model, Drug, AUC)


# mutation data
mutations <- read_tsv(mutationsFilename)

mutations <- mutations %>%
  gather(key = Model, value = Mutation, -Gene) %>%
  transmute(Gene, Model, Mutated = Mutation != "NO")


# sensitivity analysis

calculateSensitivity <- function(gene) {

  geneMutationSensitivity <- mutations %>%
    filter(Gene == gene) %>%
    inner_join(drugSensitivity, by = "Model")

  calculateDrugSensitivity <- function(drug) {

    geneDrugMutationSensitivity <- filter(geneMutationSensitivity, Drug == drug)

    result <- try(t.test(AUC ~ Mutated, data = geneDrugMutationSensitivity))

    if (class(result) == "try-error")
      stop("Error in t-test for gene ", gene, " and drug ", drug, "\n")

    tibble(
      Gene = gene,
      Drug = drug,
      t.statistic = result$statistic,
      p.value = result$p.value,
      mean.mutation = result$estimate["mean in group TRUE"],
      n.mutation = geneDrugMutationSensitivity %>% filter(Mutated) %>% nrow(),
      mean.nomutation = result$estimate["mean in group FALSE"],
      n.nomutation = geneDrugMutationSensitivity %>% filter(!Mutated) %>% nrow()
    )
  }

  geneMutationSensitivity %>%
    count(Drug, Mutated) %>%
    filter(n >= 2) %>%
    select(-n) %>%
    count(Drug) %>%
    filter(n == 2) %>%
    .$Drug %>%
    map_dfr(calculateDrugSensitivity)
}

tic()

mutationSensitivity <- mutations %>%
  count(Gene, Mutated) %>%
  filter(n >= 2) %>%
  select(-n) %>%
  count(Gene) %>%
  filter(n >= 2) %>%
  .$Gene %>%
  future_map_dfr(calculateSensitivity)

toc()


# correction for multiple testing
mutationSensitivity$fdr <- p.adjust(mutationSensitivity$p.value, method = "BH")


# write results
mutationSensitivity %>%
  mutate_at(vars(t.statistic, p.value, mean.mutation, mean.nomutation, fdr), signif, digits = 3) %>%
  write_tsv(outputFilename)


