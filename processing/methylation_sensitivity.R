# R script for performing sensitivity analysis on gene methylation data
# in relation to drug responsiveness in PDTX models

# command line arguments
args <- commandArgs(trailing=TRUE)

if (length(args) != 3)
  stop("Usage: methylation_sensitivity.R drug_sensitivity_file methylation_data_file output_file")

drugSensitivityFilename <- args[1]
methylationFilename <- args[2]
outputFilename <- args[3]


source("functions.R")


# drug sensitivity data
drugSensitivity <- readTabDelimitedFile(drugSensitivityFilename, c("Model", "Drug", "AUC"), removeOtherColumns = TRUE)


# methylation data
methylation <- readTabDelimitedFile(methylationFilename, "Gene")

methylation <- methylation %>%
  gather(key = Model, value = Methylation, -Gene)

methylation <- methylation %>%
  semi_join(drugSensitivity, by = "Model") %>%
  filter(!is.na(Methylation))


# methylation sensitivity analysis

methylationSensitivity <- data_frame(
  Gene = character(),
  Drug = character(),
  p.value = double(),
  slope = double(),
  range.auc = double(),
  range.methylation = double(),
  n = integer()
)

drugs <- drugSensitivity %>%
  select(Drug) %>%
  arrange(Drug) %>%
  unique %>%
  unlist(use.names = FALSE)

genes <- methylation %>%
  select(Gene) %>%
  arrange(Gene) %>%
  unique %>%
  unlist(use.names = FALSE)

geneCount <- 0
for (gene in genes)
{
  geneCount <- geneCount + 1
  cat(geneCount, "/", length(genes), "\n", sep = "")

  geneMethylationSensitivity <- methylation %>%
    filter(Gene == gene) %>%
    select(-Gene) %>%
    inner_join(drugSensitivity, by = "Model")

  for (drug in drugs)
  {
    geneDrugMethylationSensitivity <- filter(geneMethylationSensitivity, Drug == drug)
    if (nrow(geneDrugMethylationSensitivity) < 3) next

    result <- try(lm(AUC ~ Methylation, data = geneDrugMethylationSensitivity))

    if (class(result) != "try-error")
    {
      coefficients <- summary(result)$coefficients
      if (nrow(coefficients) != 2) next

      methylationSensitivity <- methylationSensitivity %>% bind_rows(data_frame(
        Gene = gene,
        Drug = drug,
        p.value = coefficients[2, 4],
        slope = coefficients[2, 1],
        range.auc = diff(range(result$model$AUC)),
        range.methylation = diff(range(result$model$Methylation)),
        n = nrow(geneDrugMethylationSensitivity)
      ))
    }
  }
}

methylationSensitivity$fdr <- p.adjust(methylationSensitivity$p.value, method = "BH")


# write results
write_tsv(methylationSensitivity, outputFilename)


