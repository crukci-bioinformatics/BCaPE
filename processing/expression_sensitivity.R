source("functions.R")


# drug sensitivity data
drugSensitivity <- readTabDelimitedFile("DrugResponsesAUCModels.txt", c("Model", "Drug", "AUC"), removeOtherColumns = TRUE)


# expression data
expression <- readTabDelimitedFile("ExpressionModels.txt", "Gene")

expression <- expression %>%
  gather(key = Model, value = Expression, -Gene)

expression <- expression %>%
  semi_join(drugSensitivity, by = "Model") %>%
  filter(!is.na(Expression))


# expression sensitivity analysis

expressionSensitivity <- data_frame(
  Gene = character(),
  Drug = character(),
  p.value = double(),
  slope = double(),
  range.auc = double(),
  range.expression = double(),
  n = integer()
)

drugs <- drugSensitivity %>%
  select(Drug) %>%
  arrange(Drug) %>%
  unique %>%
  unlist(use.names = FALSE)

genes <- expression %>%
  select(Gene) %>%
  arrange(Gene) %>%
  unique %>%
  unlist(use.names = FALSE)

geneCount <- 0
for (gene in genes)
{
  geneCount <- geneCount + 1
  cat(geneCount, "/", length(genes), "\n", sep = "")

  geneExpressionSensitivity <- expression %>%
    filter(Gene == gene) %>%
    select(-Gene) %>%
    inner_join(drugSensitivity, by = "Model")

  for (drug in drugs)
  {
    geneDrugExpressionSensitivity <- filter(geneExpressionSensitivity, Drug == drug)
    if (nrow(geneDrugExpressionSensitivity) < 3) next

    result <- try(lm(AUC ~ Expression, data = geneDrugExpressionSensitivity))

    if (class(result) != "try-error")
    {
      coefficients <- summary(result)$coefficients
      if (nrow(coefficients) != 2) next

      expressionSensitivity <- expressionSensitivity %>% bind_rows(data_frame(
        Gene = gene,
        Drug = drug,
        p.value = coefficients[2, 4],
        slope = coefficients[2, 1],
        range.auc = diff(range(result$model$AUC)),
        range.expression = diff(range(result$model$Expression)),
        n = nrow(geneDrugExpressionSensitivity)
      ))
    }
  }
}

expressionSensitivity$fdr <- p.adjust(expressionSensitivity$p.value, method = "BH")


# write results
write_tsv(expressionSensitivity, "ExpressionSensitivity.txt")


