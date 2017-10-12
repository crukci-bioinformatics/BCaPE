library(tidyr)
library(dplyr)
library(shiny)
library(highcharter)
library(DT)

source("plots.R")

db <- src_sqlite("data/bcape.sqlite")

modelClassifications <- collect(tbl(db, "modelClassifications"))

drugAnnotations <- collect(tbl(db, "drugAnnotations"))
drugSensitivity <- collect(tbl(db, "drugSensitivity"))

modelSelectionColumns <- c("Model", "iC10", "3-Gene", "PAM50")
modelSelection <- modelClassifications %>%
  select(one_of(modelSelectionColumns))
restrictedModelSelection <- modelSelection %>%
  right_join(drugSensitivity %>% select(Model) %>% unique, by = "Model")

drugSelection <- drugSensitivity %>%
  select(Drug) %>%
  distinct %>%
  left_join(drugAnnotations, by = "Drug")

copyNumberStates <- tbl(db, "copyNumberStates")
copyNumberGainSensitivity <- tbl(db, "copyNumberGainSensitivity")
copyNumberLossSensitivity <- tbl(db, "copyNumberLossSensitivity")

mutations <- tbl(db, "mutations")
mutationSensitivity <- tbl(db, "mutationSensitivity")

expression <- tbl(db, "expression")
expressionSensitivity <- tbl(db, "expressionSensitivity")

methylation <- tbl(db, "methylation")
methylationSensitivity <- tbl(db, "methylationSensitivity")

drugVolcanoPlotTopN <- 500
drugVolcanoPlotSampleN <- 500
drugVolcanoPlotMaxGenes <- drugVolcanoPlotTopN + drugVolcanoPlotSampleN

slice_and_sample <- function(data, n_slice = 500, n_sample = 500)
{
  n <- n_slice + n_sample
  if (nrow(data) > n)
  {
    data <- bind_rows(
      data %>% slice(1:n_slice),
      data %>% slice((n_slice + 1):nrow(data)) %>% sample_n(n_sample)
    )
  }
  return(data)
}

