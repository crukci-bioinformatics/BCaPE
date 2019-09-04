library(tidyverse)

# model classifications
modelClassifications <- read_tsv("ModelClassifications.txt", col_types = cols(
  Model = col_character(),
  ER = col_character(),
  HER2 = col_character(),
  PR = col_character(),
  BRCA1 = col_character(),
  Type = col_character(),
  iC10 = col_character(),
  `3-Gene` = col_character(),
  PAM50 = col_character()
)) %>%
  select(Model, ER, HER2, PR, BRCA1, Type, iC10, `3-Gene`, PAM50)

# drug annotations
drugAnnotations <- read_tsv("DrugAnnotations.txt", col_types = cols(
  Drug = col_character(),
  Pathway = col_character(),
  Target = col_character()
)) %>%
  select(Drug, Pathway, Target)

# gene details
geneDetails <- read_tsv("GeneDetails.txt", col_types = cols(
  Symbol = col_character(),
  Aliases = col_character(),
  Name = col_character()
)) %>%
  select(Gene = Symbol, Aliases, Name)

# drug sensitivity data
drugSensitivity <- read_tsv("DrugResponsesAUCModels.txt", col_types = cols(
  Model = col_character(),
  Drug = col_character(),
  AUC = col_double()
)) %>%
  select(Model, Drug, AUC) %>%
  filter(!is.na(AUC)) %>%
  mutate(AUC = signif(AUC, digits = 3))

# mutation data
mutations <- read_tsv("SNVsModels.txt") %>%
  gather(key = Model, value = Mutation, -Gene) %>%
  mutate(Mutated = Mutation != "NO") %>%
  select(Gene, Model, Mutated, Mutation) %>%
  separate_rows(Mutation, sep = "//") %>%
  mutate(Mutation = ifelse(Mutated, Mutation, NA)) %>%
  separate(Mutation, c("Chromosome", "Position", "Reference allele", "Alternate allele", "Type"), sep = "[:/_ ]", extra = "drop") %>%
  mutate(Type = ifelse(Type == "stoploss", "stop lost", Type)) %>%
  mutate(Type = ifelse(Type == "stopgain", "stop gained", Type)) %>%
  mutate(Type = ifelse(Type == "nonsynonymous", "non-synonymous", Type)) %>%
  mutate(Type = ifelse(Type == "nonframeshift", "non-frameshift", Type))

# expression data
expression <- read_tsv("ExpressionModels.txt") %>%
  gather(key = Model, value = Expression, -Gene) %>%
  filter(!is.na(Expression)) %>%
  mutate(Expression = signif(Expression, digits = 3))

# methylation data
methylation <- read_tsv("PromoterMethylationModels.txt") %>%
  gather(key = Model, value = Methylation, -Gene) %>%
  filter(!is.na(Methylation)) %>%
  mutate(Methylation = signif(Methylation, digits = 3))

# copy number states
copyNumberStates <- read_tsv("CNAModels.txt") %>%
  gather("Model", "State", -Gene) %>%
  filter(!is.na(State)) %>%
  mutate(State = ifelse(State == "NEUT", "NEUTRAL", State)) %>%
  mutate(State = tolower(State))

# sensitivity analysis results
mutationSensitivity <- read_tsv("MutationSensitivity.txt", col_types = cols(
  Gene = col_character(),
  Drug = col_character(),
  t.statistic = col_double(),
  p.value = col_double(),
  mean.mutation = col_double(),
  n.mutation = col_integer(),
  mean.nomutation = col_double(),
  n.nomutation = col_integer(),
  fdr = col_double()
))

expressionSensitivity <- read_tsv("ExpressionSensitivity.txt", col_types = cols(
  Gene = col_character(),
  Drug = col_character(),
  p.value = col_double(),
  slope = col_double(),
  range.auc = col_double(),
  range.expression = col_double(),
  n = col_integer(),
  fdr = col_double()
))

methylationSensitivity <- read_tsv("MethylationSensitivity.txt", col_types = cols(
  Gene = col_character(),
  Drug = col_character(),
  p.value = col_double(),
  slope = col_double(),
  range.auc = col_double(),
  range.methylation = col_double(),
  n = col_integer(),
  fdr = col_double()
))

copyNumberGainSensitivity <- read_tsv("CopyNumberGainSensitivity.adjusted.txt", col_types = cols(
  Gene = col_character(),
  Drug = col_character(),
  t.statistic = col_double(),
  p.value = col_double(),
  mean.gain = col_double(),
  n.gain = col_integer(),
  mean.nogain = col_double(),
  n.nogain = col_integer(),
  fdr = col_double()
))

copyNumberLossSensitivity <- read_tsv("CopyNumberLossSensitivity.adjusted.txt", col_types = cols(
  Gene = col_character(),
  Drug = col_character(),
  t.statistic = col_double(),
  p.value = col_double(),
  mean.loss = col_double(),
  n.loss = col_integer(),
  mean.noloss = col_double(),
  n.noloss = col_integer(),
  fdr = col_double()
))

# retain details of only the genes assessed by the various omics analyses
genes <- bind_rows(
  mutations %>% select(Gene),
  expression %>% select(Gene),
  methylation %>% select(Gene),
  copyNumberStates %>% select(Gene)
) %>%
  distinct

geneDetails <- geneDetails %>%
  right_join(genes, by = "Gene") %>%
  arrange(Gene)

# add display column to the gene details table
geneDetails <- geneDetails %>%
  mutate(
    Details = str_trim(str_c(
      str_replace_na(Name, ""),
      ifelse(is.na(Aliases), "", str_c("[", Aliases, "]")),
      sep = ifelse(is.na(Name), "", " ")
    ))
  )

# add logical columns for availability of various data types
geneDetails <- geneDetails %>%
  left_join(copyNumberStates %>% select(Gene) %>% distinct %>% mutate(CopyNumberState = TRUE), by = "Gene") %>%
  left_join(copyNumberGainSensitivity %>% select(Gene) %>% distinct %>% mutate(CopyNumberGainSensitivity = TRUE), by = "Gene") %>%
  left_join(copyNumberLossSensitivity %>% select(Gene) %>% distinct %>% mutate(CopyNumberLossSensitivity = TRUE), by = "Gene") %>%
  left_join(mutations %>% select(Gene) %>% distinct %>% mutate(Mutation = TRUE), by = "Gene") %>%
  left_join(mutationSensitivity %>% select(Gene) %>% distinct %>% mutate(MutationSensitivity = TRUE), by = "Gene") %>%
  left_join(expression %>% select(Gene) %>% distinct %>% mutate(Expression = TRUE), by = "Gene") %>%
  left_join(expressionSensitivity %>% select(Gene) %>% distinct %>% mutate(ExpressionSensitivity = TRUE), by = "Gene") %>%
  left_join(methylation %>% select(Gene) %>% distinct %>% mutate(Methylation = TRUE), by = "Gene") %>%
  left_join(methylationSensitivity %>% select(Gene) %>% distinct %>% mutate(MethylationSensitivity = TRUE), by = "Gene") %>%
  mutate_if(is.logical, replace_na, FALSE)

# create sqlite3 database
db <- src_sqlite("bcape.sqlite", create = TRUE)

copy_to(db, modelClassifications, temporary = FALSE, indexes = list("Model"))
copy_to(db, drugAnnotations, temporary = FALSE, indexes = list("Drug"))
copy_to(db, geneDetails, temporary = FALSE, indexes = list("Gene"))

copy_to(db, drugSensitivity, temporary = FALSE, indexes = list("Drug", "Model"))

copy_to(db, copyNumberStates, temporary = FALSE, indexes = list("Gene", "Model"))
copy_to(db, copyNumberGainSensitivity, temporary = FALSE, indexes = list("Gene", "Drug"))
copy_to(db, copyNumberLossSensitivity, temporary = FALSE, indexes = list("Gene", "Drug"))

copy_to(db, mutations, temporary = FALSE, indexes = list("Gene", "Model"))
copy_to(db, mutationSensitivity, temporary = FALSE, indexes = list("Gene", "Drug"))

copy_to(db, expression, temporary = FALSE, indexes = list("Gene", "Model"))
copy_to(db, expressionSensitivity, temporary = FALSE, indexes = list("Gene", "Drug"))

copy_to(db, methylation, temporary = FALSE, indexes = list("Gene", "Model"))
copy_to(db, methylationSensitivity, temporary = FALSE, indexes = list("Gene", "Drug"))

