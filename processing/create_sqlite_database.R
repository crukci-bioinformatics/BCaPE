source("functions.R")

# model classifications
modelClassifications <- readTabDelimitedFile("ModelClassifications.txt", c("Model", "ER", "HER2", "PR", "BRCA1", "Type", "iC10", "3-Gene", "PAM50"), removeOtherColumns = TRUE)

# drug annotations
drugAnnotations <- readTabDelimitedFile("DrugAnnotations.txt", c("Drug", "Pathway", "Target"), removeOtherColumns = TRUE)

# drug sensitivity data
drugSensitivity <- readTabDelimitedFile("DrugResponsesAUCModels.txt", c("Model", "Drug", "AUC"), removeOtherColumns = TRUE)

drugSensitivity <- drugSensitivity %>%
  mutate(AUC = signif(AUC, digits = 3))

# copy number states
copyNumberStates <- readTabDelimitedFile("CNAModels.txt", "Symbol")

copyNumberStates <- copyNumberStates %>%
  gather("Model", "State", -Symbol) %>%
  filter(!is.na(State)) %>%
  rename(Gene = Symbol) %>%
  mutate(State = ifelse(State == "NEUT", "NEUTRAL", State)) %>%
  mutate(State = tolower(State))

# copy number sensitivity data
copyNumberGainSensitivity <- readTabDelimitedFile("CopyNumberGainSensitivity.txt", c("Gene", "Drug", "t.statistic", "p.value", "mean.gain", "n.gain", "mean.nogain", "n.nogain", "fdr"))
copyNumberLossSensitivity <- readTabDelimitedFile("CopyNumberLossSensitivity.txt", c("Gene", "Drug", "t.statistic", "p.value", "mean.loss", "n.loss", "mean.noloss", "n.noloss", "fdr"))

p.values <- c(copyNumberGainSensitivity$p.value, copyNumberLossSensitivity$p.value)
adjusted.p.values <- p.adjust(p.values, method = "BH")
copyNumberGainSensitivity$fdr <- adjusted.p.values[1:nrow(copyNumberGainSensitivity)]
copyNumberLossSensitivity$fdr <- adjusted.p.values[(nrow(copyNumberGainSensitivity) + 1):length(adjusted.p.values)]

copyNumberGainSensitivity <- copyNumberGainSensitivity %>%
  mutate_at(vars(t.statistic, p.value, mean.gain, mean.nogain, fdr), funs(signif(., digits = 3)))

copyNumberLossSensitivity <- copyNumberLossSensitivity %>%
  mutate_at(vars(t.statistic, p.value, mean.loss, mean.noloss, fdr), funs(signif(., digits = 3)))

# mutation data
mutations <- readTabDelimitedFile("SNVsModels.txt", "Symbol")

mutations <- mutations %>%
  gather(key = Model, value = Mutation, -Symbol) %>%
  rename(Gene = Symbol) %>%
  mutate(Mutated = Mutation != "NO") %>%
  separate_rows(Mutation, sep = "//") %>%
  mutate(Mutation = ifelse(Mutated, Mutation, NA)) %>%
  separate(Mutation, c("Chromosome", "Position", "Reference allele", "Alternate allele", "Type"), sep = "[:/_ ]", extra = "drop") %>%
  mutate(Type = ifelse(Type == "stoploss", "stop lost", Type)) %>%
  mutate(Type = ifelse(Type == "stopgain", "stop gained", Type)) %>%
  mutate(Type = ifelse(Type == "nonsynonymous", "non-synonymous", Type)) %>%
  mutate(Type = ifelse(Type == "nonframeshift", "non-frameshift", Type))

# mutation sensitivity data
mutationSensitivity <- readTabDelimitedFile("MutationSensitivity.txt", c("Gene", "Drug", "t.statistic", "p.value", "mean.mutation", "n.mutation", "mean.nomutation", "n.nomutation", "fdr"))

mutationSensitivity <- mutationSensitivity %>%
  mutate(fdr = p.adjust(p.value, method = "BH")) %>%
  mutate_at(vars(t.statistic, p.value, mean.mutation, mean.nomutation, fdr), funs(signif(., digits = 3)))

# expression data
expression <- readTabDelimitedFile("ExpressionModels.txt", "Gene")

expression <- expression %>%
  gather(key = Model, value = Expression, -Gene) %>%
  filter(!is.na(Expression)) %>%
  mutate(Expression = signif(Expression, digits = 3))

# expression sensitivity data
expressionSensitivity <- readTabDelimitedFile("ExpressionSensitivity.txt", c("Gene", "Drug", "p.value", "slope", "range.auc", "range.expression", "n", "fdr"))

expressionSensitivity <- expressionSensitivity %>%
  mutate(fdr = p.adjust(p.value, method = "BH")) %>%
  mutate_at(vars(p.value, slope, range.auc, range.expression, fdr), funs(signif(., digits = 3)))

# methylation data
methylation <- readTabDelimitedFile("PromoterMethylationModels.txt", "Gene")

methylation <- methylation %>%
  gather(key = Model, value = Methylation, -Gene) %>%
  filter(!is.na(Methylation)) %>%
  mutate(Methylation = signif(Methylation, digits = 3))

# methylation sensitivity data
methylationSensitivity <- readTabDelimitedFile("MethylationSensitivity.txt", c("Gene", "Drug", "p.value", "slope", "range.auc", "range.methylation", "n", "fdr"))

methylationSensitivity <- methylationSensitivity %>%
  mutate(fdr = p.adjust(p.value, method = "BH")) %>%
  mutate_at(vars(p.value, slope, range.auc, range.methylation, fdr), funs(signif(., digits = 3)))

# gene details
geneDetails <- readTabDelimitedFile("GeneDetails.txt", c("Symbol", "Aliases", "Name"), removeOtherColumns = TRUE)

# retain details of only the genes assessed by the various omics analyses
genes <- bind_rows(
  copyNumberStates %>% select(Gene),
  mutations %>% select(Gene),
  expression %>% select(Gene),
  methylation %>% select(Gene)
) %>%
  distinct

geneDetails <- geneDetails %>%
  rename(Gene = Symbol) %>%
  right_join(genes, by = "Gene") %>%
  arrange(Gene) %>%
  mutate(Name = ifelse(is.na(Name), "", Name)) %>%
  mutate(Aliases = ifelse(is.na(Aliases), "", Aliases)) %>%
  mutate(Details = ifelse(Aliases == "", "", paste("[", Aliases, "]", sep = ""))) %>%
  mutate(Details = ifelse(Details == "", Name, paste(Name, Details)))

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
  mutate_if(is.logical, funs(!is.na(.)))

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

