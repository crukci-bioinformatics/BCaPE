# R script for performing sensitivity analysis using mutations within genes
# in relation to drug responsiveness in PDTX models

# command line arguments
args <- commandArgs(trailing=TRUE)

if (length(args) != 3)
  stop("Usage: mutation_sensitivity.R drug_sensitivity_file mutation_file output_file")

drugSensitivityFilename <- args[1]
mutationsFilename <- args[2]
outputFilename <- args[3]


source("functions.R")


# drug sensitivity data
drugSensitivity <- readTabDelimitedFile(drugSensitivityFilename, c("Model", "Drug", "AUC"), removeOtherColumns = TRUE)


# mutation data
mutations <- readTabDelimitedFile(mutationsFilename, "Symbol")

mutations <- mutations %>%
  gather(key = Model, value = Mutation, -Symbol) %>%
	transmute(Gene = Symbol, Model, Mutated = Mutation != "NO")


# mutation sensitivity analysis

mutationSensitivity <- data_frame(
  Gene = character(),
  Drug = character(),
  t.statistic = double(),
  p.value = double(),
  mean.mutation = double(),
  n.mutation = integer(),
  mean.nomutation = double(),
  n.nomutation = integer()
)

drugs <- drugSensitivity %>%
  select(Drug) %>%
	arrange(Drug) %>%
  unique %>%
  unlist(use.names = FALSE)

genes <- mutations %>%
  group_by(Gene) %>%
  count(Mutated) %>%
  summarize(n = min(n)) %>%
  filter(n > 1) %>%
	select(Gene) %>%
	arrange(Gene) %>%
  unique %>%
  unlist(use.names = FALSE)

geneCount <- 0
for (gene in genes)
{
  geneCount <- geneCount + 1
  cat(geneCount, "/", length(genes), "\n", sep = "")

  geneMutations <- mutations %>% filter(Gene == gene)

  counts <- geneMutations %>% count(Mutated) %>% arrange(n)
  if (nrow(counts) != 2 | counts %>% slice(1) %>% select(n) %>% as.integer < 2) next

  geneMutationSensitivity <- inner_join(geneMutations, drugSensitivity, by = "Model")
  if (nrow(geneMutationSensitivity) == 0) next

  for (drug in drugs)
  {
    geneDrugMutationSensitivity <- geneMutationSensitivity %>% filter(Drug == drug)
    if (nrow(geneDrugMutationSensitivity) == 0) next

    counts <- geneDrugMutationSensitivity %>% count(Mutated) %>% arrange(n)
    if (nrow(counts) != 2 || counts %>% slice(1) %>% select(n) %>% as.integer < 2) next

    result <- try(t.test(AUC ~ Mutated, data = geneDrugMutationSensitivity))

    if (class(result) != "try-error")
    {
      mutationSensitivity <- mutationSensitivity %>% bind_rows(data_frame(
        Gene = gene,
        Drug = drug,
        t.statistic = result$statistic,
        p.value = result$p.value,
        mean.mutation = result$estimate["mean in group TRUE"],
        n.mutation = counts %>% filter(Mutated) %>% select(n) %>% as.integer,
        mean.nomutation = result$estimate["mean in group FALSE"],
        n.nomutation = counts %>% filter(!Mutated) %>% select(n) %>% as.integer
      ))
    }
  }
}

mutationSensitivity$fdr <- p.adjust(mutationSensitivity$p.value, method = "BH")


# write results
write_tsv(mutationSensitivity, outputFilename)


