library(tidyverse)

# read hgnc_complete_set.txt from HGNC website
hgnc_genes <- read_tsv(
#  "ftp://ftp.ebi.ac.uk/pub/databases/genenames/new/tsv/hgnc_complete_set.txt",
  "hgnc_complete_set.txt",
  col_types = cols_only(
    hgnc_id = col_character(),
    symbol = col_character(),
    name = col_character(),
    status = col_character(),
    prev_symbol = col_character(),
    alias_symbol = col_character()
  )
)

hgnc_genes <- hgnc_genes %>%
  filter(status != "Entry Withdrawn")

# add check for duplicate gene symbols (shouldn't be any)
duplicate_symbols <- hgnc_genes %>%
  count(symbol) %>%
  filter(n > 1) %>%
  .$symbol

if (!is_empty(duplicate_symbols))
  stop("Duplicate gene symbols: ", paste0(duplicate_symbols, sep = ", "))

# create table of gene symbols categorized by their type, e.g. primary, previous or alias, in the HGNC resource
# remove any rows with missing symbols created as part of this extraction/collation
# split entries with multiple gene symbols concatenated with the | symbol into separate rows
gene_symbols <- bind_rows(
  transmute(hgnc_genes, hgnc_id, name, symbol, type = "primary", priority = 1),
  transmute(hgnc_genes, hgnc_id, name, symbol = prev_symbol, type = "previous", priority = 2),
  transmute(hgnc_genes, hgnc_id, name, symbol = alias_symbol, type = "alias", priority = 3)
) %>%
  filter(!is.na(symbol)) %>%
  separate_rows(symbol, sep = "\\|") %>%
  distinct()

# the idea is to create a table of gene symbols with associated gene names and
# aliases where we have an entry for every distinct gene symbol given in the
# HGNC resource regardless of whether these are primary symbols for a gene,
# previously-used symbols or aliases

# note that a gene symbol might refer to more than one gene entry in the HGNC
# resource in which case the gene name for that symbol is taken from the entry
# for which the symbol has the highest status/priority:
# primary > previous > alias

# select just one gene entry for each symbol based on type: primary > previous > alias
# e.g. if a symbol is both the primary symbol for a gene and an alias for another gene, the entry for which it is an alias is discarded
gene_details <- gene_symbols %>%
  group_by(symbol) %>%
  arrange(priority) %>%
  summarize_all(first) %>%
  select(symbol, name, hgnc_id)

# obtain all aliases for each selected gene by joining to the gene symbols data
# frame created above and collapsing all aliases that aren't the preferred
# symbol
gene_details <- gene_details %>%
  left_join(select(gene_symbols, hgnc_id, alias = symbol), by = "hgnc_id") %>%
  select(-hgnc_id)

# collapse rows containing aliases for each gene symbol into a single row where
# the aliases (exclude the entry matching the gene symbol) are concatenated into
# a human-readable comma-separated list
gene_details <- gene_details %>%
  group_by(symbol, name) %>%
  arrange(alias) %>%
  summarize(aliases = paste(setdiff(alias, symbol), collapse = ", ")) %>%
  ungroup() %>%
  mutate(aliases = ifelse(aliases == "", NA, aliases))

# reorder columns and capitalize column headings
gene_details <- gene_details %>%
  select(Symbol = symbol, Aliases = aliases, Name = name)

# read in gene symbols used in each of the data files
genes <- c("ExpressionModels.txt", "PromoterMethylationModels.txt", "SNVsModels.txt", "CNAModels.txt") %>%
  map_dfr(read_tsv, col_types = cols_only(Gene = col_character())) %>%
  distinct() %>%
  rename(Symbol = Gene)

# only write gene details for genes for which there are data
genes %>%
  left_join(gene_details, by = "Symbol") %>%
  arrange(Symbol) %>%
  write_tsv("GeneDetails.txt", na = "")

