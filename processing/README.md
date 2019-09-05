Processing steps for creating the BCaPE database
================================================

R scripts for carrying out an analysis of the association between various
biomarkers and drug sensitivity measurements are described in this section.
The input data files are also described and instructions for creating the SQLite
database used by the Shiny app are provided in what follows.

The data used in the BCaPE Shiny app are available on [figshare](https://figshare.com/articles/Bruna_et_al_A_biobank_of_breast_cancer_explants_with_preserved_intra-tumor_heterogeneity_to_screen_anticancer_compounds_Cell_2016/2069274).

### R package dependencies

The R processing scripts require the following packages which can be installed
using the `install.packages` function.

- [tidyverse](https://www.tidyverse.org) collection of R packages for data science
- [furrr](https://cran.r-project.org/web/packages/furrr/index.html) for distributing the processing across multiple CPUs
- [tictoc](https://cran.r-project.org/web/packages/tictoc/index.html) for timing the processing steps
- [RSQLite](https://cran.r-project.org/web/packages/RSQLite/index.html) SQLite interface for R
- [dbplyr](https://cran.r-project.org/web/packages/dbplyr/index.html) dplyr backend for databases

In addition, the SQLite database engine needs to be installed; this can be
checked as follows.

```
sqlite3 --version
```

### Annotation files

Annotation files for the patient-derived tumour xenografts (PDTX), hereafter
referred to as models, and the drugs these were treated with need to be
provided. A tabular file containing details of genes assayed is also needed but
can be generated from the human gene dataset available from the HUGO Gene
Nomenclature Committee ([HGNC](https://www.genenames.org/)).

Note that all annotation files are tab-delimited.

#### ModelClassifications.txt

The model classifications file contains details of the PDTX models including the
ER, HER2, PR and BRCA1 status (positive, negative), the type of tumour from
which the PDTX was derived (primary, metastatic), and various classifications
including the [METABRIC](https://www.nature.com/articles/nature10983) integrated
cluster, 3-gene and PAM50 classifications described in the
[BCAPE publication](http://www.cell.com/cell/abstract/S0092-8674(16)31138-2).

An excerpt from this tabular file is given below:


Model|ER|HER2|PR|BRCA1|Type|iC10|3-Gene|PAM50
-----|--|----|--|-----|----|----|------|-----
AB405|Neg|Neg|NA|NA|Metastatic|NA|NA|NA
AB521M|Neg|Pos|Neg|NA|Metastatic|10|ER-/HER2-|Basal
AB551|Pos|Pos|Neg|NA|Metastatic|5|HER2+|Her2
AB555|Pos|Neg|Neg|NA|Metastatic|1|ER+/HER2- High Prolif|NC
AB559|Pos|Neg|Neg|NA|Primary|10|ER-/HER2-|Basal
AB569|Pos|Neg|NA|NA|Primary|4ER+|NC|NA

#### DrugAnnotations.txt

The drug annotation file contains details of each of the drugs for which
sensitivity data is available, specifically the pathways and genes targeted by
each drug.

An excerpt from this tab-delimited file is given below:

Drug|Pathway|Target
----|-------|------
(5Z)-7-Oxozeaenol|other|TAK1 (MAP3K7)
17-AAG|other|HSP90
5-Fluorouracil|DNA replication|antimetabolite
681640|Genome integrity (CHEK)|WEE1, CHEK1
ABT-263|apoptosis regulation|BCL2, BCL-XL, BCL-W
AG-014699|Genome integrity (PARP)|PARP1, PARP2

#### GeneDetails.txt

The gene details file uses gene symbols as identifiers for each gene as well as
aliases and descriptive gene names. An excerpt is given below:

Symbol|Aliases|Name
------|-------|----
A1BG||alpha-1-B glycoprotein
A1BG-AS1|A1BG-AS, A1BGAS, FLJ23569, NCRNA00181|A1BG antisense RNA 1
A1CF|ACF, ACF64, ACF65, APOBEC1CF, ASP|APOBEC1 complementation factor
A2LD1|GGACT|gamma-glutamylamine cyclotransferase
A2M|CPAMD5, FWP007, S863-7|alpha-2-macroglobulin
A2M-AS1||A2M antisense RNA 1

This file can be generated from the
[complete HGNC dataset](ftp://ftp.ebi.ac.uk/pub/databases/genenames/new/tsv/hgnc_complete_set.txt)
with the `create_gene_details.R` R script.

```
wget ftp://ftp.ebi.ac.uk/pub/databases/genenames/new/tsv/hgnc_complete_set.txt

Rscript create_gene_details.R
```

### Assay data files

The BCaPE resource consists of drug sensitivity measurements and data from
high-throughput sequencing to detect single nucleotide variants and copy number
states and to measure levels of expression and methylation for all genes.

Details of how these data were generated are given in the
[BCAPE publication](http://www.cell.com/cell/abstract/S0092-8674(16)31138-2).
An excerpt from each file is given below.

#### DrugResponsesAUCModels.txt

Model|Drug|AUC|iC50|D1_CONC|D5_CONC|perc.iC50
-----|----|---|----|-------|-------|---------
HCI001|(5Z)-7-Oxozeaenol|0.218|5.483|7.701|2.338|71.507
HCI001|17-AAG|0.302|18349.741|1|0.004|183.038
HCI001|5-Fluorouracil|0.494|1.350|20|0.078|51.391
HCI001|681640|0.103|152.126|2|0.008|150.681
HCI001|ABT-263|0.497|0.125|2|0.008|49.591
HCI001|AG-014699|0.235|18.227|5|0.020|123.326

#### SNVsModels.txt

Mutations detected within genes in each PDTX model should also be provided in a
tabular format as shown below. The first column contains contains genes and all
subsequent columns contain details of mutations for each PDTX model.

Gene|AB521M|AB551|AB555|AB559|AB580
----|------|-----|-----|-----|-----
AGBL5|chr2:27278923_G/A_nonsynonymous SNV|NO|NO|NO|NO
ALAS1|chr3:52248142_A/ATGGTATTTTTGGTCCTTGAGGAACCACC_frameshift substitution|NO|NO|NO|NO
APOD|chr3:195306268_G/A_nonsynonymous SNV|NO|NO|NO|NO
B4GALT2|chr1:44447006_TAGC/T_nonframeshift substitution|NO|NO|chr1:44447006_TAGC/T_nonframeshift substitution|NO
BNIP3L|chr8:26240683_CACA/C_nonframeshift substitution|NO|NO|NO|NO
CACNA1F|chrX:49066446_G/A_nonsynonymous SNV|NO|NO|NO|NO

The format expected for mutations is as follows.

Chromosome:Position_ReferenceAllele/AlternateAllele_Type

where everything after the type of mutation is ignored. The ':', '/' and '_'
characters are used to separate the mutation string into its chromosome,
position, reference and alternate allele constituents.

Multiple mutations identified within a gene can be listed and need to be
separated by '//', e.g.

shr13:108518706_TGCTGCTGCC/T_nonframeshift substitution//chr13:108518727_T/C_nonsynonymous SNV

If a gene is not mutated within a given model a value of 'NO' should be given.

#### CNAModels.txt

Copy number states for each gene within each model should be provided in a
tabular format as shown below. The first column contains contains genes and all
subsequent columns contain copy number states for each PDTX model.

Gene|AB521M|AB551|AB555|AB559|AB580
----|------|-----|-----|-----|-----
DDX11L1|NA|NA|NA|NA|NA
WASH7P|NA|NA|NA|NA|NA
MIR6859-1|LOSS|LOSS|UNKNOWN|UNKNOWN|GAIN
MIR6859-2|LOSS|LOSS|UNKNOWN|UNKNOWN|GAIN
FAM138A|NA|NA|NA|NA|NA
FAM138F|NA|NA|NA|NA|NA

Valid values for copy number states are GAIN, LOSS, NEUTRAL (or NEUT), UNKNOWN
and NA.

#### ExpressionModels.txt

Normalized log2-transformed expression values should be provided in a tabular
format as shown below. The first column contains contains genes and all
subsequent columns contain expression values for each PDTX model.

Gene|AB521M|AB551|AB555|AB559|AB580
----|------|-----|-----|-----|-----
A1BG|5.862|5.831|5.989|5.953|5.928
A1CF|5.818|5.965|5.909|6.009|6.063
A2LD1|7.686|6.931|7.037|7.170|6.208
A2M|5.572|5.752|5.589|10.123|5.522
A3GALT2|5.873|5.857|5.965|5.837|5.827
A4GALT|5.833|5.685|6.563|8.222|5.593

#### PromoterMethylationModels.txt

Methylation values should also be provided in a similar tabular format with the
first column specifying the gene and subsequent columns for each PDTX model.
Values are % promoter methylation within the gene for a given model.

Gene|AB521M|AB555|AB559|AB564|AB572
----|------|-----|-----|-----|-----
A1BG|70.125|70.104|52.500|82.314|56.986
A1BG-AS1|76.426|80.026|69.423|84.730|77.039
A1CF|NA|NA|NA|NA|NA
A2M|NA|NA|NA|NA|NA
A2M-AS1|12.278|18.148|1.136|28.292|3.077
A2ML1|27.643|50.140|50.357|59.101|53.488

### Sensitivity analysis

This section describes how to run the R scripts that carry out an analysis of
the effect of a given alteration (copy number gain or loss, mutation) or the
level of gene expression or promoter methylation on the sensitivity of the
drugs tested on the PDTX models.

In the case of mutations, t-tests were carried out for each drug and gene
combination, comparing the sensitivity of the drug within PDTX models with a
mutation in that gene with those models where the gene is not mutated.

Similarly, t-tests were carried out to test the effect of copy number gains
(gain versus no gain) and copy number losses (loss versus no loss) for each gene
on the sensitivity of models to a given drug.

A linear model was fitted to the expression levels of a gene and the sensitivity
of the models to a given drug, for every drug and gene combination. The effect
size is the slope of the line fitted and the p-value tests the null hypothesis
that the slope is equal to zero, i.e. no effect.

The same approach of fitting a linear model is applied to the methylation data.

In each of the above analyses, p-values are adjusted for multiple comparisons
using the Benjamini and Hochberg method (`p.adjust` function with the "BH"
method argument).

The R scripts for carrying out the above sensitivity analyses are run as
follows, creating output files ending with the suffix `Sensitivity.txt`.

```
Rscript mutation_sensitivity.R DrugResponsesAUCModels.txt SNVsModels.txt MutationSensitivity.txt

Rscript copy_number_gain_sensitivity.R DrugResponsesAUCModels.txt CNAModels.txt CopyNumberGainSensitivity.txt

Rscript copy_number_loss_sensitivity.R DrugResponsesAUCModels.txt CNAModels.txt CopyNumberLossSensitivity.txt

Rscript expression_sensitivity.R DrugResponsesAUCModels.txt ExpressionModels.txt ExpressionSensitivity.txt

Rscript methylation_sensitivity.R DrugResponsesAUCModels.txt PromoterMethylationModels.txt MethylationSensitivity.txt
```

A large number of t-tests or linear regressions are carried out by these
scripts and the computational work is distributed across multiple CPUs or cores
using the R `furrr` package which in turn uses the `futures` package in its
parallelized versions of the `dplyr` map functions. By default these scripts
will use the maximum number of cores available. This works well on a HPC cluster
where the number of cores allocated to a task is specified when that task is
submitted via the job scheduler; the script then will pick up the number of
cores so allocated as the number available to it and distribute the
computational work accordingly. Otherwise, when running on a multi-core machine
the number of cores can be limited so not to overload the computer by setting
the `MC_CORES` environment variable, e.g.

```
MC_CORES=2 Rscript expression_sensitivity.R DrugResponsesAUCModels.txt ExpressionModels.txt ExpressionSensitivity.txt
```

The outputs from these sensitivity analyses are tab-delimited files with a row
for each Gene and Drug pairing with t-statistics or slope coefficients from
fitted linear models, p-values and false discovery rate (FDR) adjusted p-values.
An excerpt from the output of the mutation sensitivity analysis is given below.

#### MutationSensitivity.txt

Gene|Drug|t.statistic|p.value|mean.mutation|n.mutation|mean.nomutation|n.nomutation|fdr
----|----|-----------|-------|-------------|----------|---------------|------------|---
PIK3CA|JNK Inhibitor VIII|-0.306|0.774|0.0605|4|0.0536|15|0.975
PIK3CA|JQ1|0.648|0.539|0.161|4|0.192|15|0.946
PIK3CA|KU-55933|6.5|9.43e-6|0.102|4|0.219|15|0.00286
PIK3CA|Lenalidomide|0.16|0.877|0.038|4|0.0397|15|0.983
PIK3CA|LY317615|2.64|0.0217|0.0267|2|0.112|12|0.245
PIK3CA|MK-2206|-2.3|0.0359|0.432|4|0.305|15|0.314

#### Multiple testing correction for copy number states

Tests for the effect of copy number state of genes on the sensitivity to a drug
in a given PDTX model are applied separately for copy number gains and losses.
Similarly the correction for multiple correction is applied for gains and losses
separately but this correction can be re-applied taking into account both gains
and losses together using the `recalculate_copy_number_sensitivity_fdr.R`
script.

```
Rscript recalculate_copy_number_sensitivity_fdr.R
```

This creates files named `CopyNumberGainSensitivity.adjusted.txt` and
`CopyNumberLossSensitivity.adjusted.txt` with updated FDR values.

### Creating the SQLite database

The R script `create_sqlite_database.R` reads in all the aforementioned
annotation and data files, and the outputs of the sensitivity analyses described
above, and creates a SQLite database file called `bcape.sqlite` that is used by
the Shiny app.

```
Rscript create_sqlite_database.R
```

This database is designed for use by the BCaPE Shiny app but can be accessed and
queried using using SQLite at the command line as shown below.

```
$ sqlite3 bcape.sqlite

SQLite version 3.8.10.2 2015-05-20 18:17:19
Enter ".help" for usage hints.

sqlite> .tables

copyNumberGainSensitivity  geneDetails
copyNumberLossSensitivity  methylation
copyNumberStates           methylationSensitivity
drugAnnotations            modelClassifications
drugSensitivity            mutationSensitivity
expression                 mutations
expressionSensitivity

sqlite> .schema copyNumberStates

CREATE TABLE `copyNumberStates` (
  `Gene` TEXT,
  `Model` TEXT,
  `State` TEXT
);
CREATE INDEX `copyNumberStates_Gene` ON `copyNumberStates` (`Gene`);
CREATE INDEX `copyNumberStates_Model` ON `copyNumberStates` (`Model`);

sqlite> select * from copyNumberStates limit 10;

MIR6859-1|AB521M|loss
MIR6859-2|AB521M|loss
LINC01002|AB521M|unknown
LOC100132287|AB521M|unknown
LOC100133331|AB521M|unknown
OR4F16|AB521M|unknown
OR4F29|AB521M|unknown
OR4F3|AB521M|unknown
RNU6-2|AB521M|unknown
KLHL17|AB521M|loss

sqlite> .quit
```
