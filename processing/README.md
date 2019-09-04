Processing steps for creating the BCaPE database
================================================

R scripts for carrying out an analysis of the association between various
biomarkers and drug sensitivity measurements are described in this section.
The input data files are also described and instructions for creating the SQLite
database used by the Shiny app are provided in what follows.

The data used in the BCaPE Shiny app are available on [figshare](https://figshare.com/articles/Bruna_et_al_A_biobank_of_breast_cancer_explants_with_preserved_intra-tumor_heterogeneity_to_screen_anticancer_compounds_Cell_2016/2069274).

### R package dependencies

The R processing scripts require the following R packages which can be installed
using the R `install.packages` function.

- [tidyverse](https://www.tidyverse.org) collection of R packages for data science
- [furrr](https://cran.r-project.org/web/packages/furrr/index.html) for distributing the processing across multiple CPUs
- [tictoc](https://cran.r-project.org/web/packages/tictoc/index.html) for timing the processing steps

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

