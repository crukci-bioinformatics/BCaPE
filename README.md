R processing scripts and Shiny app for Breast Cancer PDTX Encyclopaedia
=======================================================================

Source code repository for the publication [Bruna et al., Cell 167, 260 - 274 (2016) ](http://www.cell.com/cell/abstract/S0092-8674(16)31138-2) and accompanying
[BCaPE website](http://caldaslab.cruk.cam.ac.uk/bcape).

R scripts for carrying out an analysis of the association between various
biomarkers and drug sensitivity within a biobank of breast cancer patient-derived
tumour xenografts (PDTX) are contained with the [processing](processing) folder.
Instructions for obtaining the mutation, copy number, expression and methylation
data, running the sensitivity analysis and creating the database that is used by
the BCaPE web application can be found [here](processing/README.md).

R code for the Shiny application used to create the [BCaPE website](http://caldaslab.cruk.cam.ac.uk/bcape/) is available in the [shiny](shiny)
folder. Instructions for deploying the Shiny application can be found [here](shiny/README.md).
