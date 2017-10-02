fluidPage(

  tags$head(includeScript("analytics/analytics.js")),

  titlePanel(
    title = tags$div(
      "Breast Cancer PDTX Encyclopaedia",
      tags$a(
        href="http://www.cambridgecancer.org.uk", target = "_blank",
        tags$div(style = "float:right",
          tags$img(style = "width:250px", src="CRUK_CAMBRIDGE_I_Pos_RGB_300.jpg"),
          tags$img(style = "width:150px; padding:1px; margin-left:5px; margin-bottom:15px; background-color:#221F7F", src="main-logo-small.png")
        )
      )
    ),
    windowTitle = "BCaPE"    
  ),

  tabsetPanel(
    id = "mainTabsetPanel",

    tabPanel("Home",
      tags$img(style = "width:600px; padding-left:50px; float:right", src="overview.png"),
      tags$h4("A Biobank of Breast Cancer Explants with Preserved Intra-tumor Heterogeneity to Screen Anticancer Compounds."),
      tags$a(
        href = "http://www.cell.com/cell/abstract/S0092-8674(16)31138-2",
        target = "_blank",
        "Bruna", tags$em("et al.,"), "Cell 167, 260 - 274 (2016)"),
      tags$p(),
      "The Breast Cancer PDTX Encyclopaedia (BCaPE) provides tools for visualizing, querying and downloading data from a large collection of highly molecularly annotated breast cancer patient-derived tumour xenografts (PDTX).",
      tags$p(),
      "The biobank was developed by",
      tags$a(href = "http://www.cruk.cam.ac.uk/research-groups/caldas-group", target = "_blank", "Carlos Caldas' laboratory"),
      "at the Cancer Research UK Cambridge Institute",
      "and represents a powerful resource for pre-clinical breast cancer pharmacogenomic studies, including identification of biomarkers of response or resistance.",
      tags$div(style="line-height:25%;", br()),
      tags$h5("BCaPE statistics"),
      tableOutput("statisticsTable"),
      tags$h5("Funding"),
      "This work is funded by",
      tags$a(href = "http://www.cancerresearchuk.org/", target = "_blank", "Cancer Research UK"),
      tags$div(style = "clear:both", tags$div(style="line-height:50%;", br()))
    ),

    tabPanel("Models",
      sidebarLayout(

        sidebarPanel(
          checkboxInput("modelSelectionCheckbox", "Restrict to models with drug sensitivity data", value = FALSE),
          conditionalPanel(
            condition = "input.modelSelectionCheckbox",
            DT::dataTableOutput("restrictedModelSelectionTable")
          ),
          conditionalPanel(
            condition = "!input.modelSelectionCheckbox",
            DT::dataTableOutput("modelSelectionTable")
          )
        ),

        mainPanel(
          width = 7,
          tags$div(style="line-height:50%;", br()),
          tags$div(style="font-size:120%", htmlOutput("modelLabel")),
          tags$div(style="line-height:50%;", br()),
          tabsetPanel(
            id = "modelTabsetPanel",
            tabPanel(
              "Drugs",
              highchartOutput("modelSensitivityDotPlot", height = "500px"),
              tabsetPanel(
                tabPanel(
                  "Details",
                  tags$div(style="line-height:50%;", br()),
                  helpText("Select a model in the search panel to view the AUC dot plot. Drag within plot to zoom. Click on a point to navigate to the drug view.")
                ),
                tabPanel(
                  "Data",
                  tags$div(style="line-height:100%;", br()),
                  DT::dataTableOutput("modelSensitivityTable")
                )
              )
            )
          )
        )
      )
    ),

    tabPanel("Drugs",
      sidebarLayout(

        sidebarPanel(
          DT::dataTableOutput("drugSelectionTable")
        ),

        mainPanel(
          width = 7,
          tags$div(style="line-height:50%;", br()),
          tags$div(style="font-size:120%", htmlOutput("drugLabel")),
          tags$div(style="line-height:50%;", br()),
          tabsetPanel(
            id = "drugTabsetPanel",
            tabPanel(
              "Models",
              highchartOutput("drugSensitivityDotPlot", height = "500px"),
              tabsetPanel(
                tabPanel(
                  "Details",
                  tags$div(style="line-height:50%;", br()),
                  helpText("Select a drug in the search panel to view the AUC dot plot. Drag within plot to zoom. Click on a point to navigate to the model view.")
                ),
                tabPanel(
                  "Data",
                  tags$div(style="line-height:100%;", br()),
                  DT::dataTableOutput("drugSensitivityTable")
                )
              )
            ),
            tabPanel(
              "Copy Number Gains",
              highchartOutput("drugCopyNumberGainSensitivityVolcanoPlot", height = "500px"),
              tabsetPanel(
                tabPanel(
                  "Details",
                  tags$div(style="line-height:50%;", br()),
                  helpText("The plot shows, for each gene, in the y axis the adjusted p-value in -log10 scale of a t-test comparing the AUC in models with gains vs models with no gains in that gene. Values larger than 1 are considered statistically significant."),
                  helpText("The x axis shows the negative value of the t-statistic that measures the difference in AUC means for those two groups of models. Positive values suggest sensitivity to the drug in the models with the CNA and negative values resistance."),
                  helpText("Filtered genes are those with a mean AUC that is considered sensitive for the models with the alteration and a mean AUC resistant for the models without the alteration (or the other way around; threshold for sensitivity is 0.2)."),
                  helpText("An upper limit of 1000 genes are shown; these include the most significant 500."),
                  helpText("Select a drug in the search panel to view sensitivity results. Drag within plot to zoom. Click on a point to navigate to the gene view.")
                ),
                tabPanel(
                  "Data",
                  tags$div(style="line-height:100%;", br()),
                  DT::dataTableOutput("drugCopyNumberGainSensitivityTable")
                )
              )
            ),
            tabPanel(
              "Copy Number Losses",
              highchartOutput("drugCopyNumberLossSensitivityVolcanoPlot", height = "500px"),
              tabsetPanel(
                tabPanel(
                  "Details",
                  tags$div(style="line-height:50%;", br()),
                  helpText("The plot shows, for each gene, in the y axis the adjusted p-value in -log10 scale of a t-test comparing the AUC in models with losses vs models with no losses in that gene. Values larger than 1 are considered statistically significant."),
                  helpText("The x axis shows the negative value of the t-statistic that measures the difference in AUC means for those two groups of models. Positive values suggest sensitivity to the drug in the models with the CNA and negative values resistance."),
                  helpText("Filtered genes are those with a mean AUC that is considered sensitive for the models with the alteration and a mean AUC resistant for the models without the alteration (or the other way around; threshold for sensitivity is 0.2)."),
                  helpText("An upper limit of 1000 genes are shown; these include the most significant 500."),
                  helpText("Select a drug in the search panel to view sensitivity results. Drag within plot to zoom. Click on a point to navigate to the gene view.")
                ),
                tabPanel(
                  "Data",
                  tags$div(style="line-height:100%;", br()),
                  DT::dataTableOutput("drugCopyNumberLossSensitivityTable")
                )
              )
            ),
            tabPanel(
              "Mutations",
              highchartOutput("drugMutationSensitivityVolcanoPlot", height = "500px"),
              tabsetPanel(
                tabPanel(
                  "Details",
                  tags$div(style="line-height:50%;", br()),
                  helpText("The plot shows, for each gene, in the y axis the adjusted p-value in -log10 scale of a t-test comparing the AUC in models with a mutation vs models without it in that gene. Values larger than 1 are considered statistically significant."),
                  helpText("The x axis shows the negative value of the t-statistic that measures the difference in AUC means for those two groups of models. Positive values suggest sensitivity to the drug in the models with the mutation and negative values resistance."),
                  helpText("Filtered genes are those with a mean AUC that is considered sensitive for the models with the alteration and a mean AUC resistant for the models without the alteration (or the other way around; threshold for sensitivity is 0.2)."),
                  helpText("An upper limit of 1000 genes are shown; these include the most significant 500."),
                  helpText("Select a drug in the search panel to view sensitivity results. Drag within plot to zoom. Click on a point to navigate to the gene view.")
                ),
                tabPanel(
                  "Data",
                  tags$div(style="line-height:100%;", br()),
                  DT::dataTableOutput("drugMutationSensitivityTable")
                )
              )
            ),
            tabPanel(
              "Expression",
              highchartOutput("drugExpressionSensitivityVolcanoPlot", height = "500px"),
              tabsetPanel(
                tabPanel(
                  "Details",
                  tags$div(style="line-height:50%;", br()),
                  helpText("The plot shows, for each gene, in the y axis the adjusted p-value in -log10 scale of a linear model comparing the AUC vs the log-intensity expression in that gene for all models. Values larger than 1 are considered statistically significant."),
                  helpText("The x axis shows the slope measuring the change in AUC per expression unit. Positive values suggest sensitivity to the drug in the models with over expression in the gene and negative values resistance."),
                  helpText("An upper limit of 1000 genes are shown; these include the most significant 500."),
                  helpText("Select a drug in the search panel to view sensitivity results. Drag within plot to zoom. Click on a point to navigate to the gene view.")
                ),
                tabPanel(
                  "Data",
                  tags$div(style="line-height:100%;", br()),
                  DT::dataTableOutput("drugExpressionSensitivityTable")
                )
              )
            ),
            tabPanel(
              "Methylation",
              highchartOutput("drugMethylationSensitivityVolcanoPlot", height = "500px"),
              tabsetPanel(
                tabPanel(
                  "Details",
                  tags$div(style="line-height:50%;", br()),
                  helpText("The plot shows, for each gene, in the y axis the adjusted p-value in -log10 scale of a linear model comparing the AUC vs the promoter methylation in that gene for all models. Values larger than 1 are considered statistically significant."),
                  helpText("The x axis shows the slope measuring the change in AUC per methylation percentage unit. Positive values suggest sensitivity to the drug in the models with greater methylation in the gene promoter and negative values resistance."),
                  helpText("Filtered genes are those statistically significant (with an unadjusted p-value smaller than 0.05 and a range of methylation in all models larger than 25%."),
                  helpText("An upper limit of 1000 genes are shown; these include the most significant 500."),
                  helpText("Select a drug in the search panel to view sensitivity results. Drag within plot to zoom. Click on a point to navigate to the gene view.")
                ),
                tabPanel(
                  "Data",
                  tags$div(style="line-height:100%;", br()),
                  DT::dataTableOutput("drugMethylationSensitivityTable")
                )
              )
            )
          )
        )
      )
    ),

    tabPanel("Genes",
      sidebarLayout(

        sidebarPanel(
          conditionalPanel(
            condition = "input.geneTabsetPanel == 'Copy Number' && input.geneCopyNumberTabsetPanel == 'Copy Number States'",
            DT::dataTableOutput("geneCopyNumberStateSelectionTable")
          ),
          conditionalPanel(
            condition = "input.geneTabsetPanel == 'Copy Number' && input.geneCopyNumberTabsetPanel == 'Sensitivity - Gains'",
            DT::dataTableOutput("geneCopyNumberGainSensitivitySelectionTable")
          ),
          conditionalPanel(
            condition = "input.geneTabsetPanel == 'Copy Number' && input.geneCopyNumberTabsetPanel == 'Sensitivity - Losses'",
            DT::dataTableOutput("geneCopyNumberLossSensitivitySelectionTable")
          ),
          conditionalPanel(
            condition = "input.geneTabsetPanel == 'Mutations' && input.geneMutationTabsetPanel == 'Mutation Details'",
            DT::dataTableOutput("geneMutationSelectionTable")
          ),
          conditionalPanel(
            condition = "input.geneTabsetPanel == 'Mutations' && input.geneMutationTabsetPanel == 'Sensitivity analysis'",
            DT::dataTableOutput("geneMutationSensitivitySelectionTable")
          ),
          conditionalPanel(
            condition = "input.geneTabsetPanel == 'Expression' && input.geneExpressionTabsetPanel == 'Expression Data'",
            DT::dataTableOutput("geneExpressionSelectionTable")
          ),
          conditionalPanel(
            condition = "input.geneTabsetPanel == 'Expression' && input.geneExpressionTabsetPanel == 'Sensitivity analysis'",
            DT::dataTableOutput("geneExpressionSensitivitySelectionTable")
          ),
          conditionalPanel(
            condition = "input.geneTabsetPanel == 'Methylation' && input.geneMethylationTabsetPanel == 'Methylation Data'",
            DT::dataTableOutput("geneMethylationSelectionTable")
          ),
          conditionalPanel(
            condition = "input.geneTabsetPanel == 'Methylation' && input.geneMethylationTabsetPanel == 'Sensitivity analysis'",
            DT::dataTableOutput("geneMethylationSensitivitySelectionTable")
          )
        ),

        mainPanel(
          width = 7,
          tags$div(style="line-height:50%;", br()),
          tags$div(style="font-size:120%", htmlOutput("geneLabel")),
          tags$div(style="line-height:50%;", br()),
          tabsetPanel(
            id = "geneTabsetPanel",
            tabPanel(
              "Copy Number",
              tags$div(style="line-height:25%;", br()),
              tabsetPanel(
                id = "geneCopyNumberTabsetPanel",
                tabPanel(
                  "Copy Number States",
                  highchartOutput("geneCopyNumberStateBarPlot", height = "500px"),
                  tabsetPanel(
                    tabPanel(
                      "Details",
                      tags$div(style="line-height:50%;", br()),
                      helpText("Select a gene in the search panel to copy number states.")
                    ),
                    tabPanel(
                      "Data",
                      tags$div(style="line-height:100%;", br()),
                      DT::dataTableOutput("geneCopyNumberStateTable")
                    )
                  )
                ),
                tabPanel(
                  "Sensitivity - Gains",
                  highchartOutput("geneCopyNumberGainSensitivityVolcanoPlot", height = "500px"),
                  tabsetPanel(
                    tabPanel(
                      "Details",
                      tags$div(style="line-height:50%;", br()),
                      helpText("The plot shows, for a given gene, in the y axis the adjusted p-value in -log10 scale of a t-test comparing the AUC in models with gains vs models with no gains in that gene. Values larger than 1 are considered statistically significant."),
                      helpText("The x axis shows the negative value of the t-statistic that measures the difference in AUC means for those two groups of models. Positive values suggest sensitivity to the drug in the models with the CNA and negative values resistance."),
                      helpText("Filtered genes are those with a mean AUC that is considered sensitive for the models with the alteration and a mean AUC resistant for the models without the alteration (or the other way around; threshold for sensitivity is 0.2)."),
                      helpText("Select a gene in the search panel to view sensitivity results. Drag within plot to zoom. Click on a point to navigate to the drug view.")
                    ),
                    tabPanel(
                      "Data",
                      tags$div(style="line-height:100%;", br()),
                      DT::dataTableOutput("geneCopyNumberGainSensitivityTable")
                    )
                  )
                ),
                tabPanel(
                  "Sensitivity - Losses",
                  highchartOutput("geneCopyNumberLossSensitivityVolcanoPlot", height = "500px"),
                  tabsetPanel(
                    tabPanel(
                      "Details",
                      tags$div(style="line-height:50%;", br()),
                      helpText("The plot shows, for a given gene, in the y axis the adjusted p-value in -log10 scale of a t-test comparing the AUC in models with losses vs models with no losses in that gene. Values larger than 1 are considered statistically significant."),
                      helpText("The x axis shows the negative value of the t-statistic that measures the difference in AUC means for those two groups of models. Positive values suggest sensitivity to the drug in the models with the CNA and negative values resistance."),
                      helpText("Filtered genes are those with a mean AUC that is considered sensitive for the models with the alteration and a mean AUC resistant for the models without the alteration (or the other way around; threshold for sensitivity is 0.2)."),
                      helpText("Select a gene in the search panel to view sensitivity results. Drag within plot to zoom. Click on a point to navigate to the drug view.")
                    ),
                    tabPanel(
                      "Data",
                      tags$div(style="line-height:100%;", br()),
                      DT::dataTableOutput("geneCopyNumberLossSensitivityTable")
                    )
                  )
                )
              )
            ),
            tabPanel(
              "Mutations",
              tags$div(style="line-height:25%;", br()),
              tabsetPanel(
                id = "geneMutationTabsetPanel",
                tabPanel(
                  "Sensitivity analysis",
                  highchartOutput("geneMutationSensitivityVolcanoPlot", height = "500px"),
                  tabsetPanel(
                    tabPanel(
                      "Details",
                      tags$div(style="line-height:50%;", br()),
                      helpText("The plot shows, for each gene, in the y axis the adjusted p-value in -log10 scale of a t-test comparing the AUC in models with a mutation  vs models without it in that gene. Values larger than 1 are considered statistically significant."),
                      helpText("The x axis shows the negative value of the t-statistic that measures the difference in AUC means for those two groups of models. Positive values suggest sensitivity to the drug in the models with the mutation and negative values resistance."),
                      helpText("Filtered genes are those with a mean AUC that is considered sensitive for the models with the alteration and a mean AUC resistant for the models without the alteration (or the other way around; threshold for sensitivity is 0.2)."),
                      helpText("Select a gene in the search panel to view sensitivity results. Drag within plot to zoom. Click on a point to navigate to the drug view.")
                    ),
                    tabPanel(
                      "Data",
                      tags$div(style="line-height:100%;", br()),
                      DT::dataTableOutput("geneMutationSensitivityTable")
                    )
                  )
                ),
                tabPanel(
                  "Mutation Details",
                  tags$div(style="line-height:75%;", br()),
                  DT::dataTableOutput("geneMutationsTable")
                )
              )
            ),
            tabPanel(
              "Expression",
              tags$div(style="line-height:25%;", br()),
              tabsetPanel(
                id = "geneExpressionTabsetPanel",
                tabPanel(
                  "Sensitivity analysis",
                  highchartOutput("geneExpressionSensitivityVolcanoPlot", height = "500px"),
                  tabsetPanel(
                    tabPanel(
                      "Details",
                      tags$div(style="line-height:50%;", br()),
                      helpText("The plot shows, for each gene, in the y axis the adjusted p-value in -log10 scale of a linear model comparing the AUC vs the log-intensity expression in that gene for all models. Values larger than 1 are considered statistically significant."),
                      helpText("The x axis shows the slope measuring the change in AUC per expression unit. Positive values suggest sensitivity to the drug in the models with over expression in the gene and negative values resistance."),
                      helpText("Select a gene in the search panel to view sensitivity results. Drag within plot to zoom. Click on a point to navigate to the drug view.")
                    ),
                    tabPanel(
                      "Data",
                      tags$div(style="line-height:100%;", br()),
                      DT::dataTableOutput("geneExpressionSensitivityTable")
                    )
                  )
                ),
                tabPanel(
                  "Expression Data",
                  tags$div(style="line-height:50%;", br()),
                  helpText("Expression values are z-score expression intensities."),
                  tags$div(style="line-height:50%;", br()),
                  DT::dataTableOutput("geneExpressionTable")
                )
              )
            ),
            tabPanel(
              "Methylation",
              tags$div(style="line-height:25%;", br()),
              tabsetPanel(
                id = "geneMethylationTabsetPanel",
                tabPanel(
                  "Sensitivity analysis",
                  highchartOutput("geneMethylationSensitivityVolcanoPlot", height = "500px"),
                  tabsetPanel(
                    tabPanel(
                      "Details",
                      tags$div(style="line-height:50%;", br()),
                      helpText("The plot shows, for each gene, in the y axis the adjusted p-value in -log10 scale of a linear model comparing the AUC vs the promoter methylation in that gene for all models.Values larger than 1 are considered statistically significant."),
                      helpText("The x axis shows the slope measuring the change in AUC per methylation percentage unit. Positive values suggest sensitivity to the drug in the models with greater methylation in the gene promoter and negative values resistance."),
                      helpText("Filtered genes are those statistically significant (with an unadjusted p-value smaller than 0.05 and a range of methylation in all models larger than 25%."),
                      helpText("Select a gene in the search panel to view sensitivity results. Drag within plot to zoom. Click on a point to navigate to the drug view.")
                    ),
                    tabPanel(
                      "Data",
                      tags$div(style="line-height:100%;", br()),
                      DT::dataTableOutput("geneMethylationSensitivityTable")
                    )
                  )
                ),
                tabPanel(
                  "Methylation Data",
                  tags$div(style="line-height:50%;", br()),
                  helpText("Methylation values are % promoter methylation."),
                  tags$div(style="line-height:50%;", br()),
                  DT::dataTableOutput("geneMethylationTable")
                )
              )
            )
          )
        )
      )
    ),

    tabPanel(
      "Downloads",
      tags$div(style="line-height:150%;", br()),
      helpText(
        "The BCaPE portal provides options for exporting specific subsets of the data for a selected model, gene or drug."
      ),
      helpText(
        "The complete dataset can be downloaded",
        tags$a(href = "https://figshare.com/articles/Bruna_et_al_A_biobank_of_breast_cancer_explants_with_preserved_intra-tumor_heterogeneity_to_screen_anticancer_compounds_Cell_2016/2069274", target = "_blank", "here.")
      ),
      tags$div(style="line-height:400%;", br())
    ),

    tabPanel("Help",
      tags$div(style="line-height:100%;", br()),
      tabsetPanel(
        id = "helpTabsetPanel",
        tabPanel(
          "Basic concepts",
          tags$div(style="line-height:50%;", br()),
          tags$a(name = "AUC"),
          tags$h5("Area under the curve (AUC)"),
          helpText(
            "A measure of drug sensitivity based on the area under the dose-response curve.",
            "A value of 1 corresponds to toxic drugs, that is drugs that kill all cells with any dose tested.",
            "A value of 0 corresponds to drugs that don’t kill any cells at any dose tested.",
            "We have arbitrarily set a threshold on 0.2."),
          tags$h5("Volcano plot"),
          helpText(
            "A visualisation tool to highlight the association between a given biomarker and drug sensitivity.",
            "Usually the x-axis shows differences of AUC between two conditions of the biomarker (e.g. mutated versus not mutated) and the y-axis log10 p-values.",
            "For expression and methylation data, the effect size represented on the x-axis is the slope of the linear model fit to the AUC.",
            "The size of the dots is proportional to the effect size.",
            "The p-values on the y-axis have been corrected for multiple testing using the Benjamini & Hochberg method to control the false discovery rate (FDR)."
          )
        ),
        tabPanel(
          "Models view",
          tags$div(style="line-height:50%;", br()),
          helpText(
            "The left panel shows a list of all the models available in the encyclopaedia with their molecular classification (Integrative Cluster, 3-gene and intrinsic subtype).",
            "When a model is clicked, the right panel shows sensitivity to a selection of drugs tested.",
            "Sorted by more resistant to more sensitive, each dot correspond to one drug tested on the selected model and the x-axis shows the area under the curve, ",
            actionLink("navigateToBasicConcepts1", "AUC,"), "a measure of drug sensitivity.",
            "Hovering the cursor over any of the dots will display the drug tested."
          ),
          helpText("Because we still don’t have drug sensitivity data for all our models you can tick the option 'Restrict to models with drug sensitivity data' to list only the ones that have drug response data."),
          helpText("Selecting the Data tab at the bottom will show the values of all drugs.")
        ),
        tabPanel(
          "Drugs view",
          tags$div(style="line-height:50%;", br()),
          helpText(
            "The left panel shows all the drugs that have been tested, with their pathway and putative target.",
            "When a drug is clicked, the right panel shows different information depending on the tab selected:"
          ),
          tags$h5("Models"),
          helpText(
            "This panel shows a dot plot of the sensitivity of different models to the selected drug."
          ),
          helpText(
            "Sorted by more resistant to more sensitive, each dot correspond to one model tested with the selected drug and the x-axis shows the area under the curve,",
            actionLink("navigateToBasicConcepts2", "AUC,"), "a measure of drug sensitivity.",
            "Hovering the cursor over any of the dots will show the model tested; selecting the Data tab at the bottom will show the values of all models."
          ),
          tags$h5("Copy number gains"),
          helpText(
            "This panel shows a", actionLink("navigateToBasicConcepts3", "volcano plot,"),
            "that illustrates the effect of having a copy number gain in the sensitivity to the selected drug."
          ),
          helpText(
            "Each dot represents a gene; hovering the cursor over a dot will display details for that gene.",
            "The x-axis shows the difference in AUC between models with a gain in copy number in the gene and models without the gain in copy number.",
            "On the y-axis are p-values computed using t-tests and adjusted for multiple comparisons.",
            "The view can be filtered to only include genes with a mean AUC that is considered sensitive for the models with the alteration and a mean AUC resistant for the models without the alteration (or the other way around; threshold for sensitivity is 0.2).",
            "Selecting the Data tab at the bottom will show the values of all genes."
          ),
          tags$h5("Copy number losses"),
          helpText(
            "This is similar to the copy number gains panel but shows a", actionLink("navigateToBasicConcepts4", "volcano plot,"),
            "that illustrates the effect of having a copy number gain in the sensitivity to the selected drug."
          ),
          helpText(
            "Each dot represents a gene and the x axis gives the difference in AUC between models with a loss in copy number for the gene and models without a loss in copy number.",
            "On the y-axis are p-values computed using t-tests and adjusted for multiple comparisons.",
            "The view can be filtered to only include genes with a mean AUC that is considered sensitive for the models with the alteration and a mean AUC resistant for the models without the alteration (or the other way around; threshold for sensitivity is 0.2).",
            "Selecting the Data tab at the bottom will show the values of all genes."
          ),
          tags$h5("Mutations"),
          helpText(
            "This panel shows a", actionLink("navigateToBasicConcepts5", "volcano plot,"),
            "that illustrates the effect of having a mutation in the sensitivity to the selected drug."
          ),
          helpText(
            "Each dot represents a gene; hovering the cursor over a dot will display details for that gene.",
            "The x-axis shows the difference in AUC between models with the mutation in the gene and models without the mutation.",
            "On the y-axis are p-values computed using t-tests and adjusted for multiple comparisons.",
            "The view can be filtered to only include genes with a mean AUC that is considered sensitive for the models with the alteration and a mean AUC resistant for the models without the alteration (or the other way around; threshold for sensitivity is 0.2).",
            "Selecting the Data tab at the bottom will show the values of all genes."
          ),
          tags$h5("Expression"),
          helpText(
            "This panel shows a", actionLink("navigateToBasicConcepts6", "volcano plot,"),
            "that illustrates the effect of over expression in the sensitivity to the selected drug."
          ),
          helpText(
            "Each dot represents a gene; hovering the cursor over a dot will display details for that gene.",
            "The x-axis shows the slope of the change in AUC when expression changes, resulting from fitting a linear model.",
            "A positive value means that higher expression is associated with greater sensitivity.",
            "A negative value means that higher expression is associated with greater resistance.",
            "On the y-axis are p-values computed using a linear model and adjusted for multiple comparisons.",
            "Selecting the Data tab at the bottom will show the values of all genes."
          ),
          tags$h5("Methylation"),
          helpText(
            "This panel shows a", actionLink("navigateToBasicConcepts7", "volcano plot,"),
            "that illustrates the effect of promoter methylation in the sensitivity to the selected drug."
          ),
          helpText(
            "Each dot represents a gene; hovering the cursor over a dot will display details for that gene.",
            "The x-axis shows the slope of the change in AUC when promoter methylation changes, resulting from fitting a linear model.",
            "A positive value means that higher methylation is associated with greater sensitivity.",
            "A negative value means that higher methylation is associated with resistance.",
            "On the y-axis are p-values computed using a linear model and adjusted for multiple comparisons.",
            "Selecting the Data tab at the bottom will show the values of all genes."
          )
        ),
        tabPanel(
          "Genes view",
          tags$div(style="line-height:50%;", br()),
          helpText(
            "The left panel shows all the genes for which data are available in the encyclopaedia.",
            "When a gene is clicked, the right panel shows different information depending on the tab selected",
            "(copy number, mutation, expression and methylation).",
            "Molecular data and the results of a sensitivity analysis are presented for each of these data types."
          ),
          tags$h5("Sensitivity analysis"),
          helpText(
            "This panel shows a", actionLink("navigateToBasicConcepts8", "volcano plot,"),
            "that illustrates the effect of the given alteration",
            "(copy number gain or loss, mutation, over expression or promoter methylation)",
            "in the sensitivity to the drugs tested."
          ),
          helpText(
            "Each dot represents a drug; hovering the cursor over a dot will display details for that drug.",
            "Further details of the sensitivity analysis are given on the Details tab at the bottom of the panel.",
            "Selecting the Data tab will show the sensitivity analysis results for all drugs."
          ),
          tags$h5("Molecular data"),
          helpText(
            "The molecular data used in the sensitivity analyses are also given for each data type.",
            "These include copy number states, details of mutations, expression and methylation measurements",
            "for each of the models for which these data are available.",
            "Data are presented in tabular form; copy number states are also represented as a bar plot."
          )
        ),
        tabPanel(
          "Contact us",
          tags$div(style="line-height:150%;", br()),
          helpText(
            "For questions regarding data, analyses and results, or to report issues with using this site, please contact us by email at:",
            tags$a(href = "mailto:bcape@cruk.cam.ac.uk", "bcape@cruk.cam.ac.uk")
          ),
          tags$div(style="line-height:400%;", br())
        )
      )
    )
  ),

  tags$div(style = "clear:both",
    tags$div(style="line-height:100%;", br()),
    HTML("&copy;"),
    tags$script(type = "text/javascript", "var d = new Date(); document.write(d.getFullYear())"),
    "University of Cambridge",
    tags$div(style = "float:right",
      # tags$a(href = "mailto:bcape@cruk.cam.ac.uk", "Contact us"),
      actionLink("navigateToContactUs", "Contact us"),
      "|",
      tags$a(href = "http://www.cruk.cam.ac.uk/utilities/cri-tandc", target = "_blank", "Terms and Conditions")
    ),
    tags$div(style="line-height:50%;", br())
  )
)
