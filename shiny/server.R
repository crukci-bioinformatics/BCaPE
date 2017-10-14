
function(input, output, session) {

  # selected entities

  selection <- reactiveValues(
    selectedModel = NULL,
    selectedDrug = NULL,
    selectedGene = NULL
  )


  # models

  output$modelSelectionTable <- DT::renderDataTable(
    datatable(
      modelSelection,
      rownames = FALSE,
      selection = "single",
      extensions = "Buttons",
      options = list(
        dom = 'Bfrtip',
        buttons = I('pageLength'),
        pageLength = 10,
        lengthMenu = list(c(5, 10, 25, -1), c('Show 5 rows', 'Show 10 rows', 'Show 25 rows', 'Show all rows')),
        searchHighlight = TRUE
      )
    ),
    server = TRUE
  )

  modelSelectionTableProxy <- dataTableProxy('modelSelectionTable')

  observeEvent(selection$selectedModel, {
    selectRows(
      modelSelectionTableProxy,
      which(modelSelection$Model == selection$selectedModel)
    )
  })

  observe({
    selectedRow <- input$modelSelectionTable_rows_selected
    if (!is.null(selectedRow))
    {
      selectedModel <- modelSelection %>%
        slice(selectedRow) %>%
        select(Model) %>%
        unlist %>%
        as.character
      selection$selectedModel <- selectedModel
    }
  })

  output$restrictedModelSelectionTable <- DT::renderDataTable(
    datatable(
      restrictedModelSelection,
      rownames = FALSE,
      selection = "single",
      extensions = "Buttons",
      options = list(
        dom = 'Bfrtip',
        buttons = I('pageLength'),
        pageLength = 10,
        lengthMenu = list(c(5, 10, 25, -1), c('Show 5 rows', 'Show 10 rows', 'Show 25 rows', 'Show all rows')),
        searchHighlight = TRUE
      )
    ),
    server = TRUE
  )

  restrictedModelSelectionTableProxy <- dataTableProxy('restrictedModelSelectionTable')

  observeEvent(selection$selectedModel, {
    selectRows(
      restrictedModelSelectionTableProxy,
      which(restrictedModelSelection$Model == selection$selectedModel)
    )
  })

  observe({
    selectedRow <- input$restrictedModelSelectionTable_rows_selected
    if (!is.null(selectedRow))
    {
      selectedModel <- restrictedModelSelection %>%
        slice(selectedRow) %>%
        select(Model) %>%
        unlist %>%
        as.character
      selection$selectedModel <- selectedModel
    }
  })

  output$modelLabel <- renderUI({
    HTML(paste("<b>Model:</b>", selection$selectedModel))
  })

  modelSensitivityData <- reactive({
    model <- selection$selectedModel
    if (is.null(model)) model <- "NONE_SELECTED"
    drugSensitivity %>%
      filter(Model == model) %>%
      select(Drug, Model, AUC)
  })

  output$modelSensitivityDotPlot <- renderHighchart({
    aucDotPlot(
      modelSensitivityData() %>% rename(ID = Drug),
      subtitle = ifelse(nrow(modelSensitivityData()) == 0,
                        ifelse(is.null(selection$selectedModel),
                               "Select a model to view drug sensitivity data",
                               "No data available"),
                        ""),
      prefix = "Drug",
      clicked = "modelSensitivityDotPlot_clicked")
  })

  observe({
    clicked <- input$modelSensitivityDotPlot_clicked
    if (!is.null(clicked))
    {
      selection$selectedDrug <- clicked
      updateTabsetPanel(session, "mainTabsetPanel", selected = "Drugs")
    }
  })

  modelSensitivityTableData <- reactive({
    modelSensitivityData() %>%
      left_join(drugAnnotations, by = "Drug") %>%
      select(Model, one_of(colnames(drugAnnotations)), AUC)
  })

  output$modelSensitivityTable <- DT::renderDataTable(
    datatable(
      modelSensitivityTableData(),
      rownames = FALSE,
      selection = "single",
      extensions = "Buttons",
      options = list(
        dom = 'Bfrtip',
        buttons = list('pageLength', list(extend = 'excel', filename = 'model_sensitivity')),
        pageLength = 10,
        lengthMenu = list(c(10, 25, 50, -1), c('Show 10 rows', 'Show 25 rows', 'Show 50 rows', 'Show all rows')),
        searchHighlight = TRUE
      )
    ),
    server = FALSE
  )


  # drugs

  output$drugSelectionTable <- DT::renderDataTable(
    datatable(
      drugSelection,
      rownames = FALSE,
      selection = "single",
      extensions = "Buttons",
      options = list(
        dom = 'Bfrtip',
        buttons = I('pageLength'),
        pageLength = 10,
        lengthMenu = list(c(5, 10, 25, -1), c('Show 5 rows', 'Show 10 rows', 'Show 25 rows', 'Show all rows')),
        searchHighlight = TRUE
      )
    ),
    server = TRUE
  )

  drugSelectionTableProxy <- dataTableProxy('drugSelectionTable')

  observeEvent(selection$selectedDrug, {
    selectRows(
      drugSelectionTableProxy,
      which(drugSelection$Drug == selection$selectedDrug)
    )
  })

  observe({
    selectedRow <- input$drugSelectionTable_rows_selected
    if (!is.null(selectedRow))
    {
      selectedDrug <- drugSelection %>%
        slice(selectedRow) %>%
        select(Drug) %>%
        unlist %>%
        as.character
      selection$selectedDrug <- selectedDrug
    }
  })

  output$drugLabel <- renderUI({
    HTML(paste("<b>Drug:</b>", selection$selectedDrug))
  })

  drugSensitivityData <- reactive({
    drug <- selection$selectedDrug
    if (is.null(drug)) drug <- "NONE_SELECTED"
    drugSensitivity %>%
      filter(Drug == drug) %>%
      select(Drug, Model, AUC)
  })

  output$drugSensitivityDotPlot <- renderHighchart({
    aucDotPlot(
      drugSensitivityData() %>% rename(ID = Model),
      subtitle = ifelse(nrow(drugSensitivityData()) == 0,
                        ifelse(is.null(selection$selectedModel),
                               "Select a drug to view model sensitivity data",
                               "No data available"),
                        ""),
      prefix = "Model",
      clicked = "drugSensitivityDotPlot_clicked")
  })

  observe({
    clicked <- input$drugSensitivityDotPlot_clicked
    if (!is.null(clicked))
    {
      selection$selectedModel <- clicked
      updateTabsetPanel(session, "mainTabsetPanel", selected = "Models")
    }
  })

  drugSensitivityTableData <- reactive({
    drugSensitivityData() %>%
      left_join(modelClassifications, by = "Model") %>%
      select(Drug, one_of(colnames(modelClassifications)), AUC)
  })

  output$drugSensitivityTable <- DT::renderDataTable(
    datatable(
      drugSensitivityTableData(),
      rownames = FALSE,
      selection = "single",
      extensions = "Buttons",
      options = list(
        dom = 'Bfrtip',
        buttons = list('pageLength', list(extend = 'excel', filename = 'drug_sensitivity')),
        pageLength = 10,
        lengthMenu = list(c(10, 25, 50, -1), c('Show 10 rows', 'Show 25 rows', 'Show 50 rows', 'Show all rows')),
        searchHighlight = TRUE
      )
    ),
    server = FALSE
  )

  # drugs - copy number gain sensitivity

  drugCopyNumberGainSensitivity <- reactive({
    drug <- selection$selectedDrug
    if (is.null(drug)) drug <- "NONE_SELECTED"
    sensitivity <- copyNumberGainSensitivity %>%
      filter(Drug == drug) %>%
      collect
    sensitivity %>%
      select(Drug, Gene, t.statistic, p.value, mean.gain, n.gain, mean.nogain, n.nogain, fdr) %>%
      arrange(p.value)
  })

  output$drugCopyNumberGainSensitivityTable <- DT::renderDataTable(
    datatable(
      drugCopyNumberGainSensitivity(),
      rownames = FALSE,
      colnames = c("Drug", "Gene", "t-statistic", "p-value", "Mean AUC (gain)", "N (gain)", "Mean AUC (no gain)", "N (no gain)", "FDR"),
      selection = "single",
      extensions = "Buttons",
      options = list(
        dom = 'Bfrtip',
        buttons = list('pageLength', list(extend = 'excel', filename = 'copy_number_gain_sensitivity')),
        pageLength = 10,
        lengthMenu = list(c(10, 25, 50, 100), c('Show 10 rows', 'Show 25 rows', 'Show 50 rows', 'Show 100 rows')),
        searchHighlight = TRUE
      )
    ),
    server = TRUE
  )

  drugCopyNumberGainSensitivityPlotData <- reactive({
    drugCopyNumberGainSensitivity() %>%
      # arrange(p.value) %>%
      slice_and_sample(drugVolcanoPlotTopN, drugVolcanoPlotSampleN) %>%
      transmute(
        x = -t.statistic,
        y = -log10(fdr),
        z = abs(mean.gain - mean.nogain),
        series = ifelse((mean.gain < 0.2 & mean.nogain > 0.2) | (mean.gain > 0.2 & mean.nogain < 0.2), "filtered", "not filtered"),
        id = Gene,
        tooltip = paste(
          "Gene: ", Gene,
          "<br>p-value: ", p.value, ", FDR: ", fdr,
          # "<br>t-statistic: ", t.statistic,
          "<br>Gain, mean AUC: ", mean.gain, " (N = ", n.gain, ")",
          "<br>No gain, mean AUC: ", mean.nogain, " (N = ", n.nogain, ")",
          sep = ""
        )
      )
  })

  output$drugCopyNumberGainSensitivityVolcanoPlot <- renderHighchart({
    bubblePlot(
      drugCopyNumberGainSensitivityPlotData(),
      # title = ifelse(is.null(selection$selectedDrug), "", selection$selectedDrug),
      subtitle = ifelse(nrow(drugCopyNumberGainSensitivityPlotData()) == 0,
                        ifelse(is.null(selection$selectedDrug),
                               "Select a drug to view copy number gain sensitivity analysis results",
                               "No data available"),
                        "Comparison of models with and without copy number gains"),
      xLabel = "Effect (-Resistance/+Sensitivity)",
      yLabel = "-log10(FDR)",
      yLine = 1.0,
      series = c("filtered", "not filtered"),
      colours = c("#7cb5ec", "#434348"),
      minSize = "1.5%", maxSize = "6%",
      clicked = "drugCopyNumberGainSensitivityVolcanoPlot_clicked"
    )
  })

  observe({
    clicked <- input$drugCopyNumberGainSensitivityVolcanoPlot_clicked
    if (!is.null(clicked))
    {
      selection$selectedGene <- clicked
      updateTabsetPanel(session, "mainTabsetPanel", selected = "Genes")
    }
  })

  # drugs - copy number loss sensitivity

  drugCopyNumberLossSensitivity <- reactive({
    drug <- selection$selectedDrug
    if (is.null(drug)) drug <- "NONE_SELECTED"
    sensitivity <- copyNumberLossSensitivity %>%
      filter(Drug == drug) %>%
      collect
    sensitivity %>%
      select(Drug, Gene, t.statistic, p.value, mean.loss, n.loss, mean.noloss, n.noloss, fdr) %>%
      arrange(p.value)
  })

  output$drugCopyNumberLossSensitivityTable <- DT::renderDataTable(
    datatable(
      drugCopyNumberLossSensitivity(),
      rownames = FALSE,
      colnames = c("Drug", "Gene", "t-statistic", "p-value", "Mean AUC (loss)", "N (loss)", "Mean AUC (no loss)", "N (no loss)", "FDR"),
      selection = "single",
      extensions = "Buttons",
      options = list(
        dom = 'Bfrtip',
        buttons = list('pageLength', list(extend = 'excel', filename = 'copy_number_loss_sensitivity')),
        pageLength = 10,
        lengthMenu = list(c(10, 25, 50, 100), c('Show 10 rows', 'Show 25 rows', 'Show 50 rows', 'Show 100 rows')),
        searchHighlight = TRUE
      )
    ),
    server = TRUE
  )

  drugCopyNumberLossSensitivityPlotData <- reactive({
    drugCopyNumberLossSensitivity() %>%
      # arrange(p.value) %>%
      slice_and_sample(drugVolcanoPlotTopN, drugVolcanoPlotSampleN) %>%
      transmute(
        x = -t.statistic,
        y = -log10(fdr),
        z = abs(mean.loss - mean.noloss),
        series = ifelse((mean.loss < 0.2 & mean.noloss > 0.2) | (mean.loss > 0.2 & mean.noloss < 0.2), "filtered", "not filtered"),
        id = Gene,
        tooltip = paste(
          "Gene: ", Gene,
          "<br>p-value: ", p.value, ", FDR: ", fdr,
          # "<br>t-statistic: ", t.statistic,
          "<br>Loss, mean AUC: ", mean.loss, " (N = ", n.loss, ")",
          "<br>No loss, mean AUC: ", mean.noloss, " (N = ", n.noloss, ")",
          sep = ""
        )
      )
  })

  output$drugCopyNumberLossSensitivityVolcanoPlot <- renderHighchart({
    bubblePlot(
      drugCopyNumberLossSensitivityPlotData(),
      # title = ifelse(is.null(selection$selectedDrug), "", selection$selectedDrug),
      subtitle = ifelse(nrow(drugCopyNumberLossSensitivityPlotData()) == 0,
                        ifelse(is.null(selection$selectedDrug),
                               "Select a drug to view copy number loss sensitivity analysis results",
                               "No data available"),
                        "Comparison of models with and without copy number losses"),
      xLabel = "Effect (-Resistance/+Sensitivity)",
      yLabel = "-log10(FDR)",
      yLine = 1.0,
      series = c("filtered", "not filtered"),
      colours = c("#7cb5ec", "#434348"),
      minSize = "1.5%", maxSize = "6%",
      clicked = "drugCopyNumberLossSensitivityVolcanoPlot_clicked"
    )
  })

  observe({
    clicked <- input$drugCopyNumberLossSensitivityVolcanoPlot_clicked
    if (!is.null(clicked))
    {
      selection$selectedGene <- clicked
      updateTabsetPanel(session, "mainTabsetPanel", selected = "Genes")
    }
  })

  # drugs - mutation sensitivity

  drugMutationSensitivity <- reactive({
    drug <- selection$selectedDrug
    if (is.null(drug)) drug <- "NONE_SELECTED"
    sensitivity <- mutationSensitivity %>%
      filter(Drug == drug) %>%
      collect
    sensitivity %>%
      select(Drug, Gene, t.statistic, p.value, mean.mutation, n.mutation, mean.nomutation, n.nomutation, fdr) %>%
      arrange(p.value)
  })

  output$drugMutationSensitivityTable <- DT::renderDataTable(
    datatable(
      drugMutationSensitivity(),
      rownames = FALSE,
      colnames = c("Drug", "Gene", "t-statistic", "p-value", "Mean AUC (mutated)", "N (mutated)", "Mean AUC (not mutated)", "N (not mutated)", "FDR"),
      selection = "single",
      extensions = "Buttons",
      options = list(
        dom = 'Bfrtip',
        buttons = list('pageLength', list(extend = 'excel', filename = 'mutation_sensitivity')),
        pageLength = 10,
        lengthMenu = list(c(10, 25, 50, 100), c('Show 10 rows', 'Show 25 rows', 'Show 50 rows', 'Show 100 rows')),
        searchHighlight = TRUE
      )
    ),
    server = TRUE
  )

  drugMutationSensitivityPlotData <- reactive({
    drugMutationSensitivity() %>%
      # arrange(p.value) %>%
      slice_and_sample(drugVolcanoPlotTopN, drugVolcanoPlotSampleN) %>%
      transmute(
        x = -t.statistic,
        y = -log10(fdr),
        z = abs(mean.mutation - mean.nomutation),
        series = ifelse((mean.mutation < 0.2 & mean.nomutation > 0.2) | (mean.mutation > 0.2 & mean.nomutation < 0.2), "filtered", "not filtered"),
        id = Gene,
        tooltip = paste(
          "Gene: ", Gene,
          "<br>p-value: ", p.value, ", FDR: ", fdr,
          # "<br>t-statistic: ", t.statistic,
          "<br>Mutated, mean AUC: ", mean.mutation, " (N = ", n.mutation, ")",
          "<br>Not Mutated, mean AUC: ", mean.nomutation, " (N = ", n.nomutation, ")",
          sep = ""
        )
      )
  })

  output$drugMutationSensitivityVolcanoPlot <- renderHighchart({
    bubblePlot(
      drugMutationSensitivityPlotData(),
      # title = ifelse(is.null(selection$selectedDrug), "", selection$selectedDrug),
      subtitle = ifelse(nrow(drugMutationSensitivityPlotData()) == 0,
                        ifelse(is.null(selection$selectedDrug),
                               "Select a drug to view mutation sensitivity analysis results",
                               "No data available"),
                        "Comparison of mutated and unmutated models"),
      xLabel = "Effect (-Resistance/+Sensitivity)",
      yLabel = "-log10(FDR)",
      yLine = 1.0,
      series = c("filtered", "not filtered"),
      colours = c("#7cb5ec", "#434348"),
      minSize = "1.5%", maxSize = "6%",
      clicked = "drugMutationSensitivityVolcanoPlot_clicked"
    )
  })

  observe({
    clicked <- input$drugMutationSensitivityVolcanoPlot_clicked
    if (!is.null(clicked))
    {
      selection$selectedGene <- clicked
      updateTabsetPanel(session, "mainTabsetPanel", selected = "Genes")
    }
  })

  # drugs - expression sensitivity

  drugExpressionSensitivity <- reactive({
    drug <- selection$selectedDrug
    if (is.null(drug)) drug <- "NONE_SELECTED"
    sensitivity <- expressionSensitivity %>%
      filter(Drug == drug) %>%
      collect
    sensitivity %>%
      select(Drug, Gene, p.value, slope, range.auc, range.expression, n, fdr) %>%
      arrange(p.value)
  })

  output$drugExpressionSensitivityTable <- DT::renderDataTable(
    datatable(
      drugExpressionSensitivity(),
      rownames = FALSE,
      colnames = c("Drug", "Gene", "p-value", "Slope", "Range AUC", "Range expression", "N", "FDR"),
      selection = "single",
      extensions = "Buttons",
      options = list(
        dom = 'Bfrtip',
        buttons = list('pageLength', list(extend = 'excel', filename = 'expression_sensitivity')),
        pageLength = 10,
        lengthMenu = list(c(10, 25, 50, 100), c('Show 10 rows', 'Show 25 rows', 'Show 50 rows', 'Show 100 rows')),
        searchHighlight = TRUE
      )
    ),
    server = TRUE
  )

  drugExpressionSensitivityPlotData <- reactive({
    drugExpressionSensitivity() %>%
      # arrange(p.value) %>%
      slice_and_sample(drugVolcanoPlotTopN, drugVolcanoPlotSampleN) %>%
      transmute(
        x = slope,
        y = -log10(fdr),
        z = abs(slope),
        series = "not filtered",
        id = Gene,
        tooltip = paste(
          "Gene: ", Gene,
          "<br>p-value: ", p.value, ", FDR: ", fdr,
          "<br>Slope: ", slope,
          #         "<br>Range AUC: ", range.auc,
          #         "<br>Range expression: ", range.expression,
          "<br>N: ", n,
          sep = ""
        )
      )
  })

  output$drugExpressionSensitivityVolcanoPlot <- renderHighchart({
    bubblePlot(
      drugExpressionSensitivityPlotData(),
      # title = ifelse(is.null(selection$selectedDrug), "", selection$selectedDrug),
      subtitle = ifelse(nrow(drugExpressionSensitivityPlotData()) == 0,
                        ifelse(is.null(selection$selectedDrug),
                               "Select a drug to view expression sensitivity analysis results",
                               "No data available"),
                        "Linear model fit for expression and drug sensitivity (AUC)"),
      xLabel = "Effect (-Resistance/+Sensitivity)",
      yLabel = "-log10(FDR)",
      yLine = 1.0,
      series = c("not filtered"),
      colours = c("#434348"),
      showInLegend = FALSE,
      minSize = "1.5%", maxSize = "6%",
      clicked = "drugExpressionSensitivityVolcanoPlot_clicked"
    )
  })

  observe({
    clicked <- input$drugExpressionSensitivityVolcanoPlot_clicked
    if (!is.null(clicked))
    {
      selection$selectedGene <- clicked
      updateTabsetPanel(session, "mainTabsetPanel", selected = "Genes")
    }
  })

  # drugs - methylation sensitivity

  drugMethylationSensitivity <- reactive({
    drug <- selection$selectedDrug
    if (is.null(drug)) drug <- "NONE_SELECTED"
    sensitivity <- methylationSensitivity %>%
      filter(Drug == drug) %>%
      collect
    sensitivity %>%
      select(Drug, Gene, p.value, slope, range.auc, range.methylation, n, fdr) %>%
      arrange(p.value)
  })

  output$drugMethylationSensitivityTable <- DT::renderDataTable(
    datatable(
      drugMethylationSensitivity(),
      rownames = FALSE,
      colnames = c("Drug", "Gene", "p-value", "Slope", "Range AUC", "Range methylation", "N", "FDR"),
      selection = "single",
      extensions = "Buttons",
      options = list(
        dom = 'Bfrtip',
        buttons = list('pageLength', list(extend = 'excel', filename = 'methylation_sensitivity')),
        pageLength = 10,
        lengthMenu = list(c(10, 25, 50, 100), c('Show 10 rows', 'Show 25 rows', 'Show 50 rows', 'Show 100 rows')),
        searchHighlight = TRUE
      )
    ),
    server = TRUE
  )

  drugMethylationSensitivityPlotData <- reactive({
    drugMethylationSensitivity() %>%
      # arrange(p.value) %>%
      slice_and_sample(drugVolcanoPlotTopN, drugVolcanoPlotSampleN) %>%
      transmute(
        x = slope,
        y = -log10(fdr),
        z = abs(slope),
        series = ifelse(p.value <= 0.05 & range.auc >= 0.25, "filtered", "not filtered"),
        id = Gene,
        tooltip = paste(
          "Gene: ", Gene,
          "<br>p-value: ", p.value, ", FDR: ", fdr,
          "<br>Slope: ", slope,
          #         "<br>Range AUC: ", range.auc,
          #         "<br>Range methylation: ", range.methylation,
          "<br>N: ", n,
          sep = ""
        )
      )
  })

  output$drugMethylationSensitivityVolcanoPlot <- renderHighchart({
    bubblePlot(
      drugMethylationSensitivityPlotData(),
      # title = ifelse(is.null(selection$selectedDrug), "", selection$selectedDrug),
      subtitle = ifelse(nrow(drugMethylationSensitivityPlotData()) == 0,
                        ifelse(is.null(selection$selectedDrug),
                               "Select a drug to view methylation sensitivity analysis results",
                               "No data available"),
                        "Linear model fit for methylation and drug sensitivity (AUC)"),
      xLabel = "Effect (-Resistance/+Sensitivity)",
      yLabel = "-log10(FDR)",
      yLine = 1.0,
      series = c("filtered", "not filtered"),
      colours = c("#7cb5ec", "#434348"),
      minSize = "1.5%", maxSize = "6%",
      clicked = "drugMethylationSensitivityVolcanoPlot_clicked"
    )
  })

  observe({
    clicked <- input$drugMethylationSensitivityVolcanoPlot_clicked
    if (!is.null(clicked))
    {
      selection$selectedGene <- clicked
      updateTabsetPanel(session, "mainTabsetPanel", selected = "Genes")
    }
  })


  # genes

  geneDetails <- reactive({
    collect(tbl(db, "geneDetails")) %>%
      mutate(`Name [Aliases]` = Details)
  })

  output$geneLabel <- renderUI({
    HTML(paste("<b>Gene:</b>", selection$selectedGene))
  })

  # genes - copy number states

  geneCopyNumberStateSelectionTableData <- reactive({
    geneDetails() %>%
      filter(CopyNumberState == 1) %>%
      select(Gene, `Name [Aliases]`)
  })

  output$geneCopyNumberStateSelectionTable <- DT::renderDataTable(
    datatable(
      geneCopyNumberStateSelectionTableData(),
      rownames = FALSE,
      selection = "single",
      extensions = "Buttons",
      options = list(
        dom = 'Bfrtip',
        buttons = I('pageLength'),
        pageLength = 10,
        lengthMenu = list(c(5, 10, 25, -1), c('Show 5 rows', 'Show 10 rows', 'Show 25 rows', 'Show all rows')),
        searchHighlight = TRUE
      )
    ),
    server = TRUE
  )

  geneCopyNumberStateSelectionTableProxy <- dataTableProxy('geneCopyNumberStateSelectionTable')

  observeEvent(selection$selectedGene, {
    selectRows(
      geneCopyNumberStateSelectionTableProxy,
      which(geneCopyNumberStateSelectionTableData()$Gene == selection$selectedGene)
    )
  })

  observe({
    selectedRow <- input$geneCopyNumberStateSelectionTable_rows_selected
    selectedGene <- NULL
    if (!is.null(selectedRow))
    {
      selectedGene <- geneCopyNumberStateSelectionTableData() %>%
        slice(selectedRow) %>%
        select(Gene) %>%
        unlist %>%
        as.character
      selection$selectedGene <- selectedGene
    }
  })

  geneCopyNumberStates <- reactive({
    gene <- selection$selectedGene
    if (is.null(gene)) gene <- "NONE_SELECTED"
    copyNumberStates %>%
      filter(Gene == gene) %>%
      collect
  })

  output$geneCopyNumberStateBarPlot <- renderHighchart({
    barPlot(
      geneCopyNumberStates()$State,
      # title = ifelse(is.null(selection$selectedGene), "", selection$selectedGene),
      subtitle = ifelse(nrow(geneCopyNumberStates()) == 0, "Select a gene to view copy number states", ""),
      categories = c("gain", "neutral", "loss", "unknown"),
      colours = c("#0000FF", "#BEBEBE", "#FF0000", "#E0E0E0")
    )
  })

  geneCopyNumberStateTableData <- reactive({
    geneCopyNumberStates() %>%
      rename(`Copy Number State` = State) %>%
      left_join(modelClassifications, by = "Model") %>%
      select(Gene, one_of(colnames(modelClassifications)), `Copy Number State`)
  })

  output$geneCopyNumberStateTable <- DT::renderDataTable(
    datatable(
      geneCopyNumberStateTableData(),
      rownames = FALSE,
      selection = "single",
      extensions = "Buttons",
      options = list(
        dom = 'Bfrtip',
        buttons = list('pageLength', list(extend = 'excel', filename = 'copy_number_states')),
        pageLength = 10,
        lengthMenu = list(c(10, 25, 50, -1), c('Show 10 rows', 'Show 25 rows', 'Show 50 rows', 'Show all rows')),
        searchHighlight = TRUE
      )
    ),
    server = FALSE
  )

  # genes - copy number gain sensitivity

  geneCopyNumberGainSensitivitySelectionTableData <- reactive({
    geneDetails() %>%
      filter(CopyNumberGainSensitivity == 1) %>%
      select(Gene, `Name [Aliases]`)
  })

  output$geneCopyNumberGainSensitivitySelectionTable <- DT::renderDataTable(
    datatable(
      geneCopyNumberGainSensitivitySelectionTableData(),
      rownames = FALSE,
      selection = "single",
      extensions = "Buttons",
      options = list(
        dom = 'Bfrtip',
        buttons = I('pageLength'),
        pageLength = 10,
        lengthMenu = list(c(5, 10, 25, -1), c('Show 5 rows', 'Show 10 rows', 'Show 25 rows', 'Show all rows')),
        searchHighlight = TRUE
      )
    ),
    server = TRUE
  )

  geneCopyNumberGainSensitivitySelectionTableProxy <- dataTableProxy('geneCopyNumberGainSensitivitySelectionTable')

  observeEvent(selection$selectedGene, {
    selectRows(
      geneCopyNumberGainSensitivitySelectionTableProxy,
      which(geneCopyNumberGainSensitivitySelectionTableData()$Gene == selection$selectedGene)
    )
  })

  observe({
    selectedRow <- input$geneCopyNumberGainSensitivitySelectionTable_rows_selected
    selectedGene <- NULL
    if (!is.null(selectedRow))
    {
      selectedGene <- geneCopyNumberGainSensitivitySelectionTableData() %>%
        slice(selectedRow) %>%
        select(Gene) %>%
        unlist %>%
        as.character
      selection$selectedGene <- selectedGene
    }
  })

  geneCopyNumberGainSensitivity <- reactive({
    gene <- selection$selectedGene
    if (is.null(gene)) gene <- "NONE_SELECTED"
    copyNumberGainSensitivity %>%
      filter(Gene == gene) %>%
      collect %>%
      select(Gene, Drug, t.statistic, p.value, mean.gain, n.gain, mean.nogain, n.nogain, fdr) %>%
      arrange(p.value)
  })

  output$geneCopyNumberGainSensitivityTable <- DT::renderDataTable(
    datatable(
      geneCopyNumberGainSensitivity(),
      rownames = FALSE,
      colnames = c("Gene", "Drug", "t-statistic", "p-value", "Mean AUC (gain)", "N (gain)", "Mean AUC (no gain)", "N (no gain)", "FDR"),
      selection = "single",
      extensions = "Buttons",
      options = list(
        dom = 'Bfrtip',
        buttons = list('pageLength', list(extend = 'excel', filename = 'copy_number_gain_sensitivity')),
        pageLength = 10,
        lengthMenu = list(c(10, 25, 50, -1), c('Show 10 rows', 'Show 25 rows', 'Show 50 rows', 'Show all rows')),
        searchHighlight = TRUE
      )
    ),
    server = FALSE
  )

  geneCopyNumberGainSensitivityPlotData <- reactive({
    geneCopyNumberGainSensitivity() %>% transmute(
      x = -t.statistic,
      y = -log10(fdr),
      z = abs(mean.gain - mean.nogain),
      series = ifelse((mean.gain < 0.2 & mean.nogain > 0.2) | (mean.gain > 0.2 & mean.nogain < 0.2), "filtered", "not filtered"),
      id = Drug,
      tooltip = paste(
        "Drug: ", Drug,
        "<br>p-value: ", p.value, ", FDR: ", fdr,
        # "<br>t-statistic: ", t.statistic,
        "<br>Gain, mean AUC: ", mean.gain, " (N = ", n.gain, ")",
        "<br>No gain, mean AUC: ", mean.nogain, " (N = ", n.nogain, ")",
        sep = ""
      )
    )
  })

  output$geneCopyNumberGainSensitivityVolcanoPlot <- renderHighchart({
    bubblePlot(
      geneCopyNumberGainSensitivityPlotData(),
      # title = ifelse(is.null(selection$selectedGene), "", selection$selectedGene),
      subtitle = ifelse(nrow(geneCopyNumberGainSensitivityPlotData()) == 0,
                        ifelse(is.null(selection$selectedGene),
                               "Select a gene to view copy number gain sensitivity analysis results",
                               "No data available"),
                        "Comparison of models with and without copy number gains"),
      xLabel = "Effect (-Resistance/+Sensitivity)",
      yLabel = "-log10(FDR)",
      yLine = 1.0,
      series = c("filtered", "not filtered"),
      colours = c("#7cb5ec", "#434348"),
      clicked = "geneCopyNumberGainSensitivityVolcanoPlot_clicked"
    )
  })

  observe({
    clicked <- input$geneCopyNumberGainSensitivityVolcanoPlot_clicked
    if (!is.null(clicked))
    {
      selection$selectedDrug <- clicked
      updateTabsetPanel(session, "mainTabsetPanel", selected = "Drugs")
    }
  })

  # genes - copy number loss sensitivity

  geneCopyNumberLossSensitivitySelectionTableData <- reactive({
    geneDetails() %>%
      filter(CopyNumberLossSensitivity == 1) %>%
      select(Gene, `Name [Aliases]`)
  })

  output$geneCopyNumberLossSensitivitySelectionTable <- DT::renderDataTable(
    datatable(
      geneCopyNumberLossSensitivitySelectionTableData(),
      rownames = FALSE,
      selection = "single",
      extensions = "Buttons",
      options = list(
        dom = 'Bfrtip',
        buttons = I('pageLength'),
        pageLength = 10,
        lengthMenu = list(c(5, 10, 25, -1), c('Show 5 rows', 'Show 10 rows', 'Show 25 rows', 'Show all rows')),
        searchHighlight = TRUE
      )
    ),
    server = TRUE
  )

  geneCopyNumberLossSensitivitySelectionTableProxy <- dataTableProxy('geneCopyNumberLossSensitivitySelectionTable')

  observeEvent(selection$selectedGene, {
    selectRows(
      geneCopyNumberLossSensitivitySelectionTableProxy,
      which(geneCopyNumberLossSensitivitySelectionTableData()$Gene == selection$selectedGene)
    )
  })

  observe({
    selectedRow <- input$geneCopyNumberLossSensitivitySelectionTable_rows_selected
    selectedGene <- NULL
    if (!is.null(selectedRow))
    {
      selectedGene <- geneCopyNumberLossSensitivitySelectionTableData() %>%
        slice(selectedRow) %>%
        select(Gene) %>%
        unlist %>%
        as.character
      selection$selectedGene <- selectedGene
    }
  })

  geneCopyNumberLossSensitivity <- reactive({
    gene <- selection$selectedGene
    if (is.null(gene)) gene <- "NONE_SELECTED"
    copyNumberLossSensitivity %>%
      filter(Gene == gene) %>%
      collect %>%
      select(Gene, Drug, t.statistic, p.value, mean.loss, n.loss, mean.noloss, n.noloss, fdr) %>%
      arrange(p.value)
  })

  output$geneCopyNumberLossSensitivityTable <- DT::renderDataTable(
    datatable(
      geneCopyNumberLossSensitivity(),
      rownames = FALSE,
      colnames = c("Gene", "Drug", "t-statistic", "p-value", "Mean AUC (loss)", "N (loss)", "Mean AUC (no loss)", "N (no loss)", "FDR"),
      selection = "single",
      extensions = "Buttons",
      options = list(
        dom = 'Bfrtip',
        buttons = list('pageLength', list(extend = 'excel', filename = 'copy_number_loss_sensitivity')),
        pageLength = 10,
        lengthMenu = list(c(10, 25, 50, -1), c('Show 10 rows', 'Show 25 rows', 'Show 50 rows', 'Show all rows')),
        searchHighlight = TRUE
      )
    ),
    server = FALSE
  )

  geneCopyNumberLossSensitivityPlotData <- reactive({
    geneCopyNumberLossSensitivity() %>% transmute(
      x = -t.statistic,
      y = -log10(fdr),
      z = abs(mean.loss - mean.noloss),
      series = ifelse((mean.loss < 0.2 & mean.noloss > 0.2) | (mean.loss > 0.2 & mean.noloss < 0.2), "filtered", "not filtered"),
      id = Drug,
      tooltip = paste(
        "Drug: ", Drug,
        "<br>p-value: ", p.value, ", FDR: ", fdr,
        # "<br>t-statistic: ", t.statistic,
        "<br>Loss, mean AUC: ", mean.loss, " (N = ", n.loss, ")",
        "<br>No Loss, mean AUC: ", mean.noloss, " (N = ", n.noloss, ")",
        sep = ""
      )
    )
  })

  output$geneCopyNumberLossSensitivityVolcanoPlot <- renderHighchart({
    bubblePlot(
      geneCopyNumberLossSensitivityPlotData(),
      # title = ifelse(is.null(selection$selectedGene), "", selection$selectedGene),
      subtitle = ifelse(nrow(geneCopyNumberLossSensitivityPlotData()) == 0,
                        ifelse(is.null(selection$selectedGene),
                               "Select a gene to view copy number loss sensitivity analysis results",
                               "No data available"),
                        "Comparison of models with and without copy number losses"),
      xLabel = "Effect (-Resistance/+Sensitivity)",
      yLabel = "-log10(FDR)",
      yLine = 1.0,
      series = c("filtered", "not filtered"),
      colours = c("#7cb5ec", "#434348"),
      clicked = "geneCopyNumberLossSensitivityVolcanoPlot_clicked"
    )
  })

  observe({
    clicked <- input$geneCopyNumberLossSensitivityVolcanoPlot_clicked
    if (!is.null(clicked))
    {
      selection$selectedDrug <- clicked
      updateTabsetPanel(session, "mainTabsetPanel", selected = "Drugs")
    }
  })

  # genes - mutations

  geneMutationSelectionTableData <- reactive({
    geneDetails() %>%
      filter(Mutation == 1) %>%
      select(Gene, `Name [Aliases]`)
  })

  output$geneMutationSelectionTable <- DT::renderDataTable(
    datatable(
      geneMutationSelectionTableData(),
      rownames = FALSE,
      selection = "single",
      extensions = "Buttons",
      options = list(
        dom = 'Bfrtip',
        buttons = I('pageLength'),
        pageLength = 10,
        lengthMenu = list(c(5, 10, 25, -1), c('Show 5 rows', 'Show 10 rows', 'Show 25 rows', 'Show all rows')),
        searchHighlight = TRUE
      )
    ),
    server = TRUE
  )

  geneMutationSelectionTableProxy <- dataTableProxy('geneMutationSelectionTable')

  observeEvent(selection$selectedGene, {
    selectRows(
      geneMutationSelectionTableProxy,
      which(geneMutationSelectionTableData()$Gene == selection$selectedGene)
    )
  })

  observe({
    selectedRow <- input$geneMutationSelectionTable_rows_selected
    selectedGene <- NULL
    if (!is.null(selectedRow))
    {
      selectedGene <- geneMutationSelectionTableData() %>%
        slice(selectedRow) %>%
        select(Gene) %>%
        unlist %>%
        as.character
      selection$selectedGene <- selectedGene
    }
  })

  geneMutations <- reactive({
    gene <- selection$selectedGene
    if (is.null(gene)) gene <- "NONE_SELECTED"
    mutations %>%
      filter(Gene == gene) %>%
      collect %>%
      arrange(desc(Mutated)) %>%
      mutate(Mutated = ifelse(Mutated, "Yes", "No"))
  })

  output$geneMutationsTable <- DT::renderDataTable(
    datatable(
      geneMutations(),
      rownames = FALSE,
      selection = "single",
      extensions = "Buttons",
      options = list(
        dom = 'Bfrtip',
        buttons = list('pageLength', list(extend = 'excel', filename = 'mutations')),
        pageLength = 10,
        lengthMenu = list(c(10, 25, 50, -1), c('Show 10 rows', 'Show 25 rows', 'Show 50 rows', 'Show all rows')),
        searchHighlight = TRUE
      )
    ),
    server = FALSE
  )

  # genes - mutation sensitivity

  geneMutationSensitivitySelectionTableData <- reactive({
    geneDetails() %>%
      filter(MutationSensitivity == 1) %>%
      select(Gene, `Name [Aliases]`)
  })

  output$geneMutationSensitivitySelectionTable <- DT::renderDataTable(
    datatable(
      geneMutationSensitivitySelectionTableData(),
      rownames = FALSE,
      selection = "single",
      extensions = "Buttons",
      options = list(
        dom = 'Bfrtip',
        buttons = I('pageLength'),
        pageLength = 10,
        lengthMenu = list(c(5, 10, 25, -1), c('Show 5 rows', 'Show 10 rows', 'Show 25 rows', 'Show all rows')),
        searchHighlight = TRUE
      )
    ),
    server = TRUE
  )

  geneMutationSensitivitySelectionTableProxy <- dataTableProxy('geneMutationSensitivitySelectionTable')

  observeEvent(selection$selectedGene, {
    selectRows(
      geneMutationSensitivitySelectionTableProxy,
      which(geneMutationSensitivitySelectionTableData()$Gene == selection$selectedGene)
    )
  })

  observe({
    selectedRow <- input$geneMutationSensitivitySelectionTable_rows_selected
    selectedGene <- NULL
    if (!is.null(selectedRow))
    {
      selectedGene <- geneMutationSensitivitySelectionTableData() %>%
        slice(selectedRow) %>%
        select(Gene) %>%
        unlist %>%
        as.character
      selection$selectedGene <- selectedGene
    }
  })

  geneMutationSensitivity <- reactive({
    gene <- selection$selectedGene
    if (is.null(gene)) gene <- "NONE_SELECTED"
    mutationSensitivity %>%
      filter(Gene == gene) %>%
      collect %>%
      select(Gene, Drug, t.statistic, p.value, mean.mutation, n.mutation, mean.nomutation, n.nomutation, fdr) %>%
      arrange(p.value)
  })

  output$geneMutationSensitivityTable <- DT::renderDataTable(
    datatable(
      geneMutationSensitivity(),
      rownames = FALSE,
      colnames = c("Gene", "Drug", "t-statistic", "p-value", "Mean AUC (mutated)", "N (mutated)", "Mean AUC (not mutated)", "N (not mutated)", "FDR"),
      selection = "single",
      extensions = "Buttons",
      options = list(
        dom = 'Bfrtip',
        buttons = list('pageLength', list(extend = 'excel', filename = 'mutation_sensitivity')),
        pageLength = 10,
        lengthMenu = list(c(10, 25, 50, -1), c('Show 10 rows', 'Show 25 rows', 'Show 50 rows', 'Show all rows')),
        searchHighlight = TRUE
      )
    ),
    server = FALSE
  )

  geneMutationSensitivityPlotData <- reactive({
    geneMutationSensitivity() %>% transmute(
      x = -t.statistic,
      y = -log10(fdr),
      z = abs(mean.mutation - mean.nomutation),
      series = ifelse((mean.mutation < 0.2 & mean.nomutation > 0.2) | (mean.mutation > 0.2 & mean.nomutation < 0.2), "filtered", "not filtered"),
      id = Drug,
      tooltip = paste(
        "Drug: ", Drug,
        "<br>p-value: ", p.value, ", FDR: ", fdr,
        # "<br>t-statistic: ", t.statistic,
        "<br>Mutated, mean AUC: ", mean.mutation, " (N = ", n.mutation, ")",
        "<br>Not Mutated, mean AUC: ", mean.nomutation, " (N = ", n.nomutation, ")",
        sep = ""
      )
    )
  })

  output$geneMutationSensitivityVolcanoPlot <- renderHighchart({
    bubblePlot(
      geneMutationSensitivityPlotData(),
      # title = ifelse(is.null(selection$selectedGene), "", selection$selectedGene),
      subtitle = ifelse(nrow(geneMutationSensitivityPlotData()) == 0,
                        ifelse(is.null(selection$selectedGene),
                               "Select a gene to view mutation sensitivity analysis results",
                               "No data available"),
                        "Comparison of mutated and unmutated models"),
      xLabel = "Effect (-Resistance/+Sensitivity)",
      yLabel = "-log10(FDR)",
      yLine = 1.0,
      series = c("filtered", "not filtered"),
      colours = c("#7cb5ec", "#434348"),
      clicked = "geneMutationSensitivityVolcanoPlot_clicked"
    )
  })

  observe({
    clicked <- input$geneMutationSensitivityVolcanoPlot_clicked
    if (!is.null(clicked))
    {
      selection$selectedDrug <- clicked
      updateTabsetPanel(session, "mainTabsetPanel", selected = "Drugs")
    }
  })


  # genes - gene expression

  geneExpressionSelectionTableData <- reactive({
    geneDetails() %>%
      filter(Expression == 1) %>%
      select(Gene, `Name [Aliases]`)
  })

  output$geneExpressionSelectionTable <- DT::renderDataTable(
    datatable(
      geneExpressionSelectionTableData(),
      rownames = FALSE,
      selection = "single",
      extensions = "Buttons",
      options = list(
        dom = 'Bfrtip',
        buttons = I('pageLength'),
        pageLength = 10,
        lengthMenu = list(c(5, 10, 25, -1), c('Show 5 rows', 'Show 10 rows', 'Show 25 rows', 'Show all rows')),
        searchHighlight = TRUE
      )
    ),
    server = TRUE
  )

  geneExpressionSelectionTableProxy <- dataTableProxy('geneExpressionSelectionTable')

  observeEvent(selection$selectedGene, {
    selectRows(
      geneExpressionSelectionTableProxy,
      which(geneExpressionSelectionTableData()$Gene == selection$selectedGene)
    )
  })

  observe({
    selectedRow <- input$geneExpressionSelectionTable_rows_selected
    selectedGene <- NULL
    if (!is.null(selectedRow))
    {
      selectedGene <- geneExpressionSelectionTableData() %>%
        slice(selectedRow) %>%
        select(Gene) %>%
        unlist %>%
        as.character
      selection$selectedGene <- selectedGene
    }
  })

  geneExpression <- reactive({
    gene <- selection$selectedGene
    if (is.null(gene)) gene <- "NONE_SELECTED"
    expression %>%
      filter(Gene == gene) %>%
      collect
  })

  geneExpressionTableData <- reactive({
    geneExpression() %>%
      left_join(modelClassifications, by = "Model") %>%
      select(Gene, one_of(colnames(modelClassifications)), Expression)
  })

  output$geneExpressionTable <- DT::renderDataTable(
    datatable(
      geneExpressionTableData(),
      rownames = FALSE,
      selection = "single",
      extensions = "Buttons",
      options = list(
        dom = 'Bfrtip',
        buttons = list('pageLength', list(extend = 'excel', filename = 'expression')),
        pageLength = 10,
        lengthMenu = list(c(10, 25, 50, -1), c('Show 10 rows', 'Show 25 rows', 'Show 50 rows', 'Show all rows')),
        searchHighlight = TRUE
      )
    ),
    server = FALSE
  )

  # genes - gene expression sensitivity

  geneExpressionSensitivitySelectionTableData <- reactive({
    geneDetails() %>%
      filter(ExpressionSensitivity == 1) %>%
      select(Gene, `Name [Aliases]`)
  })

  output$geneExpressionSensitivitySelectionTable <- DT::renderDataTable(
    datatable(
      geneExpressionSensitivitySelectionTableData(),
      rownames = FALSE,
      selection = "single",
      extensions = "Buttons",
      options = list(
        dom = 'Bfrtip',
        buttons = I('pageLength'),
        pageLength = 10,
        lengthMenu = list(c(5, 10, 25, -1), c('Show 5 rows', 'Show 10 rows', 'Show 25 rows', 'Show all rows')),
        searchHighlight = TRUE
      )
    ),
    server = TRUE
  )

  geneExpressionSensitivitySelectionTableProxy <- dataTableProxy('geneExpressionSensitivitySelectionTable')

  observeEvent(selection$selectedGene, {
    selectRows(
      geneExpressionSensitivitySelectionTableProxy,
      which(geneExpressionSensitivitySelectionTableData()$Gene == selection$selectedGene)
    )
  })

  observe({
    selectedRow <- input$geneExpressionSensitivitySelectionTable_rows_selected
    selectedGene <- NULL
    if (!is.null(selectedRow))
    {
      selectedGene <- geneExpressionSensitivitySelectionTableData() %>%
        slice(selectedRow) %>%
        select(Gene) %>%
        unlist %>%
        as.character
      selection$selectedGene <- selectedGene
    }
  })

  geneExpressionSensitivity <- reactive({
    gene <- selection$selectedGene
    if (is.null(gene)) gene <- "NONE_SELECTED"
    expressionSensitivity %>%
      filter(Gene == gene) %>%
      collect %>%
      select(Gene, Drug, p.value, slope, range.auc, range.expression, n, fdr) %>%
      arrange(p.value)
  })

  output$geneExpressionSensitivityTable <- DT::renderDataTable(
    datatable(
      geneExpressionSensitivity(),
      rownames = FALSE,
      colnames = c("Gene", "Drug", "p-value", "Slope", "Range AUC", "Range expression", "N", "FDR"),
      selection = "single",
      extensions = "Buttons",
      options = list(
        dom = 'Bfrtip',
        buttons = list('pageLength', list(extend = 'excel', filename = 'expression_sensitivity')),
        pageLength = 10,
        lengthMenu = list(c(10, 25, 50, -1), c('Show 10 rows', 'Show 25 rows', 'Show 50 rows', 'Show all rows')),
        searchHighlight = TRUE
      )
    ),
    server = FALSE
  )

  geneExpressionSensitivityPlotData <- reactive({
    geneExpressionSensitivity() %>% transmute(
      x = slope,
      y = -log10(fdr),
      z = abs(slope),
      series = "not filtered",
      id = Drug,
      tooltip = paste(
        "Drug: ", Drug,
        "<br>p-value: ", p.value, ", FDR: ", fdr,
        "<br>Slope: ", slope,
        #         "<br>Range AUC: ", range.auc,
        #         "<br>Range expression: ", range.expression,
        "<br>N: ", n,
        sep = ""
      )
    )
  })

  output$geneExpressionSensitivityVolcanoPlot <- renderHighchart({
    bubblePlot(
      geneExpressionSensitivityPlotData(),
      # title = ifelse(is.null(selection$selectedGene), "", selection$selectedGene),
      subtitle = ifelse(nrow(geneExpressionSensitivityPlotData()) == 0,
                        ifelse(is.null(selection$selectedGene),
                               "Select a gene to view expression sensitivity analysis results",
                               "No data available"),
                        "Linear model fit for expression and drug sensitivity (AUC)"),
      xLabel = "Effect (-Resistance/+Sensitivity)",
      yLabel = "-log10(FDR)",
      yLine = 1.0,
      series = c("not filtered"),
      colours = c("#434348"),
      showInLegend = FALSE,
      clicked = "geneExpressionSensitivityVolcanoPlot_clicked"
    )
  })

  observe({
    clicked <- input$geneExpressionSensitivityVolcanoPlot_clicked
    if (!is.null(clicked))
    {
      selection$selectedDrug <- clicked
      updateTabsetPanel(session, "mainTabsetPanel", selected = "Drugs")
    }
  })

  # genes - methylation

  geneMethylationSelectionTableData <- reactive({
    geneDetails() %>%
      filter(Methylation == 1) %>%
      select(Gene, `Name [Aliases]`)
  })

  output$geneMethylationSelectionTable <- DT::renderDataTable(
    datatable(
      geneMethylationSelectionTableData(),
      rownames = FALSE,
      selection = "single",
      extensions = "Buttons",
      options = list(
        dom = 'Bfrtip',
        buttons = I('pageLength'),
        pageLength = 10,
        lengthMenu = list(c(5, 10, 25, -1), c('Show 5 rows', 'Show 10 rows', 'Show 25 rows', 'Show all rows')),
        searchHighlight = TRUE
      )
    ),
    server = TRUE
  )

  geneMethylationSelectionTableProxy <- dataTableProxy('geneMethylationSelectionTable')

  observeEvent(selection$selectedGene, {
    selectRows(
      geneMethylationSelectionTableProxy,
      which(geneMethylationSelectionTableData()$Gene == selection$selectedGene)
    )
  })

  observe({
    selectedRow <- input$geneMethylationSelectionTable_rows_selected
    selectedGene <- NULL
    if (!is.null(selectedRow))
    {
      selectedGene <- geneMethylationSelectionTableData() %>%
        slice(selectedRow) %>%
        select(Gene) %>%
        unlist %>%
        as.character
      selection$selectedGene <- selectedGene
    }
  })

  geneMethylation <- reactive({
    gene <- selection$selectedGene
    if (is.null(gene)) gene <- "NONE_SELECTED"
    methylation %>%
      filter(Gene == gene) %>%
      collect
  })

  geneMethylationTableData <- reactive({
    geneMethylation() %>%
      left_join(modelClassifications, by = "Model") %>%
      select(Gene, one_of(colnames(modelClassifications)), Methylation)
  })

  output$geneMethylationTable <- DT::renderDataTable(
    datatable(
      geneMethylationTableData(),
      rownames = FALSE,
      selection = "single",
      extensions = "Buttons",
      options = list(
        dom = 'Bfrtip',
        buttons = list('pageLength', list(extend = 'excel', filename = 'methylation')),
        pageLength = 10,
        lengthMenu = list(c(10, 25, 50, -1), c('Show 10 rows', 'Show 25 rows', 'Show 50 rows', 'Show all rows')),
        searchHighlight = TRUE
      )
    ),
    server = FALSE
  )

  # genes - methylation sensitivity

  geneMethylationSensitivitySelectionTableData <- reactive({
    geneDetails() %>%
      filter(MethylationSensitivity == 1) %>%
      select(Gene, `Name [Aliases]`)
  })

  output$geneMethylationSensitivitySelectionTable <- DT::renderDataTable(
    datatable(
      geneMethylationSensitivitySelectionTableData(),
      rownames = FALSE,
      selection = "single",
      extensions = "Buttons",
      options = list(
        dom = 'Bfrtip',
        buttons = I('pageLength'),
        pageLength = 10,
        lengthMenu = list(c(5, 10, 25, -1), c('Show 5 rows', 'Show 10 rows', 'Show 25 rows', 'Show all rows')),
        searchHighlight = TRUE
      )
    ),
    server = TRUE
  )

  geneMethylationSensitivitySelectionTableProxy <- dataTableProxy('geneMethylationSensitivitySelectionTable')

  observeEvent(selection$selectedGene, {
    selectRows(
      geneMethylationSensitivitySelectionTableProxy,
      which(geneMethylationSensitivitySelectionTableData()$Gene == selection$selectedGene)
    )
  })

  observe({
    selectedRow <- input$geneMethylationSensitivitySelectionTable_rows_selected
    selectedGene <- NULL
    if (!is.null(selectedRow))
    {
      selectedGene <- geneMethylationSensitivitySelectionTableData() %>%
        slice(selectedRow) %>%
        select(Gene) %>%
        unlist %>%
        as.character
      selection$selectedGene <- selectedGene
    }
  })

  geneMethylationSensitivity <- reactive({
    gene <- selection$selectedGene
    if (is.null(gene)) gene <- "NONE_SELECTED"
    methylationSensitivity %>%
      filter(Gene == gene) %>%
      collect %>%
      select(Gene, Drug, p.value, slope, range.auc, range.methylation, n, fdr) %>%
      arrange(p.value)
  })

  output$geneMethylationSensitivityTable <- DT::renderDataTable(
    datatable(
      geneMethylationSensitivity(),
      rownames = FALSE,
      colnames = c("Gene", "Drug", "p-value", "Slope", "Range AUC", "Range methylation", "N", "FDR"),
      selection = "single",
      extensions = "Buttons",
      options = list(
        dom = 'Bfrtip',
        buttons = list('pageLength', list(extend = 'excel', filename = 'methylation_sensitivity')),
        pageLength = 10,
        lengthMenu = list(c(10, 25, 50, -1), c('Show 10 rows', 'Show 25 rows', 'Show 50 rows', 'Show all rows')),
        searchHighlight = TRUE
      )
    ),
    server = FALSE
  )

  geneMethylationSensitivityPlotData <- reactive({
    geneMethylationSensitivity() %>% transmute(
      x = slope,
      y = -log10(fdr),
      z = abs(slope),
      series = ifelse(p.value <= 0.05 & range.auc >= 0.25, "filtered", "not filtered"),
      id = Drug,
      tooltip = paste(
        "Drug: ", Drug,
        "<br>p-value: ", p.value, ", FDR: ", fdr,
        "<br>Slope: ", slope,
        #         "<br>Range AUC: ", range.auc,
        #         "<br>Range methylation: ", range.methylation,
        "<br>N: ", n,
        sep = ""
      )
    )
  })

  output$geneMethylationSensitivityVolcanoPlot <- renderHighchart({
    bubblePlot(
      geneMethylationSensitivityPlotData(),
      # title = ifelse(is.null(selection$selectedGene), "", selection$selectedGene),
      subtitle = ifelse(nrow(geneMethylationSensitivityPlotData()) == 0,
                        ifelse(is.null(selection$selectedGene),
                               "Select a gene to view methylation sensitivity analysis results",
                               "No data available"),
                        "Linear model fit for methylation and drug sensitivity (AUC)"),
      xLabel = "Effect (-Resistance/+Sensitivity)",
      yLabel = "-log10(FDR)",
      yLine = 1.0,
      series = c("filtered", "not filtered"),
      colours = c("#7cb5ec", "#434348"),
      clicked = "geneMethylationSensitivityVolcanoPlot_clicked"
    )
  })

  observe({
    clicked <- input$geneMethylationSensitivityVolcanoPlot_clicked
    if (!is.null(clicked))
    {
      selection$selectedDrug <- clicked
      updateTabsetPanel(session, "mainTabsetPanel", selected = "Drugs")
    }
  })


  # statistics

  statisticsTableData <- reactive({
    data_frame(type = "Models", count = nrow(modelSelection)) %>%
      bind_rows(data_frame(type = "Models with drug screening data", count = drugSensitivity %>% select(Model) %>% distinct %>% nrow)) %>%
      bind_rows(data_frame(type = "Models with mutation data", count = mutations %>% select(Model) %>% distinct %>% collect %>% nrow)) %>%
      bind_rows(data_frame(type = "Models with copy number data", count = copyNumberStates %>% select(Model) %>% distinct %>% collect %>% nrow)) %>%
      bind_rows(data_frame(type = "Models with expression data", count = expression %>% select(Model) %>% distinct %>% collect %>% nrow)) %>%
      bind_rows(data_frame(type = "Models with methylation data", count = methylation %>% select(Model) %>% distinct %>% collect %>% nrow)) %>%
      bind_rows(data_frame(type = "Number of drugs tested", count = drugSensitivity %>% select(Drug) %>% distinct %>% nrow))
  })

  output$statisticsTable <- renderTable(
    statisticsTableData(),
    include.rownames = FALSE,
    include.colnames = FALSE
  )


  # navigation events

  observeEvent(input$navigateToContactUs, {
    updateTabsetPanel(session, "mainTabsetPanel", selected = "Help")
    updateTabsetPanel(session, "helpTabsetPanel", selected = "Contact us")
  })

  observeEvent(input$navigateToBasicConcepts1, {
    updateTabsetPanel(session, "mainTabsetPanel", selected = "Help")
    updateTabsetPanel(session, "helpTabsetPanel", selected = "Basic concepts")
  })

  observeEvent(input$navigateToBasicConcepts2, {
    updateTabsetPanel(session, "mainTabsetPanel", selected = "Help")
    updateTabsetPanel(session, "helpTabsetPanel", selected = "Basic concepts")
  })

  observeEvent(input$navigateToBasicConcepts3, {
    updateTabsetPanel(session, "mainTabsetPanel", selected = "Help")
    updateTabsetPanel(session, "helpTabsetPanel", selected = "Basic concepts")
  })

  observeEvent(input$navigateToBasicConcepts4, {
    updateTabsetPanel(session, "mainTabsetPanel", selected = "Help")
    updateTabsetPanel(session, "helpTabsetPanel", selected = "Basic concepts")
  })

  observeEvent(input$navigateToBasicConcepts5, {
    updateTabsetPanel(session, "mainTabsetPanel", selected = "Help")
    updateTabsetPanel(session, "helpTabsetPanel", selected = "Basic concepts")
  })

  observeEvent(input$navigateToBasicConcepts6, {
    updateTabsetPanel(session, "mainTabsetPanel", selected = "Help")
    updateTabsetPanel(session, "helpTabsetPanel", selected = "Basic concepts")
  })

  observeEvent(input$navigateToBasicConcepts7, {
    updateTabsetPanel(session, "mainTabsetPanel", selected = "Help")
    updateTabsetPanel(session, "helpTabsetPanel", selected = "Basic concepts")
  })

  observeEvent(input$navigateToBasicConcepts8, {
    updateTabsetPanel(session, "mainTabsetPanel", selected = "Help")
    updateTabsetPanel(session, "helpTabsetPanel", selected = "Basic concepts")
  })

  observeEvent(input$navigateToBasicConcepts9, {
    updateTabsetPanel(session, "mainTabsetPanel", selected = "Help")
    updateTabsetPanel(session, "helpTabsetPanel", selected = "Basic concepts")
  })

  observeEvent(input$navigateToBasicConcepts10, {
    updateTabsetPanel(session, "mainTabsetPanel", selected = "Help")
    updateTabsetPanel(session, "helpTabsetPanel", selected = "Basic concepts")
  })

  observeEvent(input$navigateToBasicConcepts11, {
    updateTabsetPanel(session, "mainTabsetPanel", selected = "Help")
    updateTabsetPanel(session, "helpTabsetPanel", selected = "Basic concepts")
  })

  observeEvent(input$navigateToBasicConcepts12, {
    updateTabsetPanel(session, "mainTabsetPanel", selected = "Help")
    updateTabsetPanel(session, "helpTabsetPanel", selected = "Basic concepts")
  })

  observeEvent(input$navigateToBasicConcepts13, {
    updateTabsetPanel(session, "mainTabsetPanel", selected = "Help")
    updateTabsetPanel(session, "helpTabsetPanel", selected = "Basic concepts")
  })

  observeEvent(input$navigateToBasicConcepts14, {
    updateTabsetPanel(session, "mainTabsetPanel", selected = "Help")
    updateTabsetPanel(session, "helpTabsetPanel", selected = "Basic concepts")
  })

  observeEvent(input$navigateToBasicConcepts15, {
    updateTabsetPanel(session, "mainTabsetPanel", selected = "Help")
    updateTabsetPanel(session, "helpTabsetPanel", selected = "Basic concepts")
  })

  observeEvent(input$navigateToBasicConcepts16, {
    updateTabsetPanel(session, "mainTabsetPanel", selected = "Help")
    updateTabsetPanel(session, "helpTabsetPanel", selected = "Basic concepts")
  })
}

