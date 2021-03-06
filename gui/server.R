library(shiny)
library(shinyjs)
library(shinydashboard)
library(tidyverse)
library(DT)
source(file = "../gui/pguDelegate.R", local=TRUE)

shinyServer(function(input, output, session) {
  delegate <- pgu.delegate$new()
  analysisFinished <- shiny::reactiveVal(FALSE)
  
  shiny::observeEvent(input$fi.import,{
    delegate$queryExcel(input, output, session)
    delegate$hideOutdatedResults(input, output, session)
    analysisFinished(FALSE)
  })
  
  shiny::observeEvent(input$ab.import,{
    delegate$importData(input, output, session)
    delegate$updateRawDataInfo(input, output, session)
    delegate$importLoq(input, output, session)
    delegate$updateLoqInfo(input, output, session)
    delegate$importMetadata(input, output, session)
    delegate$updateMetadataInfo(input, output, session)
    delegate$updateFilterTbl(input, output, session)
    delegate$resetFilter(input, output, session)
    delegate$filterData(input, output, session)
    delegate$updateFilterStatisticsTbl(input, output, session)
    delegate$updateFilterMissingsTbl(input, output, session)
    delegate$updateLoqDetectGui(input, output, session)
    delegate$hideOutdatedResults(input, output, session)
    analysisFinished(FALSE)
  })
  
  shiny::observeEvent(input$ab.filterSet,{
    delegate$updateFilter(input, output, session)
    delegate$filterData(input, output, session)
    delegate$updateFilterStatisticsTbl(input, output, session)
    delegate$updateFilterMissingsTbl(input, output, session)
    delegate$hideOutdatedResults(input, output, session)
    analysisFinished(FALSE)
  })
  
  shiny::observeEvent(input$ab.filterInvSet,{
    delegate$updateFilterInverse(input, output, session)
    delegate$filterData(input, output, session)
    delegate$updateFilterStatisticsTbl(input, output, session)
    delegate$updateFilterMissingsTbl(input, output, session)
    delegate$hideOutdatedResults(input, output, session)
    analysisFinished(FALSE)
  })
  
  shiny::observeEvent(input$ab.filterReset,{
    delegate$resetFilter(input, output, session)
    delegate$filterData(input, output, session)
    delegate$updateFilterStatisticsTbl(input, output, session)
    delegate$updateFilterMissingsTbl(input, output, session)
    delegate$hideOutdatedResults(input, output, session)
    analysisFinished(FALSE)
  })
  
  shiny::observeEvent(input$si.exploreAbs, {
    delegate$updateExplorationAbscissa(input, output, session)
    delegate$updateExplorationGraphic(input, output, session)
    delegate$updateExplorationAbscissaGraphic(input, output, session)
    delegate$updateExplorationAbscissaTable(input, output, session)
  })
  
  shiny::observeEvent(input$si.exploreOrd, {
    delegate$updateExplorationOrdinate(input, output, session)
    delegate$updateExplorationGraphic(input, output, session)
    delegate$updateExplorationOrdinateGraphic(input, output, session)
    delegate$updateExplorationOrdinateTable(input, output, session)
  })
  
  shiny::observeEvent(input$ab.detectLoq, {
    delegate$detectLoq(input, output, session)
    delegate$updateLoqDetectStatisticsGraphic(input, output, session)
    delegate$updateLoqDetectFeatureGraphic(input, output, session)
    delegate$updateLoqDetectFeatureTbl(input, output, session)
    delegate$updateLoqDetectStatisticsTbl(input, output, session)
    delegate$updateLoqDetectOutlierTbl(input, output, session)
    delegate$updateLoqDetectDataTbl(input, output, session)
    delegate$updateLoqMutateGui(input, output, session)
    delegate$hideOutdatedResults(input, output, session)
    analysisFinished(FALSE)
  })
  
  shiny::observeEvent(input$si.loqDetectFeature, {
    delegate$updateLoqDetectFeatureGraphic(input, output, session)
    delegate$updateLoqDetectFeatureTbl(input, output, session)
  })
  
  shiny::observeEvent(input$ab.resetDetectLoq, {
    delegate$updateLoqDetectGui(input, output, session)
  })
  
  shiny::observeEvent(input$ab.mutateLoq, {
    delegate$mutateLoq(input, output, session)
    delegate$updateLoqMutateStatisticsGraphic(input, output, session)
    delegate$updateLoqMutateFeatureGraphic(input, output, session)
    delegate$updateLoqMutateFeatureTbl(input, output, session)
    delegate$updateLoqMutateStatisticsTbl(input, output, session)
    delegate$updateLoqMutateOutlierTbl(input, output, session)
    delegate$updateLoqMutateDataTbl(input, output, session)
    delegate$updateTrafoMutateGui(input, output, session)
    delegate$hideOutdatedResults(input, output, session)
    analysisFinished(FALSE)
  })
  
  shiny::observeEvent(input$si.loqMutateFeature, {
    delegate$updateLoqMutateFeatureGraphic(input, output, session)
    delegate$updateLoqMutateFeatureTbl(input, output, session)
  })
  
  shiny::observeEvent(input$ab.resetMutateLoq, {
    delegate$updateLoqMutateGui(input, output, session)
  })
  
  shiny::observeEvent(input$ab.wizardOptimize, {
    delegate$optimizeTrafoParameter(input, output, session)
    delegate$updateDetectedTrafoTypes(input, output, session)
    delegate$updateDetectedTrafoParameter(input, output, session)
    delegate$hideOutdatedResults(input, output, session)
    analysisFinished(FALSE)
  })
  
  shiny::observeEvent(input$ab.wizardReset, {
    delegate$updateTrafoDetectGui(input, output, session)
  })
  
  shiny::observeEvent(input$ab.trafoMutateSetGlobal, {
    delegate$trafoMutateGlobal(input, output, session)
    delegate$updateTrafoMutateFeatureGraphic(input, output, session)
    delegate$updateTrafoMutateFeatureParameterTbl(input, output, session)
    delegate$updateTrafoMutateFeatureQualityTbl(input, output, session)
    delegate$updtateTrafoMutateGlobalParameterTbl(input, output, session)
    delegate$updateTrafoMutateGlobalModelTbl(input, output, session)
    delegate$updateTrafoMutateGlobalQualityTbl(input, output, session)
    delegate$updateTrafoMutateGlobalTestsTbl(input, output, session)
    delegate$updateTrafoMutateGlobalDataTbl(input, output, session)
    delegate$hideOutdatedResults(input, output, session)
    analysisFinished(FALSE)
  })
  
  shiny::observeEvent(input$ab.trafoMutateFeature, {
    delegate$trafoMutateFeature(input, output, session)
    delegate$updateTrafoMutateFeatureGraphic(input, output, session)
    delegate$updateTrafoMutateFeatureParameterTbl(input, output, session)
    delegate$updateTrafoMutateFeatureQualityTbl(input, output, session)
    delegate$updtateTrafoMutateGlobalParameterTbl(input, output, session)
    delegate$updateTrafoMutateGlobalModelTbl(input, output, session)
    delegate$updateTrafoMutateGlobalQualityTbl(input, output, session)
    delegate$updateTrafoMutateGlobalTestsTbl(input, output, session)
    delegate$updateTrafoMutateGlobalDataTbl(input, output, session)
    delegate$hideOutdatedResults(input, output, session)
    analysisFinished(FALSE)
  })
  
  shiny::observeEvent(input$si.trafoMutateFeature, {
    delegate$updateTrafoMutateFeatureGraphic(input, output, session)
    delegate$updateTrafoMutateFeatureParameterTbl(input, output, session)
    delegate$updateTrafoMutateFeatureQualityTbl(input, output, session)
  })
  
  shiny::observeEvent(input$ab.trafoMutateReset, {
    delegate$resetTrafoMutateGui(input, output, session)
  })
  
  shiny::observeEvent(input$ab.imputeDetect, {
    delegate$imputeDetect(input, output, session)
    delegate$updateImputeDetectGraphic(input, output, session)
    delegate$updateImputeDetectStatisticsTbl(input, output, session)
    delegate$updateImputeDetectDetailTbl(input, output, session)
    delegate$updateImputeDetectDataTbl(input, output, session)
    delegate$updateImputeMutateGui(input, output, session)
    delegate$hideOutdatedResults(input, output, session)
    analysisFinished(FALSE)
  })
  
  shiny::observeEvent(input$ab.imputeMutateReset, {
    delegate$updateImputeMutateGui(input, output, session)
  })
  
  shiny::observeEvent(input$ab.imputeMutate, {
    delegate$imputeMutate(input, output, session)
    delegate$updateImputeMutateSeed(input, output, session)
    delegate$updateImputeMutateFeatureDetailGraphic(input, output, session)
    delegate$updateImputeMutateFeatureDetailTbl(input, output, session)
    delegate$updateImputeMutateDetailTbl(input, output, session)
    delegate$updateImputeMutateDataTbl(input, output, session)
    delegate$hideOutdatedResults(input, output, session)
    analysisFinished(FALSE)
  })

  shiny::observeEvent(input$si.imputeMutateFeature, {
    delegate$updateImputeMutateFeatureDetailGraphic(input, output, session)
    delegate$updateImputeMutateFeatureDetailTbl(input, output, session)
  })
  
  shiny::observeEvent(input$ab.outliersDetect, {
    delegate$outliersDetect(input, output, session)
    delegate$updateOutliersDetectGraphic(inpute, output, session)
    delegate$updateOutliersDetectStatisticsTbl(input, output, session)
    delegate$updateOutliersDetectDetailTbl(input, output, session)
    delegate$updateOutliersDetectDataTbl(input, output, session)
    delegate$updateOutliersMutateGui(input, output, session)
    delegate$hideOutdatedResults(input, output, session)
    analysisFinished(FALSE)
  })
  
  shiny::observeEvent(input$ab.outliersMutate, {
    delegate$outliersMutate(input, output, session)
    delegate$updateOutliersMutateSeed(input, output, session)
    delegate$updateOutliersMutateFeatureDetailGraphic(input, output, session)
    delegate$updateOutliersMutateFeatureDetailTbl(input, output, session)
    delegate$updateOutliersMutateDetailTbl(input, output, session)
    delegate$updateOutliersMutateDataTbl(input, output, session)
    delegate$hideOutdatedResults(input, output, session)
    analysisFinished(TRUE)
  })
  
  shiny::observeEvent(input$ab.outliersMutateReset, {
    delegate$updateOutliersMutateGui(input, output, session)
  })
  
  shiny::observeEvent(input$ab.correlation, {
    delegate$correlate(input, output, session)
    delegate$updateCorrelationMatrixPPearsonTbl(input, output, session)
    delegate$updateCorrelationMatrixRTbl(input, output, session)
    delegate$updateCorrelationMatrixPKendallTbl(input, output, session)
    delegate$updateCorrelationMatrixTauTbl(input, output, session)
    delegate$updateCorrelationMatrixPSpearmanTbl(input, output, session)
    delegate$updateCorrelationMatrixRhoTbl(input, output, session)
    delegate$hideOutdatedResults(input, output, session)
  })
  
  shiny::observeEvent(input$ab.regression, {
    delegate$regression(input, output, session)
    delegate$updateRegressionGui(input, output, session)
    delegate$updateRegressionGraphic(input, output, session)
    delegate$updateRegressionModelTbl(input, output, session)
    delegate$updateRegressionInterceptTbl(input, output, session)
    delegate$updateRegressionPInterceptTbl(input, output, session)
    delegate$updateRegressionSlopeTbl(input, output, session)
    delegate$updateRegressionPSlopeTbl(input, output, session)
    delegate$hideOutdatedResults(input, output, session)
  })
  
  shiny::observeEvent(input$si.regressionAbs, {
    delegate$updateRegressionOrdinate(input, output, session)
    delegate$updateRegressionGraphic(input, output, session)
    delegate$updateRegressionModelTbl(input, output, session)
  })

  shiny::observeEvent(input$si.regressionOrd, {
    delegate$updateRegressionGraphic(input, output, session)
    delegate$updateRegressionModelTbl(input, output, session)
  })
  
  shiny::observe({
    if (analysisFinished()) {
      shinyjs::enable("dbExport")
    }
    else {
      shinyjs::disable("dbExport")
    }
  })
  
  output$dbExport <- shiny::downloadHandler(
    filename = function(){
      delegate$exportFileName(input, output, session)
    },
    content = function(file){
      delegate$exportData(file)
    }
  )
  
  output$helpMenu <- shinydashboard::renderMenu({
    shinydashboard::dropdownMenu(type = "notifications",
                                 icon = icon("question-circle"),
                                 badgeStatus = NULL,
                                 headerText = "See also:",
                                 tags$li(
                                   a(href = "test.html",
                                     target = "_blank",
                                     tagAppendAttributes(icon("users"),
                                                         class = "text-info"),
                                     "google.com"
                                     )
                                 )
    )
  })
}
)



