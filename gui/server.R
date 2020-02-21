library(shiny)
library(shinydashboard)
library(tidyverse)
library(DT)
source(file = "../gui/pguDelegate.R", local=TRUE)

shinyServer(function(input, output, session) {
  delegate <- pgu.delegate$new()
  
  shiny::observeEvent(input$fi.import,{
    delegate$queryExcel(input, output, session)
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
  })
  
  shiny::observeEvent(input$ab.filterSet,{
    delegate$updateFilter(input, output, session)
    delegate$filterData(input, output, session)
    delegate$updateFilterStatisticsTbl(input, output, session)
    delegate$updateFilterMissingsTbl(input, output, session)
  })
  
  shiny::observeEvent(input$ab.filterInvSet,{
    delegate$updateFilterInverse(input, output, session)
    delegate$filterData(input, output, session)
    delegate$updateFilterStatisticsTbl(input, output, session)
    delegate$updateFilterMissingsTbl(input, output, session)
  })
  
  shiny::observeEvent(input$ab.filterReset,{
    delegate$resetFilter(input, output, session)
    delegate$filterData(input, output, session)
    delegate$updateFilterStatisticsTbl(input, output, session)
    delegate$updateFilterMissingsTbl(input, output, session)
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
  })
  
  shiny::observeEvent(input$ab.wizardReset, {
    delegate$updateModelDetectGui(input, output, session)
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
  })
  
  shiny::observeEvent(input$si.trafoMutateFeature, {
    delegate$updateTrafoMutateFeatureGraphic(input, output, session)
    delegate$updateTrafoMutateFeatureParameterTbl(input, output, session)
    delegate$updateTrafoMutateFeatureQualityTbl(input, output, session)
  })
  
  shiny::observeEvent(input$ab.trafoMutateReset, {
    delegate$resetTrafoMutateGui(input, output, session)
  })
}
)