##############
# LOQ Handling
##############
if ((input$menue) == "tab_loq"){
  if (dataLoaded()){
    updateSelectInput(session,
                      "si.loqSubstitute",
                      choices = c("NAN", "loq"),
                      selected = "NAN")
    updateSelectInput(session,
                      "si.loqNanHandling",
                      choices = c("keep", "lloq", "uloq"),
                      selected = "leep")
    updateSelectInput(session,
                      "si.loqHandleFeature",
                      choices = analysisData$numericFeatureNames,
                      selected = analysisData$numericFeatureNames[1])
  }
  else{
    errorMessage <- sprintf("No data loaded.", inFile$suffix)
    shiny::showNotification(paste(errorMessage),type = "error", duration = 10)
    output$tbl.loqStatistics <- DT::renderDataTable(NULL)
    output$plt.loqOutSummary <- shiny::renderPlot(NULL)
    output$tbl.loqOutSummary <- DT::renderDataTable(NULL)
    output$plt.loqOutCleaningSummary <- shiny::renderPlot(NULL)
    output$tbl.loqOutCleaningSummary <- DT::renderDataTable(NULL)
  }
}

shiny::observeEvent(input$ab.detectLoqOutliers,{
  if(dataLoaded() && loqLoaded()){
    loq$checkValidity(numericFeatureNames = filteredData$numericFeatureNames)
    print("Validity")
    print(loq$validity)
  }
})