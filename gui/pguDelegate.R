library("R6")
library("tidyverse")
library("shiny")
library("DT")
source(file = "../gui/pguStatus.R", local=TRUE)
source(file = "../R/pguFile.R", local=TRUE)
source(file = "../R/pguImporter.R", local=TRUE)
source(file = "../R/pguData.R", local=TRUE)
source(file = "../R/pguLimitsOfQuantification.R", local=TRUE)
source(file = "../R/pguFilter.R", local = TRUE)
source(file = "../R/pguExplorer.R", local = TRUE)
source(file = "../R/pguOptimizer.R", local=TRUE)
source(file = "../R/pguTransformator.R", local = TRUE)
source(file = "../R/pguModel.R", local=TRUE)
source(file = "../R/pguNormDist.R", local=TRUE)
source(file = "../R/pguImputation.R", local=TRUE)
source(file = "../R/pguOutliers.R", local = TRUE)
source(file = "../R/pguCorrelator.R", local=TRUE)
source(file = "../R/pguRegressor.R", local=TRUE)
source(file = "../R/pguExporter.R", local=TRUE)

pgu.delegate <- R6::R6Class("pgu.delegate",
                          ####################
                          # instance variables
                          ####################
                          private = list(
                            .status = "pgu.status",
                            .fileName = "pgu.file",
                            .importer = "pgu.importer",
                            .rawData = "pgu.data",
                            .loq = "pgu.limitsOfQuantification",
                            .metadata = "pgu.data",
                            .filterSet = "pgu.filter",
                            .filteredData = "pgu.data",
                            .filteredMetadata = "pgu.data",
                            .explorer= "pgu.explorer",
                            .loqMutatedData = "pgu.data",
                            .optimizer = "pgu.optimizer",
                            .transformator = "pgu.transformator",
                            .model = "pgu.model",
                            .transformedData = "pgu.data",
                            .featureModel = "pgu.normDist",
                            .imputer = "pgu.imputation",
                            .imputedData = "pgu.data",
                            .outliers = "pgu.outliers",
                            .revisedData = "pgu.data",
                            .cleanedData = "pgu.data",
                            .correlator = "pgu.correlator",
                            .regressor = "pgu.regressor",
                            .exporter = "pgu.exporter"
                          ),
                          ##################
                          # accessor methods
                          ##################
                          active = list(
                            status = function(){
                              return(private$.status)
                            },
                            fileName = function(){
                              return(private$.fileName)
                            },
                            importer = function(){
                              return(private$.importer)
                            },
                            rawData = function(){
                              return(private$.rawData)
                            },
                            loq = function(){
                              return(private$.loq)
                            },
                            metadata = function(){
                              return(private$.metadata)
                            },
                            filterSet = function(){
                              return(private$.filterSet)
                            },
                            filteredData = function(){
                              return(private$.filteredData)
                            },
                            filteredMetadata = function(){
                              return(private$.filteredMetadata)
                            },
                            explorer = function(){
                              return(private$.explorer)
                            },
                            loqMutatedData = function(){
                              return(private$.loqMutatedData)
                            },
                            optimizer = function(){
                              return(private$.optimizer)
                            },
                            transformator = function(){
                              return(private$.transformator)
                            },
                            model = function(){
                              return(private$.model)
                            },
                            transformedData = function(){
                              return(private$.transformedData)
                            },
                            featureModel = function(){
                              return(private$.featureModel)
                            },
                            imputer = function(){
                              return(private$.imputer)
                            },
                            imputedData = function(){
                              return(private$.imputedData)
                            },
                            outliers = function(){
                              return(private$.outliers)
                            },
                            revisedData = function(){
                              return(private$.revisedData)
                            },
                            cleanedData = function(){
                              return(private$.cleanedData)
                            },
                            correlator = function(){
                              return(private$.correlator)
                            },
                            regressor = function(){
                              return(private$.regressor)
                            },
                            exporter = function(){
                              return(private$.exporter)
                            }
                          ),
                          ###################
                          # memory management
                          ###################
                          public = list(
                            initialize = function(data = "tbl_df") {
                              print("Instance of pgu.delegate allocated")
                              private$.status <- pgu.status$new()
                              private$.fileName <- pgu.file$new()
                              private$.importer <- pgu.importer$new()
                              private$.rawData <- pgu.data$new()
                              private$.loq <- pgu.limitsOfQuantification$new()
                              private$.metadata <- pgu.data$new()
                              private$.filterSet <- pgu.filter$new()
                              private$.filteredData <- pgu.data$new()
                              private$.filteredMetadata <- pgu.data$new()
                              private$.explorer <- pgu.explorer$new()
                              private$.loqMutatedData <- pgu.data$new()
                              private$.optimizer <- pgu.optimizer$new()
                              private$.transformator <- pgu.transformator$new()
                              private$.model <- pgu.model$new()
                              private$.transformedData <- pgu.data$new()
                              private$.featureModel <- pgu.normDist$new()
                              private$.imputer <- pgu.imputation$new()
                              private$.imputedData <- pgu.data$new()
                              private$.outliers <- pgu.outliers$new()
                              private$.revisedData <- pgu.data$new()
                              private$.cleanedData <- pgu.data$new()
                              private$.correlator <- pgu.correlator$new()
                              private$.regressor <- pgu.regressor$new()
                              private$.exporter <- pgu.exporter$new()
                            },
                            finalize = function() {
                              print("Instance of pgu.delegate removed from heap")
                            },
                            ##########################
                            # print instance variables
                            ##########################
                            print = function() {
                              sprintf("\npgu.delegate\n\n") %>%
                                cat()
                              print(self$status)
                              print(self$fileName)
                              print(self$importer)
                              print(self$rawData)
                              print(self$loq)
                              print(self$metadata)
                              print(self$filterSet)
                              print(self$filteredData)
                              print(self$filteredMetadata)
                              print(self$explorer)
                              print(self$loqMutatedData)
                              print(self$optimizer)
                              print(self$transformator)
                              print(self$model)
                              print(self$transformedData)
                              print(self$featureModel)
                              print(self$imputer)
                              print(self$imputedData)
                              print(self$outliers)
                              print(self$revisedData)
                              print(self$cleanedData)
                              print(self$correlator)
                              print(self$regressor)
                              print(self$exporter)
                              invisible(self)
                            }
                          )
)
####################
# import functions
####################
pgu.delegate$set("public", "queryExcel", function(input, output, session){
  if (length(input$fi.import$datapath) > 0){
    private$.fileName$setUploadFileName <- input$fi.import$datapath
    private$.fileName$setFileName <- input$fi.import$name
    private$.fileName$splitFileName()
    private$.status$update(processName = "dataUploaded", value = TRUE)
  }
  else{
    private$.status$update(processName = "dataUploaded", value = FALSE)
    shiny::showNotification(paste("Please select a valid file."),type = "error", duration = 10)
  }
  if((private$.fileName$suffix != "xls") && (private$.fileName$suffix != "xlsx")){
    private$.status$update(processName = "dataUploaded", value = FALSE)
    shiny::showNotification(paste("Please select a valid file of type '.xls' or '.xlsx'."),type = "error", duration = 10)
  }
})

pgu.delegate$set("public", "importData", function(input, output, session){
  if (private$.status$query(processName = "dataUploaded")){
    tryCatch({
      private$.rawData$setRawData <- private$.importer$importData(self$fileName)
      private$.status$update(processName = "dataImported", value = TRUE)
    },
    error = function(e) {
      private$.status$update(processName = "dataImported", value = FALSE)
      shiny::showNotification(paste(e),type = "error", duration = 10)
    }
    )
  }
  else{
    private$.status$update(processName = "dataUploaded", value = FALSE)
    shiny::showNotification(paste("No file uploaded to import. Please upload a valid file first."),type = "error", duration = 10)
  }
})

pgu.delegate$set("public", "importLoq", function(input, output, session){
  if (private$.status$query(processName = "dataImported")){
    tryCatch({
      private$.loq$setLoq <- private$.importer$importLoq(self$fileName)
      private$.status$update(processName = "loqImported", value = TRUE)
    },
    error = function(e) {
      private$.status$update(processName = "loqImported", value = FALSE)
      shiny::showNotification(paste(e),type = "error", duration = 10)
    }
    )
  }
  else{
    private$.status$update(processName = "loqImported", value = FALSE)
    shiny::showNotification(paste("No file uploaded to import. Please upload a valid file first."),type = "error", duration = 10)
  }
})

pgu.delegate$set("public", "importMetadata", function(input, output, session){
  if (private$.status$query(processName = "dataImported")){
    tryCatch({
      private$.metadata$setRawData <- private$.importer$importMetadata(self$fileName)
      private$.status$update(processName = "metadataImported", value = TRUE)
    },
    error = function(e) {
      private$.status$update(processName = "metadataImported", value = FALSE)
      shiny::showNotification(paste(e),type = "error", duration = 10)
    }
    )
  }
  else{
    private$.status$update(processName = "metadataImported", value = FALSE)
    shiny::showNotification(paste("No file uploaded to import. Please upload a valid file first."),type = "error", duration = 10)
  }
})

####################
# filter functions
####################
pgu.delegate$set("public", "updateFilter", function(input, output, session){
  if(private$.status$query(processName = "dataImported")){
    if (length(input$tbl.filter_rows_all) < 1) {
      private$.filterSet$resetRowIdx(data = self$rawData$rawData)
    }
    else {
      private$.filterSet$setRowIdx <- input$tbl.filter_rows_all
    }
    if (length(input$tbl.filter_columns_selected) < 1) {
      private$.filterSet$resetColIdx(data = self$rawData$rawData)
    }
    else{
      colSelection <- input$tbl.filter_columns_selected - ncol(self$metadata$rawData) + 1
      colSelection <- colSelection[colSelection > 0] + 1
      if(length(colSelection) < 1){
        private$.filterSet$resetColIdx(data = self$rawData$rawData)
      }
      else{
        colSelection <- colSelection %>%
          append(1) %>%
          unique() %>%
          sort
        private$.filterSet$setColIdx <- colSelection
      }
    }
  }
  else{
    shiny::showNotification(paste("No file imported. Please import a valid file first."),type = "error", duration = 10)
  }
})

pgu.delegate$set("public", "updateFilterInverse", function(input, output, session){
  if(private$.status$query(processName = "dataImported")){
    if (length(input$tbl.filter_rows_all) < 1) {
      private$.filterSet$resetRowIdx(data = self$rawData$rawData)
    }
    else {
      private$.filterSet$setRowIdx <- input$tbl.filter_rows_all
    }
    if (length(input$tbl.filter_columns_selected) < 1) {
      private$.filterSet$resetColIdx(data = self$rawData$rawData)
    }
    else{
      colSelection <- input$tbl.filter_columns_selected - ncol(self$metadata$rawData) + 1
      colSelection <- colSelection[colSelection > 0] + 1
      idx <- seq(1,ncol(self$rawData$rawData), 1)
      iverseColSelection <- idx[-c(colSelection)]
      if(length(iverseColSelection) < 1){
        private$.filterSet$resetColIdx(data = self$rawData$rawData)
      }
      else{
        iverseColSelection <- iverseColSelection %>%
          append(1) %>%
          unique() %>%
          sort()
        private$.filterSet$setColIdx <- iverseColSelection
      }
    }
  }
  else{
    shiny::showNotification(paste("No file imported. Please import a valid file first."),type = "error", duration = 10)
  }
})

pgu.delegate$set("public", "resetFilter", function(input, output, session){
  if(private$.status$query(processName = "dataImported")){
    private$.filterSet$resetRowIdx(data = self$rawData$rawData)
    private$.filterSet$resetColIdx(data = self$rawData$rawData)
  }
  else{
    shiny::showNotification(paste("No file imported. Please import a valid file first."),type = "error", duration = 10)
  }
})

pgu.delegate$set("public", "filterData", function(input, output, session){
  if(private$.status$query(processName = "dataImported")){
    private$.filteredData$setRawData <- self$rawData$rawData %>%
      self$filterSet$filter()
    private$.filteredMetadata$setRawData <- self$metadata$rawData %>%
      self$filterSet$filterRows()
    self$updateExplorationData(input, output, session)
    private$.status$update(processName = "dataFiltered", value = TRUE)
  }
  else{
    shiny::showNotification(paste("No file imported. Please import a valid file first."),type = "error", duration = 10)
  }
})


###################
# explore functions
###################
pgu.delegate$set("public", "updateExplorationData", function(input, output, session){
  if(private$.status$query(processName = "dataFiltered")){
    private$.explorer$reset(obj =self$filteredMetadata$rawData %>%
                              dplyr::right_join(self$filteredData$rawData, by = "Sample Name"),
                            abs = "Sample Name",
                            ord = "Sample Name"
    )
    featureNames <- self$explorer$rawData %>%
      colnames()
    updateSelectInput(session, "si.exploreAbs", choices = featureNames)
    updateSelectInput(session, "si.exploreAbs", selected = featureNames[1])
    updateSelectInput(session, "si.exploreOrd", choices = featureNames)
    updateSelectInput(session, "si.exploreOrd", selected = featureNames[1])
  }
})

pgu.delegate$set("public", "updateExplorationAbscissa", function(input, output, session){
  private$.explorer$setAbscissa <- input$si.exploreAbs
})

pgu.delegate$set("public", "updateExplorationOrdinate", function(input, output, session){
  private$.explorer$setOrdinate <- input$si.exploreOrd
})

pgu.delegate$set("public", "updateExplorationGraphic", function(input, output, session){
  if(private$.status$query(processName = "dataFiltered")){
    output$plt.exploreGraphic <- shiny::renderPlot(
      self$explorer$scatterPlot(),
      height = 400)
  }
  else{
    output$plt.exploreGraphic <- shiny::renderPlot(NULL)
  }
})

pgu.delegate$set("public", "updateExplorationAbscissaGraphic", function(input, output, session){
  if(private$.status$query(processName = "dataFiltered")){
    output$plt.exploreAbscissaGraphic <- shiny::renderPlot(
      self$explorer$abscissaPlot(),
      height = 400
    )
  }
  else{
    output$plt.exploreAbscissaGraphic <- shiny::renderPlot(NULL)
  }
})

pgu.delegate$set("public", "updateExplorationOrdinateGraphic", function(input, output, session){
  if(private$.status$query(processName = "dataFiltered")){
    output$plt.exploreOrdinateGraphic <- shiny::renderPlot(
      self$explorer$ordinatePlot(),
      height = 400
    )
  }
  else{
    output$plt.exploreOrdinateGraphic <- shiny::renderPlot(NULL)
  }
})

pgu.delegate$set("public", "updateExplorationAbscissaTable", function(input, output, session){
  if(private$.status$query(processName = "dataFiltered")){
    output$tbl.exploreAbscissaStatistics <- DT::renderDataTable({
      self$explorer$abscissaStatistic() %>%
        DT::datatable(
          extensions = "Buttons",
          options = list(
            scrollX = TRUE,
            # scrollY = '75vh',
            paging = FALSE,
            dom = "Blfrtip",
            buttons = list(list(
              extend = 'csv',
              filename = self$fileName$bluntFileName("rawData_type"),
              text = "Download"
            ))
          )
        )
    })
  }
  else{
    output$tbl.exploreAbscissaStatistics <- DT::renderDataTable(NULL)
  }
})
                 
pgu.delegate$set("public", "updateExplorationOrdinateTable", function(input, output, session){
  if(private$.status$query(processName = "dataFiltered")){
    output$tbl.exploreOrdinateStatistics <- DT::renderDataTable({
      self$explorer$ordinateStatistic() %>%
        DT::datatable(
          extensions = "Buttons",
          options = list(
            scrollX = TRUE,
            paging = FALSE,
            dom = "Blfrtip",
            buttons = list(list(
              extend = 'csv',
              filename = self$fileName$bluntFileName("rawData_type"),
              text = "Download"
            ))
          )
        )
    })
  }
  else{
    output$tbl.exploreOrdinateStatistics <- DT::renderDataTable(NULL)
  }
})

######################
# LOQ Detect functions
######################
pgu.delegate$set("public", "updateLoqDetectGui", function(input, output, session){
  if(private$.status$query(processName = "dataFiltered")){
    self$updateLoqNaHandling(input, output, session)
    shiny::updateSelectInput(session,
                             "si.loqDetectFeature",
                             choices = self$filteredData$numericFeatureNames,
                             selected = self$filteredData$numericFeatureNames[1])
    
  }
  else{
    shiny::showNotification(paste("No filtered data set. Please filter data set first."),type = "error", duration = 10)
  }
})

pgu.delegate$set("public", "updateLoqNaHandling", function(input, output, session){
  shiny::updateSelectInput(session,
                           "si.loqNaHandling",
                           choices = self$loq$naHandlingAlphabet,
                           selected = self$loq$naHandlingAgent)
})



pgu.delegate$set("public", "detectLoq", function(input, output, session){
  if(private$.status$query(processName = "dataFiltered")){
    private$.loq$setNaHandlingAgent <- input$si.loqNaHandling
    private$.loq$findOutliers(obj = self$filteredData$numericData())
    private$.loq$collectStatistics(obj = self$filteredData$numericData())
    private$.status$update(processName = "loqDetected", value = TRUE)
  }
  else{
    shiny::showNotification(paste("No filtered data set. Please filter data set first."),type = "error", duration = 10)
  }
})



pgu.delegate$set("public", "updateLoqDetectStatisticsTbl", function(input, output, session){
  if(private$.status$query(processName = "loqDetected")){
    output$tbl.loqDetectStatistics <- DT::renderDataTable(
      self$loq$loqStatistics %>%
        format.data.frame(scientific = FALSE, digits = 3) %>%
        DT::datatable(
          extensions = "Buttons",
          options = list(
            scrollX = TRUE,
            scrollY = '350px',
            paging = FALSE,
            dom = "Blfrtip",
            buttons = list(list(
              extend = 'csv',
              filename = self$fileName$bluntFileName("loqStatistics"),
              text = "Download"
            ))
          )
        )
    )
  }
  else{
    output$tbl.loqDetectStatistics <- DT::renderDataTable(NULL)
  }
})

pgu.delegate$set("public", "updateLoqDetectOutlierTbl", function(input, output, session){
  options(htmlwidgets.TOJSON_ARGS = list(na = 'string'))
  t <- NULL
  if(self$status$query(processName = "loqDetected")){
    dfData <- self$filteredMetadata$rawData %>%
      dplyr::right_join(self$filteredData$rawData, by = "Sample Name")
    dfOutlier <- self$loq$outliers
    idx <- dfOutlier[["measurement"]][!duplicated(dfOutlier[["measurement"]])]
    t <- dfData %>%
      dplyr::slice(idx) %>%
      dplyr::mutate_if(is.numeric, round, 3) %>%
      DT::datatable(
        options = list(
          scrollX = TRUE,
          scrollY = '350px',
          paging = FALSE,
          dom = "Blfrtip",
          buttons = list(list(
            extend = 'csv',
            filename = self$fileName$bluntFileName("loqOutlier"),
            text = "Download"
          ))
        )
      )
    for (featureName in self$filteredData$numericFeatureNames){
      featureOutlier <- dfOutlier %>%
        dplyr::filter(feature == featureName) %>%
        dplyr::mutate_if(is.numeric, round, 3)
      if (nrow(featureOutlier)>0){
        t <- DT::formatStyle(t,
                             featureName,
                             backgroundColor = styleEqual(dfData %>%
                                                            dplyr::select(!!featureName) %>%
                                                            dplyr::slice(featureOutlier[["measurement"]]) %>%
                                                            unlist() %>%
                                                            as.numeric() %>%
                                                            round(digits = 3),
                                                          featureOutlier[["color"]])

        )
      }
    }
  }
  output$tbl.loqDetectOutlier <- DT::renderDataTable(t)    
})

pgu.delegate$set("public", "updateLoqDetectDataTbl", function(input, output, session){
  options(htmlwidgets.TOJSON_ARGS = list(na = 'string'))
  t <- NULL
  if(self$status$query(processName = "loqDetected")){
    dfData <- self$filteredMetadata$rawData %>%
      dplyr::right_join(self$filteredData$rawData, by = "Sample Name")
    dfOutlier <- self$loq$outliers
    idx <- dfOutlier[["measurement"]][!duplicated(dfOutlier[["measurement"]])]
    t <- dfData %>%
      dplyr::mutate_if(is.numeric, round, 3) %>%
      DT::datatable(
        options = list(
          scrollX = TRUE,
          scrollY = '350px',
          paging = FALSE,
          dom = "Blfrtip",
          buttons = list(list(
            extend = 'csv',
            filename = self$fileName$bluntFileName("loqData"),
            text = "Download"
          ))
        )
      )
    for (featureName in self$filteredData$numericFeatureNames){
      featureOutlier <- dfOutlier %>%
        dplyr::filter(feature == featureName) %>%
        dplyr::mutate_if(is.numeric, round, 3)
      if (nrow(featureOutlier)>0){
        t <- DT::formatStyle(t,
                             featureName,
                             backgroundColor = styleEqual(dfData %>%
                                                            dplyr::select(!!featureName) %>%
                                                            dplyr::slice(featureOutlier[["measurement"]]) %>%
                                                            unlist() %>%
                                                            as.numeric() %>%
                                                            round(digits = 3),
                                                          featureOutlier[["color"]])
                             
        )
      }
    }
  }
  output$tbl.loqDetectData <- DT::renderDataTable(t)    
})

pgu.delegate$set("public", "updateLoqDetectStatisticsGraphic", function(input, output, session){
  if(self$status$query(processName = "loqDetected")){
    output$plt.loqDetectStatistics <- shiny::renderPlot(
      self$loq$plotLoqDistribution(),
      height = 400
    )
  }
  else{
    output$plt.loqDetectStatistics <- shiny::renderPlot(NULL)
  }
})

pgu.delegate$set("public", "updateLoqDetectFeatureGraphic", function(input, output, session){
  if(self$status$query(processName = "loqDetected")){
    output$plt.loqDetectFeature <- shiny::renderPlot(
      self$loq$featurePlot(obj = self$filteredData$rawData, feature = input$si.loqDetectFeature),
      height = 425
    )
  }
  else{
    output$plt.loqDetectFeature <- shiny::renderPlot(NULL)
  }
})

pgu.delegate$set("public", "updateLoqDetectFeatureTbl", function(input, output, session){
  options(htmlwidgets.TOJSON_ARGS = list(na = 'string'))
  t <- NULL
  if(private$.status$query(processName = "loqDetected")){
    feature <- input$si.loqDetectFeature
    dfData <- self$filteredMetadata$rawData %>%
      dplyr::right_join(self$filteredData$rawData %>%
                          dplyr::select(c("Sample Name", !!feature)),
                        by = "Sample Name")

    dfOutlier <- self$loq$featureOutlier(feature = feature)

    t <- dfData %>%
      DT::datatable(options = list(scrollX = TRUE,
                                   scrollY = '300px',
                                   paging = FALSE,
                                   dom = "Blfrtip",
                                   buttons = list(list(
                                     extend = 'csv',
                                     filename = self$fileName$bluntFileName("rawData_type"),
                                     text = "Download"
                                   ))
      ))
    if (nrow(dfOutlier) > 0){
      t <- DT::formatStyle(
        t,
        feature,
        backgroundColor = DT::styleEqual(dfData %>%
                                           dplyr::select(!!feature) %>%
                                           dplyr::slice(dfOutlier[["measurement"]]) %>%
                                           unlist() %>%
                                           round(digits = 3),
                                         dfOutlier[["color"]]))
    }
  }
  output$tbl.loqDetectFeature <- DT::renderDataTable(t)
})

######################
# LOQ mutate functions
######################
pgu.delegate$set("public", "updateLoqMutateGui", function(input, output, session){
  if(private$.status$query(processName = "loqDetected")){
    self$updateLloqSubstitute(input, output, session)
    self$updateUloqSubstitute(input, output, session)
    shiny::updateSelectInput(session,
                             "si.loqMutateFeature",
                             choices = self$filteredData$numericFeatureNames,
                             selected = self$filteredData$numericFeatureNames[1])
    
  }
  else{
    shiny::showNotification(paste("No loq outliers detected. Please screen for loq outliers first."),type = "error", duration = 10)
  }
})

pgu.delegate$set("public", "updateLloqSubstitute", function(input, output, session){
  shiny::updateSelectInput(session,
                           "si.lloqSubstitute",
                           choices = self$loq$lloqSubstituteAlphabet,
                           selected = self$loq$lloqSubstituteAgent)
})

pgu.delegate$set("public", "updateUloqSubstitute", function(input, output, session){
  shiny::updateSelectInput(session,
                           "si.uloqSubstitute",
                           choices = self$loq$uloqSubstituteAlphabet,
                           selected = self$loq$uloqSubstituteAgent)
})

pgu.delegate$set("public", "mutateLoq", function(input, output, session){
  if(private$.status$query(processName = "loqDetected")){
    private$.loq$setLloqSubstituteAgent <- input$si.lloqSubstitute
    private$.loq$setUloqSubstituteAgent <- input$si.uloqSubstitute
    name  <- as.name("Sample Name")
    private$.loqMutatedData$setRawData <- self$filteredData$numericData() %>%
      self$loq$mutateLoqOutliers() %>%
      tibble::add_column(!! name := self$filteredData$rawData %>%
                           dplyr::select(!!name) %>%
                           unlist() %>%
                           as.character()) %>%
      dplyr::select(c(!!name, self$filteredData$numericFeatureNames))
    private$.status$update(processName = "loqMutated", value = TRUE)
  }
  else{
    shiny::showNotification(paste("No loq outliers detected. Please screen for loq outliers first."),type = "error", duration = 10)
  }
})

pgu.delegate$set("public", "updateLoqMutateStatisticsTbl", function(input, output, session){
  if(private$.status$query(processName = "loqMutated")){
    output$tbl.loqMutateStatistics <- DT::renderDataTable(
      self$loq$loqStatistics %>%
        format.data.frame(scientific = FALSE, digits = 3) %>%
        DT::datatable(
          extensions = "Buttons",
          options = list(
            scrollX = TRUE,
            scrollY = '350px',
            paging = FALSE,
            dom = "Blfrtip",
            buttons = list(list(
              extend = 'csv',
              filename = self$fileName$bluntFileName("loqStatistics"),
              text = "Download"
            ))
          )
        )
    )
  }
  else{
    output$tbl.loqMutateStatistics <- DT::renderDataTable(NULL)
  }
})

pgu.delegate$set("public", "updateLoqMutateOutlierTbl", function(input, output, session){
  options(htmlwidgets.TOJSON_ARGS = list(na = 'string'))
  t <- NULL
  if(self$status$query(processName = "loqMutated")){
    dfData <- self$filteredMetadata$rawData %>%
      dplyr::right_join(self$loqMutatedData$rawData, by = "Sample Name")
    dfOutlier <- self$loq$outliers
    idx <- dfOutlier[["measurement"]][!duplicated(dfOutlier[["measurement"]])]
    t <- dfData %>%
      dplyr::slice(idx) %>%
      dplyr::mutate_if(is.numeric, round, 3) %>%
      DT::datatable(
        options = list(
          scrollX = TRUE,
          scrollY = '350px',
          paging = FALSE,
          dom = "Blfrtip",
          buttons = list(list(
            extend = 'csv',
            filename = self$fileName$bluntFileName("loqOutlier"),
            text = "Download"
          ))
        )
      )
    for (featureName in self$loqMutatedData$numericFeatureNames){
      featureOutlier <- dfOutlier %>%
        dplyr::filter(feature == featureName) %>%
        dplyr::mutate_if(is.numeric, round, 3)
      if (nrow(featureOutlier)>0){
        t <- DT::formatStyle(t,
                             featureName,
                             backgroundColor = styleEqual(dfData %>%
                                                            dplyr::select(!!featureName) %>%
                                                            dplyr::slice(featureOutlier[["measurement"]]) %>%
                                                            unlist() %>%
                                                            as.numeric() %>%
                                                            round(digits = 3),
                                                          featureOutlier[["color"]])

        )
      }
    }
  }
  output$tbl.loqMutateOutlier <- DT::renderDataTable(t)
})

pgu.delegate$set("public", "updateLoqMutateDataTbl", function(input, output, session){
  options(htmlwidgets.TOJSON_ARGS = list(na = 'string'))
  t <- NULL
  if(self$status$query(processName = "loqMutated")){
    dfData <- self$filteredMetadata$rawData %>%
      dplyr::right_join(self$loqMutatedData$rawData, by = "Sample Name")
    dfOutlier <- self$loq$outliers
    idx <- dfOutlier[["measurement"]][!duplicated(dfOutlier[["measurement"]])]
    t <- dfData %>%
      dplyr::mutate_if(is.numeric, round, 3) %>%
      DT::datatable(
        options = list(
          scrollX = TRUE,
          scrollY = '350px',
          paging = FALSE,
          dom = "Blfrtip",
          buttons = list(list(
            extend = 'csv',
            filename = self$fileName$bluntFileName("loqData"),
            text = "Download"
          ))
        )
      )
    for (featureName in self$loqMutatedData$numericFeatureNames){
      featureOutlier <- dfOutlier %>%
        dplyr::filter(feature == featureName) %>%
        dplyr::mutate_if(is.numeric, round, 3)
      if (nrow(featureOutlier)>0){
        t <- DT::formatStyle(t,
                             featureName,
                             backgroundColor = styleEqual(dfData %>%
                                                            dplyr::select(!!featureName) %>%
                                                            dplyr::slice(featureOutlier[["measurement"]]) %>%
                                                            unlist() %>%
                                                            as.numeric() %>%
                                                            round(digits = 3),
                                                          featureOutlier[["color"]])

        )
      }
    }
  }
  output$tbl.loqMutateData <- DT::renderDataTable(t)
})

pgu.delegate$set("public", "updateLoqMutateStatisticsGraphic", function(input, output, session){
  if(self$status$query(processName = "loqMutated")){
    output$plt.loqMutateStatistics <- shiny::renderPlot(
      self$loq$plotLoqDistribution(),
      height = 400
    )
  }
  else{
    output$plt.loqMutateStatistics <- shiny::renderPlot(NULL)
  }
})

pgu.delegate$set("public", "updateLoqMutateFeatureGraphic", function(input, output, session){
  if(self$status$query(processName = "loqMutated")){
    output$plt.loqMutateFeature <- shiny::renderPlot(
      self$loq$featurePlot(obj = self$loqMutatedData$rawData, feature = input$si.loqMutateFeature),
      height = 425
    )
  }
  else{
    output$plt.loqMutateFeature <- shiny::renderPlot(NULL)
  }
})

pgu.delegate$set("public", "updateLoqMutateFeatureTbl", function(input, output, session){
  options(htmlwidgets.TOJSON_ARGS = list(na = 'string'))
  t <- NULL
  if(self$status$query(processName = "loqMutated")){
    feature <- input$si.loqMutateFeature
    dfData <- self$filteredMetadata$rawData %>%
      dplyr::right_join(self$loqMutatedData$rawData %>%
                          dplyr::select(c("Sample Name", !!feature)),
                        by = "Sample Name")

    dfOutlier <- self$loq$featureOutlier(feature = feature)

    t <- dfData %>%
      DT::datatable(options = list(scrollX = TRUE,
                                   scrollY = '300px',
                                   paging = FALSE,
                                   dom = "Blfrtip",
                                   buttons = list(list(
                                     extend = 'csv',
                                     filename = self$fileName$bluntFileName("rawData_type"),
                                     text = "Download"
                                   ))
      ))
    if (nrow(dfOutlier) > 0){
      t <- DT::formatStyle(
        t,
        feature,
        backgroundColor = DT::styleEqual(dfData %>%
                                           dplyr::select(!!feature) %>%
                                           dplyr::slice(dfOutlier[["measurement"]]) %>%
                                           unlist() %>%
                                           round(digits = 3),
                                         dfOutlier[["color"]]))
    }
  }
  output$tbl.loqMutateFeature <- DT::renderDataTable(t)
})

###########################################
# Trafo Detect functions (Parameter Wizard)
###########################################
pgu.delegate$set("public", "optimizeTrafoParameter", function(input, output, session){
  if(self$status$query(processName = "loqMutated")){
    trafoAlphabet <- c("none")
    if(input$cb.wizardLog){
      trafoAlphabet <- c(trafoAlphabet, "log2", "logNorm", "log10")
    }
    if(input$cb.wizardRoot){
      trafoAlphabet <- c(trafoAlphabet, "squareRoot", "cubeRoot")
    }
    if(input$cb.wizardArcsine){
      trafoAlphabet <- c(trafoAlphabet, "arcsine")
    }
    if(input$cb.wizardInverse){
      trafoAlphabet <- c(trafoAlphabet, "inverse")
    }
    if(input$cb.wizardTLOP){
      trafoAlphabet <- c(trafoAlphabet, "tukeyLOP")
    }
    if(input$cb.wizardBoxCox){
      trafoAlphabet <- c(trafoAlphabet, "boxCox")
    }
    # data in obj ändern
    private$.optimizer$resetOptimizer(data = self$loqMutatedData$numericData())
    private$.optimizer$setTrafoAlphabet <- trafoAlphabet
    private$.optimizer$setMirror <- input$cb.wizardMirror
    progress <- shiny::Progress$new(session, min = 1, max = length(self$optimizer$trafoAlphabet)*2)
    progress$set(message = "optimizing transformation parameters ...", value = 1)
    private$.optimizer$optimize(data = self$loqMutatedData$numericData(), progress = progress)
    on.exit(progress$close())
    private$.status$update(processName = "modelOptimized", value = TRUE)
  }
  else{
    shiny::showNotification(paste("No loq analysis perfomred. Please mutate loq outliers first."),type = "error", duration = 10)
  }
})

pgu.delegate$set("public", "updateDetectedTrafoTypes", function(input, output, session){
  if(self$status$query(processName = "modelOptimized")){
    output$tbl.trafoDetectTypes <- DT::renderDataTable(
      self$optimizer$optTypes %>%
        DT::datatable(
          extensions = "Buttons",
          options = list(
            scrollX = TRUE,
            scrollY = '350px',
            paging = FALSE,
            dom = "Blfrtip",
            buttons = list(list(
              extend = 'csv',
              filename = self$fileName$bluntFileName("optTrafoTypes"),
              text = "Download"
            ))
          ))
    )
  }
  else{
    output$tbl.trafoDetectTypes <- DT::renderDataTable(NULL)
  }
})

pgu.delegate$set("public", "updateDetectedTrafoParameter", function(input, output, session){
  if(self$status$query(processName = "modelOptimized")){
    output$tbl.trafoDetectParameters <- DT::renderDataTable(
      self$optimizer$optParameter %>%
        format.data.frame(scientific = TRUE, digits = 4) %>%
        DT::datatable(
          extensions = "Buttons",
          options = list(
            scrollX = TRUE,
            scrollY = '350px',
            paging = FALSE,
            dom = "Blfrtip",
            buttons = list(list(
              extend = 'csv',
              filename = self$fileName$bluntFileName("optTrafoParameter"),
              text = "Download"
            ))
          ))
    )
  }
  else{
    output$tbl.trafoDetectParameters <- DT::renderDataTable(NULL)
  }
})

pgu.delegate$set("public", "updateTrafoDetectGui", function(input, output, session){
  if(self$status$query(processName = "loqMutated")){
    trafoAlphabet <- self$optimizer$trafoAlphabet
    shiny::updateCheckboxInput(session,
                               "cb.wizardLog",
                               value = any(grepl(pattern = "log",
                                                 x = trafoAlphabet)))
    shiny::updateCheckboxInput(session,
                               "cb.wizardRoot",
                               value = any(grepl(pattern = "Root",
                                                 x = trafoAlphabet)))
    shiny::updateCheckboxInput(session,
                               "cb.wizardArcsine",
                               value = any(grepl(pattern = "arcsine",
                                                 x = trafoAlphabet)))
    shiny::updateCheckboxInput(session,
                               "cb.wizardInverse",
                               value = any(grepl(pattern = "inverse",
                                                 x = trafoAlphabet)))
    shiny::updateCheckboxInput(session,
                               "cb.wizardTLOP",
                               value = any(grepl(pattern = "tukeyLOP",
                                                 x = trafoAlphabet)))
    shiny::updateCheckboxInput(session,
                               "cb.wizardBoxCox",
                               value = any(grepl(pattern = "boxCox",
                                                 x = trafoAlphabet)))
    shiny::updateCheckboxInput(session,
                               "cb.wizardMirror",
                               value = self$optimizer$mirror)
  }
  else{
    shiny::showNotification(paste("No loq analysis perfomred. Please mutate loq outliers first."),type = "error", duration = 10)
  }
})

############################################
# trafo mutate functions (model by gaussion)
############################################
pgu.delegate$set("public", "updateTrafoMutateFeature", function(input, output, session){
  if(self$status$query(processName = "loqMutated")){
    shiny::updateSelectInput(session,
                             inputId = "si.trafoMutateFeature",
                             choices = self$loqMutatedData$numericFeatureNames,
                             selected = self$loqMutatedData$numericFeatureNames[1])
  }
})

pgu.delegate$set("public", "updateTrafoMutateType", function(input, output, session){
  if(self$status$query(processName = "loqMutated")){
    shiny::updateSelectInput(session,
                             inputId = "si.trafoMutateType",
                             choices = self$transformator$trafoAlphabet,
                             selected = self$transformator$trafoType(feature = self$loqMutatedData$numericFeatureNames[1])
                             )
  }
})

pgu.delegate$set("public", "updateTrafoMutateMirror", function(input, output, session){
  if(self$status$query(processName = "loqMutated")){
    shiny::updateCheckboxInput(session,
                               inputId = "cb.trafoMutateMirror",
                               value = self$transformator$mirrorLogic(feature = self$loqMutatedData$numericFeatureNames[1])
    )
  }
})

pgu.delegate$set("public", "updateTrafoMutateGui", function(input, output, session){
  if(self$status$query(processName = "loqMutated")){
    self$updateTrafoMutateFeature(input, output, session)
    self$updateTrafoMutateType(input, output, session)
    self$updateTrafoMutateMirror(input, output, session)
  }
})

pgu.delegate$set("public", "resetTrafoMutateGui", function(input, output, session){
  if(self$status$query(processName = "loqMutated")){
    self$updateTrafoMutateType(input, output, session)
    self$updateTrafoMutateMirror(input, output, session)
  }
})

pgu.delegate$set("public", "trafoMutateGlobal", function(input, output, session){
  if(self$status$query(processName = "loqMutated")){
    progress <- shiny::Progress$new(session, min = 1, max = length(self$loqMutatedData$numericFeatureNames))
    progress$set(message = "Optimizing model parameter", value = 1)
    on.exit(progress$close())
    self$loqMutatedData$numericData() %>%
      private$.transformator$resetTrafoParameter()
    for (feature in self$loqMutatedData$numericFeatureNames){
      private$.transformator$setTrafoType(feature = feature,
                                          type = input$si.trafoMutateType)
      private$.transformator$setMirrorLogic(feature = feature,
                                            logic = input$cb.trafoMutateMirror)
    }
    
    self$loqMutatedData$numericData() %>%
      private$.transformator$estimateTrafoParameter()
    self$loqMutatedData$numericData() %>%
      private$.transformator$mutateData() %>%
      private$.model$resetModel(progress)
    name  <- as.name("Sample Name")
    private$.transformedData$setRawData <- self$loqMutatedData$numericData() %>%
      self$transformator$mutateData() %>%
      self$model$scaleData() %>%
      tibble::add_column(!! name := self$loqMutatedData$rawData %>%
                           dplyr::select(!!name) %>%
                           unlist() %>%
                           as.character()) %>%
      dplyr::select(c(!!name, self$loqMutatedData$numericFeatureNames))
    private$.status$update(processName = "modelDefined", value = TRUE)
  }
  else{
    shiny::showNotification(paste("No loq analysis perfomred. Please mutate loq outliers first."),type = "error", duration = 10)
  }
})

pgu.delegate$set("public", "trafoMutateFeature", function(input, output, session){
  if(self$status$query(processName = "modelDefined")){
    private$.transformator$setTrafoType(feature = input$si.trafoMutateFeature,
                                        type = input$si.trafoMutateType)
    private$.transformator$setMirrorLogic(feature = input$si.trafoMutateFeature,
                                          logic = input$cb.trafoMutateMirror)
    
    self$loqMutatedData$numericData() %>%
      private$.transformator$estimateTrafoParameter()
    
    self$loqMutatedData$numericData() %>%
      self$transformator$mutateData() %>%
      dplyr::select(input$si.trafoMutateFeature) %>%
      private$.featureModel$resetNormDist()
    
    tryCatch({
      private$.featureModel$fit()
    },
    error = function(e) {
      errorString <- sprintf("Error: could not optimize model parameters for  %s transformation of feature %s. Trafo type is reset to 'none'",
                             input$si.trafoMutateType,
                             input$si.trafoMutateFeature)
      shiny::showNotification(paste(errorString),type = "error", duration = 10)
      
      shiny::updateSelectInput(session,
                               inputId = "si.trafoMutateType",
                               choices = self$transformator$trafoAlphabet,
                               selected = "none")
      
      shiny::updateCheckboxInput(session,
                                 inputId = "cb.trafoMutateMirror",
                                 value = FALSE)
      
      private$.transformator$setTrafoType(feature = input$si.trafoMutateFeature,
                                          type = input$si.trafoMutateType)
      
      private$.transformator$setMirrorLogic(feature = input$si.trafoMutateFeature,
                                            logic = input$cb.trafoMutateMirror)
      
      self$loqMutatedData$numericData() %>%
        private$.transformator$estimateTrafoParameter()
      
      private$.featureModel$resetNormDist(data = self$loqMutatedData$numericData %>%
                                            self$transformator$mutateData() %>%
                                            dplyr::select(input$si.trafoMutateFeature)
      )
      
      private$.featureModel$fit()
    })
    
    private$.model$setNormDist(data = self$featureModel, feature = input$si.trafoMutateFeature)
    
    name  <- as.name("Sample Name")
    private$.transformedData$setRawData <- self$loqMutatedData$numericData() %>%
      self$transformator$mutateData() %>%
      self$model$scaleData() %>%
      tibble::add_column(!! name := self$loqMutatedData$rawData %>%
                           dplyr::select(!!name) %>%
                           unlist() %>%
                           as.character()) %>%
      dplyr::select(c(!!name, self$loqMutatedData$numericFeatureNames))
  }
  else{
    shiny::showNotification(paste("No global model defined. Please defina a global transformation model first."),type = "error", duration = 10)
  }
})

pgu.delegate$set("public", "updateTrafoMutateFeatureGraphic", function(input, output, session){
  if(self$status$query(processName = "modelDefined")){
    output$plt.trafoMutateFeature <- shiny::renderPlot(
      self$model$plotModel(feature = input$si.trafoMutateFeature)
    )
  }
  else{
    output$plt.trafoMutateFeature <- shiny::renderPlot(NULL)
  }
})

pgu.delegate$set("public", "updateTrafoMutateFeatureParameterTbl", function(input, output, session){
  if(self$status$query(processName = "modelDefined")){
    output$tbl.trafoMutateFeatureParameter <- DT::renderDataTable(
      self$model$fitResultFeature(feature = input$si.trafoMutateFeature) %>%
        transformTibble() %>%
        format.data.frame(scientific = TRUE, digits = 4) %>%
        DT::datatable(
          extensions = "Buttons",
          options = list(
            scrollX = TRUE,
            scrollY = '350px',
            paging = FALSE,
            dom = "Blfrtip",
            buttons = list(list(
              extend = 'csv',
              filename = self$fileName$bluntFileName("featureTrafoParameter"),
              text = "Download"
            ))
          ))
    )
  }
  else{
    output$tbl.trafoMutateFeatureParameter <- DT::renderDataTable(NULL)
  }
})

pgu.delegate$set("public", "updateTrafoMutateFeatureQualityTbl", function(input, output, session){
  if(self$status$query(processName = "modelDefined")){
    output$tbl.trafoMutateFeatureQuality <- DT::renderDataTable(
      self$model$testResultFeature(feature = input$si.trafoMutateFeature) %>%
        transformTibble() %>%
        format.data.frame(scientific = TRUE, digits = 4) %>%
        DT::datatable(
          extensions = "Buttons",
          options = list(
            scrollX = TRUE,
            scrollY = '350px',
            paging = FALSE,
            dom = "Blfrtip",
            buttons = list(list(
              extend = 'csv',
              filename = self$fileName$bluntFileName("featureTrafoQuality"),
              text = "Download"
            ))
          ))
    )
  }
  else{
    output$tbl.trafoMutateFeatureQuality <- DT::renderDataTable(NULL)
  }
})

pgu.delegate$set("public", "updtateTrafoMutateGlobalParameterTbl", function(input, output, session){
  if(self$status$query(processName = "modelDefined")){
    output$tbl.trafoMutateGlobalParameter <- DT::renderDataTable(
      self$transformator$trafoParameter %>%
        format.data.frame(scientific = TRUE, digits = 4) %>%
        DT::datatable(
          extensions = "Buttons",
          options = list(
            scrollX = TRUE,
            scrollY = '350px',
            paging = FALSE,
            dom = "Blfrtip",
            buttons = list(list(
              extend = 'csv',
              filename = self$fileName$bluntFileName("globalTrafoParameter"),
              text = "Download"
            ))
          ))
    )
  }
  else{
    output$tbl.trafoMutateGlobalParameter <- DT::renderDataTable(NULL)
  }
})

pgu.delegate$set("public", "updateTrafoMutateGlobalModelTbl", function(input, output, session){
  if(self$status$query(processName = "modelDefined")){
    output$tbl.trafoMutateGlobalModel <- DT::renderDataTable(
      self$model$modelParameterData() %>%
        format.data.frame(scientific = TRUE, digits = 4) %>%
        DT::datatable(
          extensions = "Buttons",
          options = list(
            scrollX = TRUE,
            scrollY = '350px',
            paging = FALSE,
            dom = "Blfrtip",
            buttons = list(list(
              extend = 'csv',
              filename = self$fileName$bluntFileName("globalModelParameter"),
              text = "Download"
            ))
          ))
    )
  }
  else{
    output$tbl.trafoMutateGlobalModel <- DT::renderDataTable(NULL)
  }
})

pgu.delegate$set("public", "updateTrafoMutateGlobalQualityTbl", function(input, output, session){
  if(self$status$query(processName = "modelDefined")){
    output$tbl.trafoMutateGlobalQuality <- DT::renderDataTable(
      self$model$modelQualityData() %>%
        format.data.frame(scientific = TRUE, digits = 4) %>%
        DT::datatable(
          extensions = "Buttons",
          options = list(
            scrollX = TRUE,
            scrollY = '350px',
            paging = FALSE,
            dom = "Blfrtip",
            buttons = list(list(
              extend = 'csv',
              filename = self$fileName$bluntFileName("globalModelQuality"),
              text = "Download"
            ))
          ))
    )
  }
  else{
    output$tbl.trafoMutateGlobalQuality <- DT::renderDataTable(NULL)
  }
})

pgu.delegate$set("public", "updateTrafoMutateGlobalTestsTbl", function(input, output, session){
  if(self$status$query(processName = "modelDefined")){
    output$tbl.trafoMutateGlobalTests <- DT::renderDataTable(
      self$model$testResultData() %>%
        format.data.frame(scientific = TRUE, digits = 4) %>%
        DT::datatable(
          extensions = "Buttons",
          options = list(
            scrollX = TRUE,
            scrollY = '350px',
            paging = FALSE,
            dom = "Blfrtip",
            buttons = list(list(
              extend = 'csv',
              filename = self$fileName$bluntFileName("globalModelQuality"),
              text = "Download"
            ))
          ))
    )
  }
  else{
    output$tbl.trafoMutateGlobalTests <- DT::renderDataTable(NULL)
  }
})

pgu.delegate$set("public", "updateTrafoMutateGlobalDataTbl", function(input, output, session){
    options(htmlwidgets.TOJSON_ARGS = list(na = 'string'))
    if(self$status$query(processName = "modelDefined")){
      output$tbl.trafoMutateGlobalData <- DT::renderDataTable(
        self$filteredMetadata$rawData %>%
          dplyr::right_join(self$transformedData$rawData, by = "Sample Name") %>%
          dplyr::mutate_if(is.numeric, round, 3) %>%
          DT::datatable(
            options = list(
              scrollX = TRUE,
              scrollY = '350px',
              paging = FALSE,
              dom = "Blfrtip",
              buttons = list(list(
                extend = 'csv',
                filename = self$fileName$bluntFileName("transformedData"),
                text = "Download"
              ))
            )
          )
      )
    }
  else{
    output$tbl.trafoMutateGlobalData <- DT::renderDataTable(NULL)
  }
})

#########################
# impute detect functions
#########################
pgu.delegate$set("public", "imputeDetect", function(input, output, session){
  if(self$status$query(processName = "modelDefined")){
    self$transformedData$numericData() %>%
      private$.imputer$resetImputationParameter()
    private$.status$update(processName = "naDetected", value = TRUE)
  }
  else{
    shiny::showNotification(paste("No global model defined. Please defina a global transformation model first."),type = "error", duration = 10)
  }
})

pgu.delegate$set("public", "updateImputeDetectGraphic", function(input, output, session){
  if(self$status$query(processName = "naDetected")){
    output$plt.imputeDetectSummary <- shiny::renderPlot(
      self$imputer$imputationSiteHeatMap()
    )
  }
  else{
    output$plt.imputeDetectSummary <- shiny::renderPlot(NULL)
  }
})

pgu.delegate$set("public", "updateImputeDetectStatisticsTbl", function(input, output, session){
  if(self$status$query(processName = "naDetected")){
    output$tbl.imputeDetectStatistics <- DT::renderDataTable(
      self$transformedData$numericData() %>%
        self$imputer$imputationSiteDistribution() %>%
        DT::datatable(
          extensions = "Buttons",
          options = list(
            scrollX = TRUE,
            scrollY = '350px',
            paging = FALSE,
            dom = "Blfrtip",
            buttons = list(list(
              extend = 'csv',
              filename = self$fileName$bluntFileName("imputationSiteDetectionStatistics"),
              text = "Download"
            ))
          ))
    )
  }
  else{
    output$tbl.imputeDetectStatistics <- DT::renderDataTable(NULL)
  }
})

pgu.delegate$set("public", "updateImputeDetectDetailTbl", function(input, output, session){
  if(self$status$query(processName = "naDetected")){
    output$tbl.imputeDetectDetail <- DT::renderDataTable(
      self$loqMutatedData$rawData %>%
        self$imputer$mergeImputationSiteData(dfMetadata = self$metadata$rawData) %>%
        format.data.frame(scientific = TRUE, digits = 4) %>%
        DT::datatable(
          extensions = "Buttons",
          options = list(
            scrollX = TRUE,
            scrollY = '350px',
            paging = FALSE,
            dom = "Blfrtip",
            buttons = list(list(
              extend = 'csv',
              filename = self$fileName$bluntFileName("imputationSiteDetectionDetail"),
              text = "Download"
            ))
          ))
    )
  }
  else{
    output$tbl.imputeDetectDetail <- DT::renderDataTable(NULL)
  }
})

pgu.delegate$set("public", "updateImputeDetectDataTbl", function(input, output, session){
  if(self$status$query(processName = "naDetected")){
    output$tbl.imputeDetectData <- DT::renderDataTable(
      self$filteredMetadata$rawData %>%
        dplyr::right_join(self$transformedData$rawData, by = "Sample Name") %>%
        format.data.frame(scientific = TRUE, digits = 4) %>%
        DT::datatable(
          extensions = "Buttons",
          options = list(
            scrollX = TRUE,
            scrollY = '350px',
            paging = FALSE,
            dom = "Blfrtip",
            buttons = list(list(
              extend = 'csv',
              filename = self$fileName$bluntFileName("imputationSiteDetectionData"),
              text = "Download"
            ))
          ))
    )
  }
  else{
    output$tbl.imputeDetectData <- DT::renderDataTable(NULL)
  }
})

#########################
# impute mutate functions
#########################
pgu.delegate$set("public", "updateImputeMutateFeature", function(input, output, session){
  if(self$status$query(processName = "naDetected")){
    shiny::updateSelectInput(session,
                             "si.imputeMutateFeature",
                             choices = self$imputer$imputationParameter[["features"]],
                             selected = self$imputer$imputationParameter[["features"]][1]
    )
  }
})

pgu.delegate$set("public", "updateImputeMutateMethod", function(input, output, session){
  if(self$status$query(processName = "naDetected")){
    shiny::updateSelectInput(session,
                             "si.imputeMutateMethod",
                             choices = self$imputer$imputationAgentAlphabet,
                             selected = self$imputer$imputationAgent
    )
  }
})

pgu.delegate$set("public", "updateImputeMutateSeed", function(input, output, session){
  if(self$status$query(processName = "naDetected")){
    shiny::updateNumericInput(session,
                              "ni.imputeMutateSeed",
                              value = self$imputer$seed)
  }
})

pgu.delegate$set("public", "updateImputeMutateIterations", function(input, output, session){
  if(self$status$query(processName = "naDetected")){
    shiny::updateNumericInput(session,
                              "ni.imputeMutateIterations",
                              value = self$imputer$iterations)
  }
})

pgu.delegate$set("public", "updateImputeMutateGui", function(input, output, session){
  if(self$status$query(processName = "naDetected")){
    self$updateImputeMutateMethod(input, output, session)
    self$updateImputeMutateSeed(input, output, session)
    self$updateImputeMutateIterations(input, output, session)
    self$updateImputeMutateFeature(input, output, session)
  }
})

pgu.delegate$set("public", "imputeMutate", function(input, output, session){
  if(self$status$query(processName = "naDetected")){
    private$.imputer$setImputationAgent <- input$si.imputeMutateMethod
    private$.imputer$setSeed <- input$ni.imputeMutateSeed
    private$.imputer$setIterations <- input$ni.imputeMutateIterations
    
    progress <- shiny::Progress$new(session, min = 1, max = length(self$transformedData$numericFeatureNames))
    progress$set(message = "Impute Missings", value = 0)
    on.exit(progress$close())
    
    name  <- as.name("Sample Name")
    private$.imputedData$setRawData <- self$transformedData$numericData() %>%
      self$imputer$handleImputationSites(progress = progress) %>%
      # self$model$rescaleData() %>%
      # self$transformator$reverseMutateData() %>%
      tibble::add_column(!! name := self$transformedData$rawData %>%
                           dplyr::select(!!name) %>%
                           unlist() %>%
                           as.character()) %>%
      dplyr::select(c(!!name, self$transformedData$numericFeatureNames))
    private$.status$update(processName = "naMutated", value = TRUE)
  }
  else{
    shiny::showNotification(paste("No Na's detected. Please detect missing first."),type = "error", duration = 10)
  }
})

pgu.delegate$set("public", "updateImputeMutateFeatureDetailGraphic", function(input, output, session){
  if(self$status$query(processName = "naMutated")){
    output$plt.imputeMutateFeatureDetail <- shiny::renderPlot(
      self$imputer$featurePlot(data = self$imputedData$numericData(),
                               feature = input$si.imputeMutateFeature)
    )
  }
  else{
    output$plt.imputeMutateFeatureDetail <- shiny::renderPlot(NULL)
  }
})

pgu.delegate$set("public", "updateImputeMutateFeatureDetailTbl", function(input, output, session){
  if (self$status$query(processName = "naMutated")){
    output$tbl.imputeMutateFeatureDetail <- DT::renderDataTable(
      self$filteredMetadata$rawData %>%
        dplyr::right_join(self$imputedData$rawData %>%
                            dplyr::select(c("Sample Name", input$si.imputeMutateFeature)),
                          by = "Sample Name") %>%
        dplyr::slice(self$imputer$imputationSiteIdxByFeature(feature = input$si.imputeMutateFeature)) %>%
        format.data.frame(scientific = TRUE, digits = 4) %>%
        DT::datatable(
          extensions = "Buttons",
          options = list(
            scrollX = TRUE,
            scrollY = '350px',
            paging = FALSE,
            dom = "Blfrtip",
            buttons = list(list(
              extend = 'csv',
              filename = self$fileName$bluntFileName("imputationSiteMutationDetails"),
              text = "Download"
            ))
          ))
    ) 
  }
  else{
    output$tbl.imputeMutateFeatureDetail <- DT::renderDataTable(NULL)
  }
})

pgu.delegate$set("public", "updateImputeMutateDetailTbl", function(input, output, session){
  if(self$status$query(processName = "naMutated")){
    dfImputationSites <- self$imputer$imputationSites
    idx <- dfImputationSites[["row"]][!duplicated(dfImputationSites[["row"]])]

    output$tbl.imputeMutateDetail <- DT::renderDataTable(
      self$filteredMetadata$rawData %>%
        dplyr::right_join(self$imputedData$rawData, by = "Sample Name") %>%
        dplyr::slice(idx) %>%
        format.data.frame(scientific = TRUE, digits = 4) %>%
        DT::datatable(
          extensions = "Buttons",
          options = list(
            scrollX = TRUE,
            scrollY = '350px',
            paging = FALSE,
            dom = "Blfrtip",
            buttons = list(list(
              extend = 'csv',
              filename = self$fileName$bluntFileName("imputationSiteMutationDetail"),
              text = "Download"
            ))
          ))
    )
  }
  else{
    output$tbl.imputeMutateDetail <- DT::renderDataTable(NULL)
  }
})

pgu.delegate$set("public", "updateImputeMutateDataTbl", function(input, output, session){
  if(self$status$query(processName = "naMutated")){
    output$tbl.imputeMutateData <- DT::renderDataTable(
      self$filteredMetadata$rawData %>%
        dplyr::right_join(self$imputedData$rawData, by = "Sample Name") %>%
        format.data.frame(scientific = TRUE, digits = 4) %>%
        DT::datatable(
          extensions = "Buttons",
          options = list(
            scrollX = TRUE,
            scrollY = '350px',
            paging = FALSE,
            dom = "Blfrtip",
            buttons = list(list(
              extend = 'csv',
              filename = self$fileName$bluntFileName("imputationSiteMutationData"),
              text = "Download"
            ))
          ))
    )
  }
  else{
    output$tbl.imputeMutateData <- DT::renderDataTable(NULL)
  }
})


##########################
# outlier detect functions
##########################
pgu.delegate$set("public", "outliersDetect", function(input, output, session){
  if(self$status$query(processName = "naMutated")){
    self$transformedData$numericData() %>%
      private$.outliers$resetOutliersParameter()
    private$.status$update(processName = "outliersDetected", value = TRUE)
  }
  else{
    shiny::showNotification(paste("No Na's imputed. Please run imputation of missings first."),type = "error", duration = 10)
  }
})

pgu.delegate$set("public", "updateOutliersDetectGraphic", function(input, output, session){
  if(self$status$query(processName = "outliersDetected")){
    output$plt.outliersDetectSummary <- shiny::renderPlot(
      self$outliers$plotOutliersDistribution()
    )
  }
  else{
    output$plt.outliersDetectSummary <- shiny::renderPlot(NULL)
  }
})

pgu.delegate$set("public", "updateOutliersDetectStatisticsTbl", function(input, output, session){
  if(self$status$query(processName = "outliersDetected")){
    output$tbl.outliersDetectStatistics <- DT::renderDataTable(
      self$outliers$outliersStatistics %>%
        DT::datatable(
          extensions = "Buttons",
          options = list(
            scrollX = TRUE,
            scrollY = '350px',
            paging = FALSE,
            dom = "Blfrtip",
            buttons = list(list(
              extend = 'csv',
              filename = self$fileName$bluntFileName("outliersDetectionStatistics"),
              text = "Download"
            ))
          ))
    )
  }
  else{
    output$tbl.outliersDetectStatistics <- DT::renderDataTable(NULL)
  }
})

pgu.delegate$set("public", "updateOutliersDetectDetailTbl", function(input, output, session){
  if(self$status$query(processName = "outliersDetected")){
    dfOutlier <- self$outliers$outliers
    idx <- dfOutlier[["measurement"]][!duplicated(dfOutlier[["measurement"]])]
    output$tbl.outliersDetectDetail <- DT::renderDataTable(
      self$filteredMetadata$rawData %>%
        dplyr::right_join(self$transformedData$rawData, by = "Sample Name") %>%
        dplyr::slice(idx) %>%
        format.data.frame(scientific = TRUE, digits = 4) %>%
        DT::datatable(
          extensions = "Buttons",
          options = list(
            scrollX = TRUE,
            scrollY = '350px',
            paging = FALSE,
            dom = "Blfrtip",
            buttons = list(list(
              extend = 'csv',
              filename = self$fileName$bluntFileName("outliersDetectionDetail"),
              text = "Download"
            ))
          ))
    )
  }
  else{
    output$tbl.outliersDetectDetail <- DT::renderDataTable(NULL)
  }
})

pgu.delegate$set("public", "updateOutliersDetectDataTbl", function(input, output, session){
  if(self$status$query(processName = "outliersDetected")){
    output$tbl.outliersDetectData <- DT::renderDataTable(
      self$filteredMetadata$rawData %>%
        dplyr::right_join(self$transformedData$rawData, by = "Sample Name") %>%
        format.data.frame(scientific = TRUE, digits = 4) %>%
        DT::datatable(
          extensions = "Buttons",
          options = list(
            scrollX = TRUE,
            scrollY = '350px',
            paging = FALSE,
            dom = "Blfrtip",
            buttons = list(list(
              extend = 'csv',
              filename = self$fileName$bluntFileName("outliersDetectionData"),
              text = "Download"
            ))
          ))
    )
  }
  else{
    output$tbl.outliersDetectData <- DT::renderDataTable(NULL)
  }
})

##########################
# outlier mutate functions
##########################
pgu.delegate$set("public", "updateOutliersMutateFeature", function(input, output, session){
  if(self$status$query(processName = "outliersDetected")){
    shiny::updateSelectInput(session,
                             "si.outliersMutateFeature",
                             choices = self$outliers$outliersParameter[["features"]],
                             selected = self$outliers$outliersParameter[["features"]][1]
    )
  }
})

pgu.delegate$set("public", "updateOutliersMutateMethod", function(input, output, session){
  if(self$status$query(processName = "outliersDetected")){
    shiny::updateSelectInput(session,
                             "si.outliersMutateMethod",
                             choices = self$outliers$cleaningAgentAlphabet,
                             selected = self$outliers$cleaningAgent
    )
  }
})

pgu.delegate$set("public", "updateOutliersMutateSeed", function(input, output, session){
  if(self$status$query(processName = "outliersDetected")){
    shiny::updateNumericInput(session,
                              "ni.outliersMutateSeed",
                              value = self$outliers$seed)
  }
})

pgu.delegate$set("public", "updateOutliersMutateIterations", function(input, output, session){
  if(self$status$query(processName = "outliersDetected")){
    shiny::updateNumericInput(session,
                              "ni.outliersMutateIterations",
                              value = self$outliers$iterations)
  }
})

pgu.delegate$set("public", "updateOutliersMutateGui", function(input, output, session){
  if(self$status$query(processName = "outliersDetected")){
    self$updateOutliersMutateFeature(input, output, session)
    self$updateOutliersMutateMethod(input, output, session)
    self$updateOutliersMutateSeed(input, output, session)
    self$updateOutliersMutateIterations(input, output, session)
  }
})

pgu.delegate$set("public", "outliersMutate", function(input, output, session){
  if(self$status$query(processName = "outliersDetected")){
    private$.outliers$setCleaningAgent <- input$si.outliersMutateMethod
    private$.outliers$setSeed <- input$ni.outliersMutateSeed
    private$.outliers$setIterations <- input$ni.outliersMutateIterations
    
    progress <- shiny::Progress$new(session, min = 1, max = 2 * length(self$imputedData$numericFeatureNames))
    progress$set(message = "Mutate Outliers", value = 0)
    on.exit(progress$close())
    
    name  <- as.name("Sample Name")
    private$.revisedData$setRawData <- self$imputedData$numericData() %>%
      self$outliers$handleOutliers(progress = progress) %>%
      tibble::add_column(!! name := self$imputedData$rawData %>%
                           dplyr::select(!!name) %>%
                           unlist() %>%
                           as.character()) %>%
      dplyr::select(c(!!name, self$imputedData$numericFeatureNames))
    private$.cleanedData$setRawData <- self$revisedData$numericData() %>%
      self$model$rescaleData() %>%
      self$transformator$reverseMutateData() %>%
      tibble::add_column(!! name := self$imputedData$rawData %>%
                         dplyr::select(!!name) %>%
                         unlist() %>%
                         as.character()) %>%
      dplyr::select(c(!!name, self$imputedData$numericFeatureNames))
    private$.status$update(processName = "outliersMutated", value = TRUE)
  }
  else{
    shiny::showNotification(paste("No outliers detected. Please detect outliers first."),type = "error", duration = 10)
  }
})

pgu.delegate$set("public", "updateOutliersMutateFeatureDetailGraphic", function(input, output, session){
  if(self$status$query(processName = "outliersMutated")){
    output$plt.outliersMutateFeatureDetail <- shiny::renderPlot(
      self$outliers$featurePlot(data = self$revisedData$numericData(),
                                feature = input$si.outliersMutateFeature)
    )
  }
  else{
    output$plt.outliersMutateFeatureDetail <- shiny::renderPlot(NULL)
  }
})

pgu.delegate$set("public", "updateOutliersMutateFeatureDetailTbl", function(input, output, session){
  if (self$status$query(processName = "outliersMutated")){
    output$tbl.outliersMutateFeatureDetail <- DT::renderDataTable(
      self$filteredMetadata$rawData %>%
        dplyr::right_join(self$revisedData$rawData %>%
                            dplyr::select(c("Sample Name", input$si.outliersMutateFeature))  ,
                          by = "Sample Name") %>%
        dplyr::slice(self$outliers$outliersIdxByFeature(feature = input$si.outliersMutateFeature)) %>%
        format.data.frame(scientific = TRUE, digits = 4) %>%
        DT::datatable(
          extensions = "Buttons",
          options = list(
            scrollX = TRUE,
            scrollY = '350px',
            paging = FALSE,
            dom = "Blfrtip",
            buttons = list(list(
              extend = 'csv',
              filename = self$fileName$bluntFileName("outliersMutationDetails"),
              text = "Download"
            ))
          ))
    ) 
  }
  else{
    output$tbl.outliersMutateFeatureDetail <- DT::renderDataTable(NULL)
  }
})

pgu.delegate$set("public", "updateOutliersMutateDetailTbl", function(input, output, session){
  if(self$status$query(processName = "outliersMutated")){
    dfOutlier <- self$outliers$outliers
    idx <- dfOutlier[["measurement"]][!duplicated(dfOutlier[["measurement"]])]
    output$tbl.outliersMutateDetail <- DT::renderDataTable(
      self$filteredMetadata$rawData %>%
        dplyr::right_join(self$revisedData$rawData, by = "Sample Name") %>%
        dplyr::slice(idx) %>%
        format.data.frame(scientific = TRUE, digits = 4) %>%
        DT::datatable(
          extensions = "Buttons",
          options = list(
            scrollX = TRUE,
            scrollY = '350px',
            paging = FALSE,
            dom = "Blfrtip",
            buttons = list(list(
              extend = 'csv',
              filename = self$fileName$bluntFileName("outliersMutationDetail"),
              text = "Download"
            ))
          ))
    )
  }
  else{
    output$tbl.outliersMutateDetail <- DT::renderDataTable(NULL)
  }
})

pgu.delegate$set("public", "updateOutliersMutateDataTbl", function(input, output, session){
  if(self$status$query(processName = "outliersMutated")){
    output$tbl.outliersMutateData <- DT::renderDataTable(
      self$filteredMetadata$rawData %>%
        dplyr::right_join(self$revisedData$rawData, by = "Sample Name") %>%
        format.data.frame(scientific = TRUE, digits = 4) %>%
        DT::datatable(
          extensions = "Buttons",
          options = list(
            scrollX = TRUE,
            scrollY = '350px',
            paging = FALSE,
            dom = "Blfrtip",
            buttons = list(list(
              extend = 'csv',
              filename = self$fileName$bluntFileName("outliersMutationData"),
              text = "Download"
            ))
          ))
    )
  }
  else{
    output$tbl.outliersMutateData <- DT::renderDataTable(NULL)
  }
})

#######################
# correlation functions
#######################
pgu.delegate$set("public", "correlate", function(input, output, session){
  if(self$status$query(processName = "outliersMutated")){
    progress <- shiny::Progress$new(session, min = 1, max  = 3 * length(self$imputedData$numericFeatureNames) ** 2)
    progress$set(message = "Calculate Correlation", value = 1)
    on.exit(progress$close())
    # self$cleanedData$numericData() %>%
    self$revisedData$numericData() %>%
      dplyr::select_if(function(x){!all(is.na(x))}) %>%
      private$.correlator$resetCorrelator(progress = progress)
    private$.status$update(processName = "correlated", value = TRUE)
  }
  else{
    shiny::showNotification(paste("No outliers revised. Please revise outliers first."),type = "error", duration = 10)
  }
})

pgu.delegate$set("public", "updateCorrelationMatrixRTbl", function(input, output, session){
  if(self$status$query(processName = "correlated")){
    output$tbl.correlationMatrixR <- DT::renderDataTable(
      self$correlator$printRTbl() %>%
        format.data.frame(scientific = TRUE, digits = 4) %>%
        DT::datatable(
          extensions = "Buttons",
          options = list(
            scrollX = TRUE,
            scrollY = '350px',
            paging = FALSE,
            dom = "Blfrtip",
            buttons = list(list(
              extend = 'csv',
              filename = self$fileName$bluntFileName("CorrelationMatrix_R"),
              text = "Download"
            )),
            autoWidth = TRUE,
            columnDefs = list(list(width = '50px', targets = "_all"))
          ))
    )
  }
  else{
    output$tbl.correlationMatrixR <- DT::renderDataTable(NULL)
  }
})

pgu.delegate$set("public", "updateCorrelationMatrixPPearsonTbl", function(input, output, session){
  if(self$status$query(processName = "correlated")){
    output$tbl.correlationMatrixPPearson <- DT::renderDataTable(
      self$correlator$printPPearsonTbl() %>%
        format.data.frame(scientific = TRUE, digits = 4) %>%
        DT::datatable(
          extensions = "Buttons",
          options = list(
            scrollX = TRUE,
            scrollY = '350px',
            paging = FALSE,
            dom = "Blfrtip",
            buttons = list(list(
              extend = 'csv',
              filename = self$fileName$bluntFileName("CorrelationMatrix_P_Pearson"),
              text = "Download"
            )),
            autoWidth = TRUE,
            columnDefs = list(list(width = '50px', targets = "_all"))
          ))
    )
  }
  else{
    output$tbl.correlationMatrixPPearson <- DT::renderDataTable(NULL)
  }
})

pgu.delegate$set("public", "updateCorrelationMatrixTauTbl", function(input, output, session){
  if(self$status$query(processName = "correlated")){
    output$tbl.correlationMatrixTau <- DT::renderDataTable(
      self$correlator$printTauTbl() %>%
        format.data.frame(scientific = TRUE, digits = 4) %>%
        DT::datatable(
          extensions = "Buttons",
          options = list(
            scrollX = TRUE,
            scrollY = '350px',
            paging = FALSE,
            dom = "Blfrtip",
            buttons = list(list(
              extend = 'csv',
              filename = self$fileName$bluntFileName("CorrelationMatrix_Tau"),
              text = "Download"
            )),
            autoWidth = TRUE,
            columnDefs = list(list(width = '50px', targets = "_all"))
          ))
    )
  }
  else{
    output$tbl.correlationMatrixTau <- DT::renderDataTable(NULL)
  }
})

pgu.delegate$set("public", "updateCorrelationMatrixPKendallTbl", function(input, output, session){
  if(self$status$query(processName = "correlated")){
    output$tbl.correlationMatrixPKendall <- DT::renderDataTable(
      self$correlator$printPKendallTbl() %>%
        format.data.frame(scientific = TRUE, digits = 4) %>%
        DT::datatable(
          extensions = "Buttons",
          options = list(
            scrollX = TRUE,
            scrollY = '350px',
            paging = FALSE,
            dom = "Blfrtip",
            buttons = list(list(
              extend = 'csv',
              filename = self$fileName$bluntFileName("CorrelationMatrix_P_Kendall"),
              text = "Download"
            )),
            autoWidth = TRUE,
            columnDefs = list(list(width = '50px', targets = "_all"))
          ))
    )
  }
  else{
    output$tbl.correlationMatrixPKendall <- DT::renderDataTable(NULL)
  }
})

pgu.delegate$set("public", "updateCorrelationMatrixRhoTbl", function(input, output, session){
  if(self$status$query(processName = "correlated")){
    output$tbl.correlationMatrixRho <- DT::renderDataTable(
      self$correlator$printRhoTbl() %>%
        format.data.frame(scientific = TRUE, digits = 4) %>%
        DT::datatable(
          extensions = "Buttons",
          options = list(
            scrollX = TRUE,
            scrollY = '350px',
            paging = FALSE,
            dom = "Blfrtip",
            buttons = list(list(
              extend = 'csv',
              filename = self$fileName$bluntFileName("CorrelationMatrix_Rho"),
              text = "Download"
            )),
            autoWidth = TRUE,
            columnDefs = list(list(width = '50px', targets = "_all"))
          ))
    )
  }
  else{
    output$tbl.correlationMatrixRho <- DT::renderDataTable(NULL)
  }
})

pgu.delegate$set("public", "updateCorrelationMatrixPSpearmanTbl", function(input, output, session){
  if(self$status$query(processName = "correlated")){
    output$tbl.correlationMatrixPSpearman <- DT::renderDataTable(
      self$correlator$printPSpearmanTbl() %>%
        format.data.frame(scientific = TRUE, digits = 4) %>%
        DT::datatable(
          extensions = "Buttons",
          options = list(
            scrollX = TRUE,
            scrollY = '350px',
            paging = FALSE,
            dom = "Blfrtip",
            buttons = list(list(
              extend = 'csv',
              filename = self$fileName$bluntFileName("CorrelationMatrix_P_Spearman"),
              text = "Download"
            )),
            autoWidth = TRUE,
            columnDefs = list(list(width = '50px', targets = "_all"))
          ))
    )
  }
  else{
    output$tbl.correlationMatrixPSpearman <- DT::renderDataTable(NULL)
  }
})

######################
# regression functions
######################
pgu.delegate$set("public", "regression", function(input, output, session){
  if(self$status$query(processName = "correlated")){
    progress <- shiny::Progress$new(session, min = 1, max = length(self$cleanedData$numericFeatureNames)**2)
    progress$set(message = "Calculate Regression", value = 1)
    on.exit(progress$close())
    # private$.regressor$resetRegressor(data = self$cleanedData$numericData(), progress = progress)
    private$.regressor$resetRegressor(data = self$revisedData$numericData(), progress = progress)
    private$.status$update(processName = "regression", value = TRUE) 
  }
  else{
    shiny::showNotification(paste("No outliers revised. Please revise outliers first."),type = "error", duration = 10)
  }
})

pgu.delegate$set("public", "updateRegressionAbscissa", function(input, output, session){
  if(self$status$query(processName = "regression")){
    shiny::updateSelectInput(session,
                             "si.regressionAbs",
                             choices = self$regressor$featureNames,
                             selected = self$regressor$featureNames[1]
    )
  }
})

pgu.delegate$set("public", "updateRegressionOrdinate", function(input, output, session){
  if(self$status$query(processName = "regression")){
    ordinateFeatureNames <- self$regressor$featureNames[-self$regressor$featureIdx(input$si.regressionAbs)]
    shiny::updateSelectInput(session,
                             "si.regressionOrd",
                             choices = ordinateFeatureNames,
                             selected = ordinateFeatureNames[1]
    )
  }
})

pgu.delegate$set("public", "updateRegressionGui", function(input, output, session){
  self$updateRegressionAbscissa(input, output, session)
  self$updateRegressionOrdinate(input, output, session)
})

pgu.delegate$set("public", "updateRegressionGraphic", function(input, output, session){
  if(self$status$query(processName = "regression")){
    output$plt.regressionFeature <- shiny::renderPlot(
      # self$regressor$plotModel(data = self$cleanedData$numericData(),
      self$regressor$plotModel(data = self$revisedData$numericData(),
                               abscissa = input$si.regressionAbs,
                               ordinate = input$si.regressionOrd)
      )
  }
  else{
    output$plt.regressionFeature <- shiny::renderPlot(NULL)
  }
})

pgu.delegate$set("public", "updateRegressionModelTbl", function(input, output, session){
  if(self$status$query(processName = "regression")){
    output$tbl.regressionFeature <- DT::renderDataTable(
      self$regressor$printModel(abscissa = input$si.regressionAbs, ordinate = input$si.regressionOrd) %>%
        # format.data.frame(scientific = TRUE, digits = 4) %>%
        DT::datatable(
          extensions = "Buttons",
          options = list(
            scrollX = TRUE,
            scrollY = '350px',
            paging = FALSE,
            dom = "Blfrtip",
            buttons = list(list(
              extend = 'csv',
              filename = self$fileName$bluntFileName("RegressionMatrix_Model"),
              text = "Download"
            )),
            autoWidth = TRUE,
            columnDefs = list(list(width = '50px', targets = "_all"))
          ))
    )
  }
  else{
    output$tbl.regressionFeature <- DT::renderDataTable(NULL)
  }
})

pgu.delegate$set("public", "updateRegressionInterceptTbl", function(input, output, session){
  if(self$status$query(processName = "regression")){
    output$tbl.regressionIntercept <- DT::renderDataTable(
      self$regressor$printInterceptTbl() %>%
        format.data.frame(scientific = TRUE, digits = 4) %>%
        DT::datatable(
          extensions = "Buttons",
          options = list(
            scrollX = TRUE,
            scrollY = '350px',
            paging = FALSE,
            dom = "Blfrtip",
            buttons = list(list(
              extend = 'csv',
              filename = self$fileName$bluntFileName("RegressionMatrix_Intercept"),
              text = "Download"
            )),
            autoWidth = TRUE,
            columnDefs = list(list(width = '50px', targets = "_all"))
          ))
    )
  }
  else{
    output$tbl.regressionIntercept <- DT::renderDataTable(NULL)
  }
})

pgu.delegate$set("public", "updateRegressionPInterceptTbl", function(input, output, session){
  if(self$status$query(processName = "regression")){
    output$tbl.regressionPIntercept <- DT::renderDataTable(
      self$regressor$printPInterceptTbl() %>%
        format.data.frame(scientific = TRUE, digits = 4) %>%
        DT::datatable(
          extensions = "Buttons",
          options = list(
            scrollX = TRUE,
            scrollY = '350px',
            paging = FALSE,
            dom = "Blfrtip",
            buttons = list(list(
              extend = 'csv',
              filename = self$fileName$bluntFileName("RegressionMatrix_P_Intercept"),
              text = "Download"
            )),
            autoWidth = TRUE,
            columnDefs = list(list(width = '50px', targets = "_all"))
          ))
    )
  }
  else{
    output$tbl.regressionPIntercept <- DT::renderDataTable(NULL)
  }
})

pgu.delegate$set("public", "updateRegressionSlopeTbl", function(input, output, session){
  if(self$status$query(processName = "regression")){
    output$tbl.regressionSlope <- DT::renderDataTable(
      self$regressor$printSlopeTbl() %>%
        format.data.frame(scientific = TRUE, digits = 4) %>%
        DT::datatable(
          extensions = "Buttons",
          options = list(
            scrollX = TRUE,
            scrollY = '350px',
            paging = FALSE,
            dom = "Blfrtip",
            buttons = list(list(
              extend = 'csv',
              filename = self$fileName$bluntFileName("RegressionMatrix_Slope"),
              text = "Download"
            )),
            autoWidth = TRUE,
            columnDefs = list(list(width = '50px', targets = "_all"))
          ))
    )
  }
  else{
    output$tbl.regressionSlope <- DT::renderDataTable(NULL)
  }
})

pgu.delegate$set("public", "updateRegressionPSlopeTbl", function(input, output, session){
  if(self$status$query(processName = "regression")){
    output$tbl.regressionPSlope <- DT::renderDataTable(
      self$regressor$printPSlopeTbl() %>%
        format.data.frame(scientific = TRUE, digits = 4) %>%
        DT::datatable(
          extensions = "Buttons",
          options = list(
            scrollX = TRUE,
            scrollY = '350px',
            paging = FALSE,
            dom = "Blfrtip",
            buttons = list(list(
              extend = 'csv',
              filename = self$fileName$bluntFileName("RegressionMatrix_P_Slope"),
              text = "Download"
            )),
            autoWidth = TRUE,
            columnDefs = list(list(width = '50px', targets = "_all"))
          ))
    )
  }
  else{
    output$tbl.regressionPSlope <- DT::renderDataTable(NULL)
  }
})

############################
# numerical output functions
############################
pgu.delegate$set("public", "updateRawDataInfo", function(input, output, session){
  if(private$.status$query(processName = "dataImported")){
    output$tbl.rawDataInfo <- DT::renderDataTable({self$rawData$dataInformation() %>%
        DT::datatable(
          extensions = "Buttons",
          options = list(
            scrollX = TRUE,
            scrollY = '75vh',
            paging = FALSE,
            dom = "Blfrtip",
            buttons = list(list(
              extend = 'csv',
              filename = self$fileName$bluntFileName("rawData_type"),
              text = "Download"
            ))
          )
        )}
    )
  }
  else{
    output$tbl.rawDataInfo <- DT::renderDataTable(NULL)
  }
})

pgu.delegate$set("public", "updateLoqInfo", function(input, output, session){
  if(private$.status$query(processName = "dataImported")){
    output$tbl.loqInfo <- DT::renderDataTable({self$loq$dataInformation() %>%
        DT::datatable(
          extensions = "Buttons",
          options = list(
            scrollX = TRUE,
            scrollY = '75vh',
            paging = FALSE,
            dom = "Blfrtip",
            buttons = list(list(
              extend = 'csv',
              filename = self$fileName$bluntFileName("loqData_type"),
              text = "Download"
            ))
          )
        )
      })
  }
  else{
    output$tbl.loqInfo <- DT::renderDataTable(NULL)
  }
})

pgu.delegate$set("public", "updateMetadataInfo", function(input, output, session){
  if(private$.status$query(processName = "metadataImported")){
    output$tbl.metadataInfo <- DT::renderDataTable({self$metadata$dataInformation() %>%
        DT::datatable(
          extensions = "Buttons",
          options = list(
            scrollX = TRUE,
            scrollY = '75vh',
            paging = FALSE,
            dom = "Blfrtip",
            buttons = list(list(
              extend = 'csv',
              filename = self$fileName$bluntFileName("metadata_type"),
              text = "Download"
            ))
          )
        )
      })
  }
  else{
    output$tbl.metadataInfo <- DT::renderDataTable(NULL)
  }
})


pgu.delegate$set("public", "updateRawDataStatisticsTbl", function(input, output, session){
  if(private$.status$query(processName = "dataImported")){
    output$tbl.rawDataStatistics <- DT::renderDataTable({self$rawData$dataStatistics() %>%
        format.data.frame(scientific = TRUE, digits = 4) %>%
        DT::datatable(
          extensions = "Buttons",
          options = list(
            scrollX = TRUE,
            scrollY = '25vh',
            paging = FALSE,
            dom = "Blfrtip",
            buttons = list(list(
              extend = 'csv',
              filename = self$fileName$bluntFileName("rawData_statistics"),
              text = "Download"
            ))
          )
        )}
    )
  }
  else{
    output$tbl.rawDataStatistics <- DT::renderDataTable(NULL)
  }
})

pgu.delegate$set("public", "updateFilterTbl", function(input, output, session){
  if(private$.status$query(processName = "dataImported")){
    df <- self$metadata$rawData %>%
      dplyr::right_join(self$rawData$rawData, by = "Sample Name")
    output$tbl.filter <- DT::renderDataTable({df %>%
      DT::datatable(
        extensions = "Buttons",
        rownames = FALSE,
        selection = list(target = "column"),
        filter = "top",
        options = list(
          scrollX = TRUE,
          scrollY = '45vh',
          paging = FALSE,
          autoWidth = TRUE,
          columnDefs = list(list(width = '75px', targets = "_all")),
          dom = "Blfrtip",
          buttons = list(list(
            extend = 'csv',
            filename = self$fileName$bluntFileName("filter_data"),
            text = "Download"
          ))
        )
      )
    })
  }
  else{
    output$tbl.filter <- DT::renderDataTable(NULL)
  }
})

pgu.delegate$set("public", "updateFilterStatisticsTbl", function(input, output, session){
  if(private$.status$query(processName = "dataFiltered")){
    output$tbl.filterStatistics <- DT::renderDataTable({self$filteredData$dataStatistics() %>%
        format.data.frame(scientific = TRUE, digits = 4) %>%
        DT::datatable(
          extensions = "Buttons",
          options = list(
            scrollX = TRUE,
            scrollY = '25vh',
            paging = FALSE,
            autoWidth = TRUE,
            columnDefs = list(list(width = '50px', targets = "_all")),
            dom = "Blfrtip",
            buttons = list(list(
              extend = 'csv',
              filename = self$fileName$bluntFileName("filter_statistics"),
              text = "Download"
            ))
          )
        )
    })
  }
  else{
    output$tbl.filterStatistics <- DT::renderDataTable(NULL)
  }
})

pgu.delegate$set("public", "updateFilterMissingsTbl", function(input, output, session){
  if(private$.status$query(processName = "dataFiltered")){
    output$tbl.filterMissings <- DT::renderDataTable({self$filteredData$missings() %>%
        format.data.frame(scientific = FALSE, digits = 2) %>%
        DT::datatable(
          options = list(
            scrollX = TRUE,
            scrollY = '25vh',
            paging = FALSE,
            autoWidth = TRUE,
            columnDefs = list(list(width = '50px', targets = "_all")),
            dom = "Blfrtip",
            buttons = list(list(
              extend = 'csv',
              filename = self$fileName$bluntFileName("filter_missings"),
              text = "Download"
            ))
          )
        )
    })
  }
  else{
    output$tbl.filterMissings <- DT::renderDataTable(NULL)
  }
})

#########################
# data export functions #
#########################
pgu.delegate$set("public", "exportFileName", function(input, output, session){
  private$.fileName$setSuffix <- "xlsx"
  private$.fileName$updateTimeString()
  private$.fileName$exportFileName() %>%
    return()
})

pgu.delegate$set("public", "exportData", function(file){
  if(self$status$query(processName = "outliersMutated")){
    private$.exporter$setFileName <- file
    analysis_parameter <- tibble::tibble(
      parameter = c("value"),
      loq_na_handling = c(self$loq$naHandlingAgent),
      lloq_substitute = c(self$loq$lloqSubstituteAgent),
      uloq_substitute = c(self$loq$uloqSubstituteAgent),
      imputation_method = c(self$imputer$imputationAgent),
      imputation_seed = c(self$imputer$seed),
      imputation_iterations = c(self$imputer$iterations),
      outlier_method = c(self$outliers$cleaningAgent),
      outier_seed = c(self$outliers$seed),
      outlier_iterations = c(self$outliers$iterations)
    )
    
    list(raw_data = self$cleanedData$rawData,
         loq = self$loq$loq,
         metadata = self$filteredMetadata$rawData,
         transfromed_data = self$revisedData$rawData,
         trafo_parameter = self$transformator$trafoParameter,
         model_parameter = self$model$modelParameterData(),
         model_quality = self$model$modelQualityData(),
         model_statistics = self$model$testResultData(),
         analysis_parameter = analysis_parameter
         ) %>%
           self$exporter$writeDataToExcel()
  }
})

###################################
# update graphical user interface #
###################################
pgu.delegate$set("public", "hideOutdatedResults", function(input, output, session){
  if(!private$.status$query(processName = "dataImported")){
    output$tbl.rawDataInfo <- DT::renderDataTable(NULL)
    output$tbl.loqInfo <- DT::renderDataTable(NULL)
    output$tbl.rawDataStatistics <- DT::renderDataTable(NULL)
    output$tbl.filter <- DT::renderDataTable(NULL)
  }
  if(!private$.status$query(processName = "metadataImported")){
    output$tbl.metadataInfo <- DT::renderDataTable(NULL)
  }
  if(!private$.status$query(processName = "dataFiltered")){
    output$plt.exploreGraphic <- shiny::renderPlot(NULL)
    output$plt.exploreAbscissaGraphic <- shiny::renderPlot(NULL)
    output$plt.exploreOrdinateGraphic <- shiny::renderPlot(NULL)
    output$tbl.exploreAbscissaStatistics <- DT::renderDataTable(NULL)
    output$tbl.exploreOrdinateStatistics <- DT::renderDataTable(NULL)
    output$tbl.filterStatistics <- DT::renderDataTable(NULL)
    output$tbl.filterMissings <- DT::renderDataTable(NULL)
  }
  if(!private$.status$query(processName = "loqDetected")){
    output$tbl.loqDetectStatistics <- DT::renderDataTable(NULL)
    output$tbl.loqDetectOutlier <- DT::renderDataTable(NULL)
    output$tbl.loqDetectData <- DT::renderDataTable(NULL)
    output$plt.loqDetectStatistics <- shiny::renderPlot(NULL)
    output$plt.loqDetectFeature <- shiny::renderPlot(NULL)
    output$tbl.loqDetectFeature <- DT::renderDataTable(NULL)
  }
  if(!private$.status$query(processName = "loqMutated")){
    output$tbl.loqMutateStatistics <- DT::renderDataTable(NULL)
    output$tbl.loqMutateOutlier <- DT::renderDataTable(NULL)
    output$tbl.loqMutateData <- DT::renderDataTable(NULL)
    output$plt.loqMutateStatistics <- shiny::renderPlot(NULL)
    output$tbl.loqMutateFeature <- DT::renderDataTable(NULL)
    output$plt.loqMutateFeature <- shiny::renderPlot(NULL)
  }
  if(!self$status$query(processName = "modelOptimized")){
    output$tbl.trafoDetectTypes <- DT::renderDataTable(NULL)
    output$tbl.trafoDetectParameters <- DT::renderDataTable(NULL)
  }
  if(!self$status$query(processName = "modelDefined")){
    output$plt.trafoMutateFeature <- shiny::renderPlot(NULL)
    output$tbl.trafoMutateFeatureParameter <- DT::renderDataTable(NULL)
    output$tbl.trafoMutateFeatureQuality <- DT::renderDataTable(NULL)
    output$tbl.trafoMutateGlobalParameter <- DT::renderDataTable(NULL)
    output$tbl.trafoMutateGlobalModel <- DT::renderDataTable(NULL)
    output$tbl.trafoMutateGlobalQuality <- DT::renderDataTable(NULL)
    output$tbl.trafoMutateGlobalTests <- DT::renderDataTable(NULL)
    output$tbl.trafoMutateGlobalData <- DT::renderDataTable(NULL)
  }
  if(!self$status$query(processName = "naDetected")){
    output$plt.imputeDetectSummary <- shiny::renderPlot(NULL)
    output$tbl.imputeDetectStatistics <- DT::renderDataTable(NULL)
    output$tbl.imputeDetectDetail <- DT::renderDataTable(NULL)
    output$tbl.imputeDetectData <- DT::renderDataTable(NULL)
  }
  if(!self$status$query(processName = "naMutated")){
    output$plt.imputeMutateFeatureDetail <- shiny::renderPlot(NULL)
    output$tbl.imputeMutateFeatureDetail <- DT::renderDataTable(NULL)
    output$tbl.imputeMutateDetail <- DT::renderDataTable(NULL)
    output$tbl.imputeMutateData <- DT::renderDataTable(NULL)
  }
  if(!self$status$query(processName = "outliersDetected")){
    output$plt.outliersDetectSummary <- shiny::renderPlot(NULL)
    output$tbl.outliersDetectStatistics <- DT::renderDataTable(NULL)
    output$tbl.outliersDetectDetail <- DT::renderDataTable(NULL)
    output$tbl.outliersDetectData <- DT::renderDataTable(NULL)
  }
  if(!self$status$query(processName = "outliersMutated")){
    output$plt.outliersMutateFeatureDetail <- shiny::renderPlot(NULL)
    output$tbl.outliersMutateFeatureDetail <- DT::renderDataTable(NULL)
    output$tbl.outliersMutateDetail <- DT::renderDataTable(NULL)
    output$tbl.outliersMutateData <- DT::renderDataTable(NULL)
  }
  if(!self$status$query(processName = "correlated")){
    output$tbl.correlationMatrixR <- DT::renderDataTable(NULL)
    output$tbl.correlationMatrixPPearson <- DT::renderDataTable(NULL)
    output$tbl.correlationMatrixTau <- DT::renderDataTable(NULL)
    output$tbl.correlationMatrixPKendall <- DT::renderDataTable(NULL)
    output$tbl.correlationMatrixRho <- DT::renderDataTable(NULL)
    output$tbl.correlationMatrixPSpearman <- DT::renderDataTable(NULL)
  }
  if(!self$status$query(processName = "regression")){
    output$plt.regressionFeature <- shiny::renderPlot(NULL)
    output$tbl.regressionFeature <- DT::renderDataTable(NULL)
    output$tbl.regressionIntercept <- DT::renderDataTable(NULL)
    output$tbl.regressionPIntercept <- DT::renderDataTable(NULL)
    output$tbl.regressionSlope <- DT::renderDataTable(NULL)
    output$tbl.regressionPSlope <- DT::renderDataTable(NULL)
  }
})