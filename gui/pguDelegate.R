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
                            .featureModel = "pgu.normDist"
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
    private$.status$update(processName = "dataImported", value = FALSE)
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
    shiny::showNotification(paste("No file uploaded to import. Please upload a valid file first."),type = "error", duration = 10)
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
    shiny::showNotification(paste("No file uploaded to import. Please upload a valid file first."),type = "error", duration = 10)
  }
})

pgu.delegate$set("public", "resetFilter", function(input, output, session){
  if(private$.status$query(processName = "dataImported")){
    private$.filterSet$resetRowIdx(data = self$rawData$rawData)
    private$.filterSet$resetColIdx(data = self$rawData$rawData)
  }
  else{
    shiny::showNotification(paste("No file uploaded to import. Please upload a valid file first."),type = "error", duration = 10)
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
})

pgu.delegate$set("public", "updateExplorationAbscissaGraphic", function(input, output, session){
  if(private$.status$query(processName = "dataFiltered")){
    output$plt.exploreAbscissaGraphic <- shiny::renderPlot(
      self$explorer$abscissaPlot(),
      height = 400
    )
  }
})

pgu.delegate$set("public", "updateExplorationOrdinateGraphic", function(input, output, session){
  if(private$.status$query(processName = "dataFiltered")){
    output$plt.exploreOrdinateGraphic <- shiny::renderPlot(
      self$explorer$ordinatePlot(),
      height = 400
    )
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
})

pgu.delegate$set("public", "updateLoqDetectFeatureGraphic", function(input, output, session){
  if(self$status$query(processName = "loqDetected")){
    output$plt.loqDetectFeature <- shiny::renderPlot(
      self$loq$featurePlot(obj = self$filteredData$rawData, feature = input$si.loqDetectFeature),
      height = 425
    )
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
})

pgu.delegate$set("public", "updateLoqMutateFeatureGraphic", function(input, output, session){
  if(self$status$query(processName = "loqMutated")){
    output$plt.loqMutateFeature <- shiny::renderPlot(
      self$loq$featurePlot(obj = self$loqMutatedData$rawData, feature = input$si.loqMutateFeature),
      height = 425
    )
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
})

pgu.delegate$set("public", "trafoMutateFeature", function(input, output, session){
  if(self$status$query(processName = "modelDefined")){
    private$.transformator$setTrafoType(feature = input$si.trafoMutateFeature,
                                        type = input$si.trafoMutateType)
    private$.transformator$setMirrorLogic(feature = input$si.trafoMutateFeature,
                                          logic = input$cb.trafoMutateMirror)
    
    self$loqMutatedData$numericData() %>%
      private$.transformator$estimateTrafoParameter()
    
    self$loqMutatedData$numericData %>%
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
})

pgu.delegate$set("public", "updateTrafoMutateFeatureGraphic", function(input, output, session){
  if(self$status$query(processName = "modelDefined")){
    output$plt.trafoMutateFeature <- shiny::renderPlot(
      self$model$plotModel(feature = input$si.trafoMutateFeature)
    )
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

############################
# graphical output functions
############################
