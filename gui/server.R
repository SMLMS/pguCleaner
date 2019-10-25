library(shiny)
library(shinydashboard)
library(tidyverse)
library(DT)
library(promises)
library(future)
library(future.callr)
plan(callr)
source(file = "../R/pguFile.R", local=TRUE)
source(file = "../R/pguImporter.R", local=TRUE)
source(file = "../R/pguData.R", local=TRUE)
source(file = "../R/pguFilter.R", local=TRUE)
source(file = "../R/pguPlot.R", local=TRUE)
source(file = "../R/pguTransformator.R", local=TRUE)
source(file = "../R/pguModel.R", local=TRUE)
source(file = "../R/pguOptimizer.R", local=TRUE)
# source(file = "../R/pgu_exporter.R", local=TRUE)
# source(file = "../R/pgu_hdf5.R", local=TRUE)
# source(file = "../R/pgu_data.R", local=TRUE)
# source(file = "../R/pgu_filter.R", local=TRUE)
# source(file = "../R/pgu_transformator.R", local=TRUE)
# source(file = "../R/pgu_normalizer.R", local=TRUE)
# source(file = "../R/pgu_globals.R", local=TRUE)
# source(file = "../R/pgu_cleaner.R", local=TRUE)
# source(file = "../R/pgu_optimizer.R", local=TRUE)
# source(file = "../R/pgu_regressor.R", local=TRUE)
# source(file = "../R/pgu_correlator.R", local=TRUE)

shinyServer(function(input, output, session) {
  ######################
  # init reactive values
  ######################
  dataLoaded <- shiny::reactiveVal(value = FALSE)
  rawData <- pgu.data$new()
  filteredData <- pgu.data$new()
  transformedData <- pgu.data$new()
  
  inFile <- pgu.file$new()
  importer <- pgu.importer$new()
  filterSet <- pgu.filter$new()
  plt <- pgu.plot$new()
  transformator <- pgu.transformator$new()
  model <- pgu.model$new()
  optimizer <- pgu.optimizer$new()
  
  
  ###############
  # import button
  ###############
  shiny::observeEvent(input$ab.import,{
    print(input$fi.import$datapath)
    if (length(input$fi.import$datapath) > 0){
      inFile$setUploadFileName <- input$fi.import$datapath
      inFile$setFileName <- input$fi.import$name
      inFile$splitFileName()
      inFile$setSheetIndex <- input$ni.import
      inFile$setSkipRows <- input$ni.skipRows
      inFile$setSeparator <-input$rb.separator
      inFile$setHeader <- input$cb.header
    }
    else{
      dataLoaded(FALSE)
      shiny::showNotification(paste("Please select a valid file."),type = "error", duration = 10)
    }
    if(importer$suffixIsKnown(obj = inFile)){
      dataLoaded(TRUE)
    }
    else{
      dataLoaded(FALSE)
      errorMessage <- sprintf("File extension %s is not known.", inFile$suffix)
      shiny::showNotification(paste(errorMessage),type = "error", duration = 10)
    }
    if(dataLoaded()){
      tryCatch({
        rawData$setRawData <- importer$import(obj = inFile)
        dataLoaded(TRUE)
      },
      error = function(e) {
        dataLoaded(FALSE)
        shiny::showNotification(paste(e),type = "error", duration = 10)
      },
      warning = function(w) {
        dataLoaded(FALSE)
        shiny::showNotification(paste(w),type = "error", duration = 10)
      }
      )
    }
    # update Class Instances
    if(dataLoaded()){
      filterSet$resetFilter(data = rawData$rawData)
      filteredData$setRawData <- rawData$rawData %>%
        filterSet$filter()
    }
    # update output modules
    if(dataLoaded()){
      output$tbl.dataInfo <- DT::renderDataTable({rawData$dataInformation() %>%
          DT::datatable(options = list(
            scrollX = TRUE,
            #scrollY = '160px',
            scrollY = '14vh',
            paging = FALSE
          ))})
      output$tbl.rawDataStatistics <- DT::renderDataTable({rawData$dataStatistics() %>%
          format.data.frame(scientific = TRUE, digits = 4) %>%
          DT::datatable(options = list(
            scrollX = TRUE,
            scrollY = '14vh',
            #scrollY = TRUE,
            paging = FALSE
          ))})
      output$tbl.filterData <- DT::renderDataTable(
        rawData$rawData,
        selection = list(target = "column"),
        rownames = FALSE,
        options = list(
          scrollX = TRUE,
          scrollY = '350px',
          paging = FALSE,
          autoWidth = TRUE,
          columnDefs = list(list(width = '75px', targets = "_all"))
        ),
        filter = "top"
      )
      output$tbl.filterStatistics <- DT::renderDataTable({rawData$reducedDataStatistics() %>%
          format.data.frame(scientific = TRUE, digits = 4) %>%
          DT::datatable(options = list(
            scrollX = TRUE,
            scrollY = '14vh',
            #scrollY = TRUE,
            paging = FALSE))})
      output$tbl.filterMissings <- DT::renderDataTable({rawData$missings() %>%
          format.data.frame(scientific = FALSE, digits = 2) %>%
          DT::datatable(options = list(
            scrollX = TRUE,
            scrollY = '14vh',
            #scrollY = TRUE,
            paging = FALSE))})
    }
    else{
      output$tbl.dataInfo <- DT::renderDataTable(NULL)
      output$tbl.rawDataStatistics <- DT::renderDataTable(NULL)
      
    }
  })
  ###################
  # Set Filter button
  ###################
  shiny::observeEvent(input$ab.filterSet,{
    if(dataLoaded()){
      if (length(input$tbl.filterData_rows_all) < 1) {
        filterSet$resetRowIdx(data = rawData$rawData)
      }
      else {
        filterSet$setRowIdx <- input$tbl.filterData_rows_all
      }
      if (length(input$tbl.filterData_columns_selected) < 1) {
        filterSet$resetColIdx(data = rawData$rawData)
      }
      else{
        filterSet$setColIdx <- input$tbl.filterData_columns_selected + 1
      }
      filteredData$setRawData <- rawData$rawData %>%
        filterSet$filter()
      # updata output
      output$tbl.filterStatistics <- DT::renderDataTable({filteredData$reducedDataStatistics() %>%
          format.data.frame(scientific = TRUE, digits = 4) %>%
          DT::datatable(options = list(
            scrollX = TRUE,
            scrollY = '14vh',
            #scrollY = TRUE,
            paging = FALSE))})
      output$tbl.filterMissings <- DT::renderDataTable({filteredData$missings() %>%
          format.data.frame(scientific = FALSE, digits = 2) %>%
          DT::datatable(options = list(
            scrollX = TRUE,
            scrollY = '14vh',
            #scrollY = TRUE,
            paging = FALSE))})
      
    }
    else{
      errorMessage <- sprintf("No data loaded.", inFile$suffix)
      shiny::showNotification(paste(errorMessage),type = "error", duration = 10)
      output$tbl.filterStatistics <- DT::renderDataTable(NULL)
      output$tbl.filterMissings <- DT::renderDataTable(NULL)
      output$tbl.filterData <- DT::renderDataTable(NULL)
    }
  })
  ###########################
  # Set Inverse Filter button
  ###########################
  shiny::observeEvent(input$ab.filterInvSet,{
    if(dataLoaded()){
      if (length(input$tbl.filterData_rows_all) < 1) {
        filterSet$resetRowIdx(data = rawData$rawData)
      }
      else {
        filterSet$setRowIdx <- input$tbl.filterData_rows_all
      }
      if (length(input$tbl.filterData_columns_selected) < 1) {
        filterSet$resetColIdx(data = rawData$rawData)
      }
      else{
        idx <- seq(1,ncol(rawData$rawData), 1)
        idx <- idx[-c(input$tbl.filterData_columns_selected + 1)]
        filterSet$setColIdx <- idx
      }
      filteredData$setRawData <- rawData$rawData %>%
        filterSet$filter()
      # update output
      output$tbl.filterStatistics <- DT::renderDataTable({filteredData$reducedDataStatistics() %>%
          format.data.frame(scientific = TRUE, digits = 4) %>%
          DT::datatable(options = list(
            scrollX = TRUE,
            scrollY = '14vh',
            #scrollY = TRUE,
            paging = FALSE))})
      output$tbl.filterMissings <- DT::renderDataTable({filteredData$missings() %>%
          format.data.frame(scientific = FALSE, digits = 2) %>%
          DT::datatable(options = list(
            scrollX = TRUE,
            scrollY = '14vh',
            #scrollY = TRUE,
            paging = FALSE))})
      
    }
    else{
      errorMessage <- sprintf("No data loaded.", inFile$suffix)
      shiny::showNotification(paste(errorMessage),type = "error", duration = 10)
      output$tbl.filterStatistics <- DT::renderDataTable(NULL)
      output$tbl.filterMissings <- DT::renderDataTable(NULL)
      output$tbl.filterData <- DT::renderDataTable(NULL)
    }
  }) 
  #####################
  # Reset Filter button
  #####################
  shiny::observeEvent(input$ab.filterReset,{
    if(dataLoaded()){
      output$tbl.filterData <- DT::renderDataTable(
        rawData$rawData,
        selection = list(target = "column"),
        rownames = FALSE,
        options = list(
          scrollX = TRUE,
          scrollY = '350px',
          paging = FALSE,
          autoWidth = TRUE,
          columnDefs = list(list(width = '75px', targets = "_all"))
        ),
        filter = "top"
      )
      output$tbl.filterStatistics <- DT::renderDataTable({rawData$dataStatistics() %>%
          format.data.frame(scientific = TRUE, digits = 4) %>%
          DT::datatable(options = list(
            scrollX = TRUE,
            scrollY = '14vh',
            #scrollY = TRUE,
            paging = FALSE))})
      output$tbl.filterMissings <- DT::renderDataTable({rawData$missings() %>%
          format.data.frame(scientific = FALSE, digits = 2) %>%
          DT::datatable(options = list(
            scrollX = TRUE,
            scrollY = '14vh',
            #scrollY = TRUE,
            paging = FALSE))})
    }
    else{
      errorMessage <- sprintf("No data loaded.", inFile$suffix)
      shiny::showNotification(paste(errorMessage),type = "error", duration = 10)
      output$tbl.filterStatistics <- DT::renderDataTable(NULL)
      output$tbl.filterMissings <- DT::renderDataTable(NULL)
      output$tbl.filterData <- DT::renderDataTable(NULL)
    }
  })
  ##################
  # Explore Abscissa
  ##################
  shiny::observeEvent(input$si.exploreAbs,{
    filteredData$setAbscissa <- input$si.exploreAbs
    output$plt.exploreGraphics <- shiny::renderPlot(
      plt$scatterPlot(data = filteredData$rawData,
                      abs = filteredData$abscissa,
                      ord = filteredData$ordinate),
      height = 400)
  })
  ##################
  # Explore Ordinate
  ##################
  shiny::observeEvent(input$si.exploreOrd,{
    filteredData$setOrdinate <- input$si.exploreOrd
    output$plt.exploreGraphics <- shiny::renderPlot(
      plt$scatterPlot(data = filteredData$rawData,
                          abs = filteredData$abscissa,
                          ord = filteredData$ordinate),
      height = 400)
  })
  ####################
  # model optimization
  ####################
  shiny::observeEvent(input$ab.modelOptimization,{
    if(dataLoaded()){
      optimizer$optimize(filteredData$rawData)
      output$tbl.optimizedTypes <- DT::renderDataTable(
        optimizer$optTypes %>%
          DT::datatable(options = list(
            scrollX = TRUE,
            scrollY = '350px',
            paging = FALSE
          ))
      )
      output$tbl.optimizedValues <- DT::renderDataTable(
        optimizer$optParameter %>%
          DT::datatable(options = list(
            scrollX = TRUE,
            scrollY = '350px',
            paging = FALSE
          ))
      )
    }
  })
  #####################
  # user based modeling
  #####################
  shiny::observeEvent(input$ab.modelApply,{
    if(dataLoaded()){
      transformator$setTrafoType(feature = input$si.transformationFeature,
                                 type = input$si.transformationType)
      transformator$setMirrorLogic(feature = input$si.transformationFeature,
                                   logic = input$cb.mirrorLogic)
      filteredData$rawData %>%
        transformator$estimateTrafoParameter()
      transformedData$setRawData <- filteredData$rawData %>%
        transformator$mutateData()
      print(transformator)
      
    }
  })
  ##############
  # observe Tabs
  ##############
  shiny::observeEvent(input$menue, {
    if (input$menue == "tab_filter"){
      if (!dataLoaded()){
        errorMessage <- sprintf("No data loaded.", inFile$suffix)
        shiny::showNotification(paste(errorMessage),type = "error", duration = 10)
        output$tbl.filterStatistics <- DT::renderDataTable(NULL)
        output$tbl.filterMissings <- DT::renderDataTable(NULL)
        output$tbl.filterData <- DT::renderDataTable(NULL)
      }
    }
    if (input$menue == "tab_explore"){
      if(dataLoaded()){
        filteredData$setRawData <- rawData$rawData %>%
          filterSet$filter()
        updateSelectInput(session, "si.exploreAbs", choices = filteredData$featureNames)
        updateSelectInput(session, "si.exploreAbs", selected = filteredData$featureNames[1])
        updateSelectInput(session, "si.exploreOrd", choices = filteredData$featureNames)
        updateSelectInput(session, "si.exploreOrd", selected = filteredData$featureNames[1])
        output$tbl.exploreStatistics <-
          DT::renderDataTable({
            filteredData$dataStatistics() %>%
              format.data.frame(scientific = TRUE, digits = 4) %>%
              DT::datatable(options = list(
                scrollX = TRUE,
                scrollY = '350px',
                paging = FALSE
              ))
          })
      }
      else{
        output$tbl.exploreStatistics <- DT::renderDataTable(NULL)
        output$plt.exploreGraphics <- shiny::renderPlot(NULL)
        errorMessage <- sprintf("No data loaded.", inFile$suffix)
        shiny::showNotification(paste(errorMessage),type = "error", duration = 10)
      }
    }
    if (input$menue == "tab_model"){
      if(dataLoaded()){
        filteredData$setRawData <- rawData$rawData %>%
          filterSet$filter()
        transformator$resetTrafoParameter(data = filteredData$rawData)
        model$resetModel(data = filteredData$rawData)
        optimizer$resetOptimizer(data =filteredData$rawData)
        
        shiny::updateSelectInput(session, "si.transformationFeature",
                                 choices = filteredData$numericFeatureNames,
                                 selected = filteredData$numericFeatureNames[1])
        shiny::updateSelectInput(session, "si.transformationType",
                                 choices = transformator$trafoAlphabet,
                                 selected = transformator$trafoType(feature = filteredData$numericFeatureNames[1]))
        shiny::updateCheckboxInput(session, "cd.mirrorLogic",
                                   value = transformator$mirrorLogic(feature = filteredData$numericFeatureNames[1]))
      }
      else{
        output$tbl.modelOptimized <- DT::renderDataTable(NULL)
        output$tbl.featureTransformFit <- DT::renderDataTable(NULL)
        output$tbl.featureTransformTest <- DT::renderDataTable(NULL)
        output$tbl.transformationParameter <- DT::renderDataTable(NULL)
        output$tbl.modelParameter <- DT::renderDataTable(NULL)
        output$tbl.modelQuality <- DT::renderDataTable(NULL)
        output$tbl.testResults <- DT::renderDataTable(NULL)
        output$plt.featureTransformFit <- shiny::renderPlot(NULL)
        errorMessage <- sprintf("No data loaded.", inFile$suffix)
        shiny::showNotification(paste(errorMessage),type = "error", duration = 10)
      }
    }
  })
})



# chooseFeatureSummaryPlot <- function(obj = "pgu.data", choice="character"){
#   if (grepl("Histogram", choice)){
#     p <- obj$histFeatureOverview()
#   }
#   else if(grepl("Box Plot", choice)){
#     p <- obj$boxPlotOverview()
#   }
#   return(p)
# }
# 
# chooseFeatureNanTable <- function(obj = "pgu.data", choice="character"){
#   if (grepl("Statistics", choice)){
#     t <- obj$nanDistribution()
#   }
#   else if (grepl("Details", choice)){
#     t <- obj$nanPositives()
#   }
#   return(t)
# }
# 
# chooseFeatureOutlierTable <- function(obj ="pgu.data", optimizer = "pgu.optimizer", choice = "character"){
#   if (grepl("Statistics", choice)){
#     t <- optimizer$printOutlierStatistics()
#   }
#   else if (grepl("Details", choice)){
#     t <- optimizer$printOutlierSummary(obj = obj)
#   }
#   return(t)
# }
# 
# chooseFeatureOutlierPlot <- function(obj = "pgu.data", optimizer = "pgu.optimizer", choice = "character"){
#   if (grepl("Heatmap", choice)){
#     p <- optimizer$plotOutlierHeatmap(obj = obj)
#   }
#   else if (grepl("Histogram", choice)){
#     p <- optimizer$plotOutlierHistogram()
#   }
#   return(p)
# }
# 
# chooseCorrelationMatrixTable <- function(regObj = "pgu.regressor", corObj = "pgu.correlator", choice = "character"){
#   switch(choice,
#          Intercept = {return(regObj$printInterceptTbl())},
#          Slope = {return(regObj$printSlopeTbl())},
#          p.regression = {return(regObj$printPValueTbl())},
#          Rho = {return(corObj$printCoefficientTbl())},
#          p.correlation = {return(corObj$printPValueTbl())}
#          )
#   return(t)
# }
# 
# 
# shinyServer(function(input, output, session) {
#   ######################
#   # init reactive values
#   ######################
#   dataLoaded <- reactiveVal(value = FALSE)
#   importer <- reactiveVal(pgu.importer$new())
#   exporter <- reactiveVal(pgu.exporter$new())
# 
#   rawData <- reactiveVal()
#   filteredData <- reactiveVal()
#   tfData <- reactiveVal()
#   cleanedData <- reactiveVal()
# 
#   filterSet <- reactiveVal()
#   transformator <- reactiveVal()
#   normalizer <- reactiveVal()
#   cleaner <- reactiveVal()
#   optimizer <- reactiveVal()
#   regressor <- reactiveVal()
#   correlator <- reactiveVal()
#   hdf <- reactiveVal()
#   #filter.rowIdx <- reactiveVal()
#   #filter.colIdx <- reactiveVal()
#   filter.proxy <- reactiveVal()
#   errorMessage <- reactiveVal()
# 
#   ###############
#   # sidebar menue
#   ###############
#   shiny::observeEvent(input$menue,{
#     if (input$menue == "tab_import"){
#       shiny::observeEvent(input$fi.import,{
#         #################
#         # update importer
#         #################
#         importer()$setSheetIndex(input$ni.import)
#         importer()$setSeparator(input$rb.separator)
#         importer()$setHeader(input$cb.header)
#         importer()$setSkipRows(input$ni.skipRows)
#         importer()$setUploadFileName(name = input$fi.import$datapath)
#         importer()$setFileName(name = input$fi.import$name)
#         importer()$splitFileName()
#         importer()$extractFolderName()
#         importer()$setContent(value = "Raw")
#         importer()$createOutFileName()
#         if(importer()$fileTypeIsValid()){
#           ###########
#           # load data
#           ###########
#           tryCatch({
#             rawData(pgu.data$new(importer()$importFile()))
#             dataLoaded(TRUE)
#           },
#           error = function(e){
#             dataLoaded(FALSE)
#             shiny::showNotification(e,
#                                     duration = 10,
#                                     type = "error",
#                                     closeButton = TRUE)})
#           ###########################
#           # init all object instances
#           ###########################
#           # werden überhaupt weitere Datenframe benötigt?
#           filteredData(pgu.data$new(rawData()$getRawData()))
#           tfData(pgu.data$new(rawData()$getRawData()))
#           cleanedData(pgu.data$new(rawData()$rawData))
# 
#           filterSet(pgu.filter$new(obj = rawData()))
#           transformator(pgu.transformator$new(obj = rawData()))
#           normalizer(pgu.normalizer$new(obj = rawData()))
#           # cleaner(pgu.cleaner$new(dataObj = rawData(), modelObj = normalizer()))
#           # optimizer(pgu.optimizer$new(dataObj = rawData(), modelObj = normalizer()))
#           cleaner(pgu.cleaner$new(obj = rawData()))
#           # can fail!!!
#           optimizer(pgu.optimizer$new(obj = rawData()))
#           ########################
#           # update all ui elements
#           ########################
#           # update import elements
#           output$tbl.import <- DT::renderDataTable({rawData()$getRawData() %>%
#               format.data.frame(scientific = TRUE, digits = 4) %>%
#               DT::datatable(options = list(scrollX = TRUE, scrollY='350px', paging=FALSE))})
#           # update explore elements
#           updateSelectInput(session, "si.byFeature", choices= (rawData()$getFeatureNames()))
#           updateSelectInput(session, "si.byFeature", selected = rawData()$getFeatureNames()[1])
#           updateSelectInput(session, "si.detailedAbs", choices= (rawData()$getFeatureNames()))
#           updateSelectInput(session, "si.detailedAbs", selected = rawData()$getFeatureNames()[1])
#           updateSelectInput(session, "si.detailedOrd", choices= (rawData()$getFeatureNames()))
#           updateSelectInput(session, "si.detailedOrd", selected = rawData()$getFeatureNames()[1])
#           # update model elements
#           updateSelectInput(session, "si.transformationFeature", choices = (rawData()$getNumericFeatureNames()))
#           updateSelectInput(session, "si.transformationFeature", selected = rawData()$getNumericFeatureNames()[1])
#           updateSelectInput(session, "si.transformationType", choices = (transformator()$getTrafoAlphabet()))
#           updateSelectInput(session, "si.transformationType", selected = transformator()$getTrafoAlphabet()[1])
#           # update tidy elements
#           updateSelectInput(session, "si.nanHandleMethod", choices = (cleaner()$getCleaningAgentAlphabet()))
#           updateSelectInput(session, "si.nanHandleMethod", selected = (cleaner()$getCleaningAgentAlphabet()[1]))
#           updateNumericInput(session, "ni.nanSeed", value = cleaner()$getSeed())
#           updateSelectInput(session, "si.nanHandleFeature", choices = (rawData()$getNumericFeatureNames()))
#           updateSelectInput(session, "si.nanHandleFeature", selected = rawData()$getNumericFeatureNames()[1])
#           # update revise elements
#           updateSelectInput(session, "si.outlierHandleMethod", choices = (optimizer()$getCleaningAgentAlphabet()))
#           updateSelectInput(session, "si.outlierHandleMethod", selected = (optimizer()$getCleaningAgentAlphabet()[1]))
#           updateNumericInput(session, "ni.outlierSeed", value = optimizer()$seed)
#           updateSelectInput(session, "si.outlierHandleFeature", choices = (rawData()$getNumericFeatureNames()))
#           updateSelectInput(session, "si.outlierHandleFeature", selected = rawData()$getNumericFeatureNames()[1])
#           # update correlation elements
#           updateSelectInput(session, "si.regressionAbs", choices = (rawData()$getNumericFeatureNames()))
#           updateSelectInput(session, "si.regressionAbs", selected = (rawData()$getNumericFeatureNames()[1]))
#           updateSelectInput(session, "si.regressionOrd", choices = (rawData()$getNumericFeatureNames()))
#           updateSelectInput(session, "si.regressionOrd", selected = (rawData()$getNumericFeatureNames()[1]))
#           # update overview elements
#           shiny::updateSelectInput(session, "si.overviewContView", choices = (rawData()$getFeatureNames()))
#           shiny::updateSelectInput(session, "si.overviewContView", selected = rawData()$getFeatureNames()[1])
#           # update export elements
#           shiny::updateSelectInput(session, "si.exportType", choices = (exporter()$getContentList()))
#           shiny::updateSelectInput(session, "si.exportType", selected = exporter()$getContentList()[1])
#           shiny::updateSelectInput(session, "si.exportFormat", choices = exporter()$getIndividualSuffixList())
#           shiny::updateSelectInput(session, "si.exportFormat", choices = exporter()$getIndividualSuffixList()[1])
#           ###################
#           # update filter tab
#           ###################
#           output$tbl.filterData <- DT::renderDataTable(
#             rawData()$getRawData(),
#             selection = list(target = "column"),
#             rownames = FALSE,
#             options = list(scrollX = TRUE, scrollY='350px', paging=FALSE, autoWidth = TRUE, columnDefs = list(list(width = '75px', targets = "_all"))),
#             filter = "top")
#           filter.proxy(DT::dataTableProxy("tbl.filterData"))
#           ##################
#           # initial analysis
#           ##################
#           normalizer()$testDataForNormality(obj = tfData())
#         }
#         else{
#           dataLoaded(FALSE)
#           if(length(importer()$getSuffix())>0){
#             errorMessage(sprintf("unsupported file type: %s", importer()$getSuffix()))
#           }
#           else{
#             errorMessage(sprintf("No data loaded. Import valid file."))
#           }
#           shiny::showNotification(errorMessage(),
#                                   duration = 10,
#                                   type = "error",
#                                   closeButton = TRUE)
# 
#         }
#       })
#     }
#     else if (input$menue == "tab_filter"){
#       if(importer()$fileTypeIsValid()){
#         filter.proxy() %>%
#           DT::reloadData(clearSelection = "none")
# 
#         shiny::observeEvent(input$btn.filterReset, {
#           rawData() %>%
#             filterSet()$initFilterIdx()
#           filter.proxy() %>%
#             DT::clearSearch()
#           filter.proxy() %>%
#             DT::reloadData(clearSelection = "all")
#           rawData() %>%
#             filterSet()$filterData() %>%
#             transformator()$update()
#           rawData() %>%
#             filterSet()$filterData() %>%
#             normalizer()$update()
#           ########################
#           # update all ui elements
#           ########################
#           shiny::updateSelectInput(session, "si.detailedAbs", choices= (filterSet()$getFeatureNames()))
#           shiny::updateSelectInput(session, "si.detailedAbs", selected = filterSet()$getFeatureNames()[1])
#           shiny::updateSelectInput(session, "si.detailedOrd", choices= (filterSet()$getFeatureNames()))
#           shiny::updateSelectInput(session, "si.detailedOrd", selected = filterSet()$getFeatureNames()[1])
# 
#           shiny::updateSelectInput(session, "si.transformationFeature", choices = (filterSet()$getNumericFeatureNames()))
#           shiny::updateSelectInput(session, "si.transformationFeature", selected = filterSet()$getNumericFeatureNames()[1])
#           shiny::updateSelectInput(session, "si.transformationType", choices = (transformator()$getTrafoAlphabet()))
#           shiny::updateSelectInput(session, "si.transformationType", selected = transformator()$getFeatureTrafoType(feature = filterSet()$getNumericFeatureNames()[1]))
# 
#           shiny::updateSelectInput(session, "si.nanHandleFeature", choices = (filterSet()$getNumericFeatureNames()))
#           shiny::updateSelectInput(session, "si.nanHandleFeature", selected = filterSet()$getNumericFeatureNames()[1])
# 
#           shiny::updateSelectInput(session, "si.outlierHandleFeature", choices = (filterSet()$getNumericFeatureNames()))
#           shiny::updateSelectInput(session, "si.outlierHandleFeature", selected = filterSet()$getNumericFeatureNames()[1])
# 
#           updateSelectInput(session, "si.regressionAbs", choices = (filterSet()$getNumericFeatureNames()))
#           updateSelectInput(session, "si.regressionAbs", selected = (filterSet()$getNumericFeatureNames()[1]))
#           updateSelectInput(session, "si.regressionOrd", choices = (filterSet()$getNumericFeatureNames()))
#           updateSelectInput(session, "si.regressionOrd", selected = (filterSet()$getNumericFeatureNames()[1]))
#         })
# 
#         shiny::observeEvent(input$btn.filterApply, {
#           if (length(input$tbl.filterData_rows_all) < 1){
#             filterSet()$initFilterRowIdx(obj = rawData())
#           }
#           else {
#             filterSet()$setFilterRowIdx(idx = input$tbl.filterData_rows_all)
#           }
#           if (length(input$tbl.filterData_columns_selected) <1){
#             filterSet()$initFilterColIdx(obj = rawData())
#           }
#           else{
#             filterSet()$setFilterColIdx(idx = input$tbl.filterData_columns_selected+1)
#           }
#           rawData() %>%
#             filterSet()$filterData() %>%
#             transformator()$update()
#           rawData() %>%
#             filterSet()$filterData() %>%
#             normalizer()$update()
#           ########################
#           # update all ui elements
#           ########################
#           updateSelectInput(session, "si.detailedAbs", choices= (filterSet()$getFeatureNames()))
#           updateSelectInput(session, "si.detailedAbs", selected = filterSet()$getFeatureNames()[1])
#           updateSelectInput(session, "si.detailedOrd", choices= (filterSet()$getFeatureNames()))
#           updateSelectInput(session, "si.detailedOrd", selected = filterSet()$getFeatureNames()[1])
# 
#           updateSelectInput(session, "si.transformationFeature", choices = (filterSet()$getNumericFeatureNames()))
#           updateSelectInput(session, "si.transformationFeature", selected = filterSet()$getNumericFeatureNames()[1])
#           updateSelectInput(session, "si.transformationType", choices = (transformator()$getTrafoAlphabet()))
#           updateSelectInput(session, "si.transformationType", selected = transformator()$getFeatureTrafoType(feature = filterSet()$getFeatureNames()[1]))
# 
#           shiny::updateSelectInput(session, "si.nanHandleFeature", choices = (filterSet()$getNumericFeatureNames()))
#           shiny::updateSelectInput(session, "si.nanHandleFeature", selected = filterSet()$getNumericFeatureNames()[1])
# 
#           shiny::updateSelectInput(session, "si.outlierHandleFeature", choices = (filterSet()$getNumericFeatureNames()))
#           shiny::updateSelectInput(session, "si.outlierHandleFeature", selected = filterSet()$getNumericFeatureNames()[1])
# 
#           updateSelectInput(session, "si.regressionAbs", choices = (filterSet()$getNumericFeatureNames()))
#           updateSelectInput(session, "si.regressionAbs", selected = (filterSet()$getNumericFeatureNames()[1]))
#           updateSelectInput(session, "si.regressionOrd", choices = (filterSet()$getNumericFeatureNames()))
#           updateSelectInput(session, "si.regressionOrd", selected = (filterSet()$getNumericFeatureNames()[1]))
#         })
#       }
#       else {
#         errorMessage(sprintf("No data loaded. Import a valid file."))
#         shiny::showNotification(errorMessage(),
#                                 duration = 10,
#                                 type = "error",
#                                 closeButton = TRUE)
#       }
#     }
#     else if (input$menue == "tab_explore"){
#       if(importer()$fileTypeIsValid()){
#         ########################
#         # update data
#         ########################
#         filteredData(rawData() %>%
#                        filterSet()$filterData())
#         ########################
#         # calculate
#         ########################
#         shiny::observeEvent(input$tabsetExplore, {
#           switch(input$tabsetExplore,
#                  Graphics = {output$plt.exploreGraphics <- renderPlot({filteredData()$plotFeature(abscissae = input$si.detailedAbs, ordinate = input$si.detailedOrd)}, height = 400)},
#                  Statistics = {output$tbl.exploreStatistics <- DT::renderDataTable({filteredData()$dataStatistics() %>%
#                      format.data.frame(scientific = TRUE, digits = 4) %>%
#                      DT::datatable(options = list(scrollX = TRUE, scrollY='350px', paging=FALSE))})})})
#       }
#       else{
#         errorMessage(sprintf("No data loaded. Import a valid file."))
#         shiny::showNotification(errorMessage(),
#                                 duration = 10,
#                                 type = "error",
#                                 closeButton = TRUE)
#       }
#     }
#     else if (input$menue == "tab_model"){
#       if(importer()$fileTypeIsValid()){
#         ########################
#         # update data
#         ########################
#         filteredData(rawData() %>%
#                        filterSet()$filterData())
#         tfData(filteredData() %>%
#                  transformator()$model())
#         tfData() %>%
#           normalizer()$testDataForNormality()
#         shiny::observeEvent(input$tabsetModel, {
#           switch(input$tabsetModel,
#                  "Transformation" = {
#                    shiny::observeEvent(input$si.transformationFeature,{
#                      # update ui
#                      updateSelectInput(session, "si.transformationType",
#                                        selected = transformator()$getFeatureTrafoType(feature = input$si.transformationFeature))
#                      updateCheckboxInput(session, "cb.mirrorLogic",
#                                          value = transformator()$getFeatureMirrorLogic(feature = input$si.transformationFeature))
#                      # run analysis
#                      tfData() %>%
#                        normalizer()$testFeatureForNormality(feature = input$si.transformationFeature)
#                      # log result to screen
#                      output$tbl.featureTransformFit <- DT::renderDataTable({normalizer()$featureFitResult(feature = input$si.transformationFeature) %>%
#                          format.data.frame(scientific = TRUE, digits = 4) %>%
#                          DT::datatable(options = list(scrollX = TRUE, scrollY='350px', paging=FALSE))})
#                      output$tbl.featureTransformTest <- DT::renderDataTable({normalizer()$featureTestResult(feature = input$si.transformationFeature) %>%
#                          format.data.frame(scientific = TRUE, digits = 4) %>%
#                          DT::datatable(options = list(scrollX = TRUE, scrollY='350px', paging=FALSE))})
#                      output$plt.featureTransformFit <- renderPlot(normalizer()$plotFeatureFitResult(feature = input$si.transformationFeature))
#                    })
# 
#                    shiny::observeEvent(input$si.transformationType,{
#                      if(input$si.transformationType != transformator()$getFeatureTrafoType(feature = input$si.transformationFeature)){
#                        # run analysis
#                        transformator()$setFeatureTrafoType(feature = input$si.transformationFeature, type = input$si.transformationType)
#                        filteredData() %>%
#                          transformator()$optimize(feature = input$si.transformationFeature)
#                        tfData(filteredData() %>%
#                                 transformator()$model())
# 
#                        if(!tfData() %>%
#                           normalizer()$testFeatureForNormality(feature = input$si.transformationFeature)){
#                          errorMessage(sprintf("Error during Maximum Likelihood Estimation. Choose different model."))
#                          shiny::showNotification(errorMessage(),
#                                                  duration = 10,
#                                                  type = "error",
#                                                  closeButton = TRUE)
#                          updateSelectInput(session, "si.transformationType", selected = transformator()$getTrafoAlphabet()[1])
#                        }
#                        # fit result success logical einführen
#                        # if false set transformValue to none!
# 
#                        # log result to screen
#                        output$tbl.featureTransformFit <- DT::renderDataTable({normalizer()$featureFitResult(feature = input$si.transformationFeature) %>%
#                            format.data.frame(scientific = TRUE, digits = 4) %>%
#                            DT::datatable(options = list(scrollX = TRUE, scrollY='350px', paging=FALSE))})
#                        output$tbl.featureTransformTest <- DT::renderDataTable({normalizer()$featureTestResult(feature = input$si.transformationFeature) %>%
#                            format.data.frame(scientific = TRUE, digits = 4) %>%
#                            DT::datatable(options = list(scrollX = TRUE, scrollY='350px', paging=FALSE))})
#                        output$plt.featureTransformFit <- renderPlot({normalizer()$plotFeatureFitResult(feature = input$si.transformationFeature)})
#                      }
#                    })
#                    shiny::observeEvent(input$cb.mirrorLogic, {
#                      transformator()$setFeatureMirrorLogic(feature = input$si.transformationFeature, logic = input$cb.mirrorLogic)
#                      filteredData() %>%
#                        transformator()$estimateAddConst()
#                      #print(transformator()$getTrafoPara())
#                      filteredData() %>%
#                        transformator()$optimize(feature = input$si.transformationFeature)
#                      tfData(filteredData() %>%
#                               transformator()$model())
#                      if(!tfData() %>%
#                         normalizer()$testFeatureForNormality(feature = input$si.transformationFeature)){
#                        errorMessage(sprintf("Error during Maximum Likelihood Estimation. Choose different model."))
#                        shiny::showNotification(errorMessage(),
#                                                duration = 10,
#                                                type = "error",
#                                                closeButton = TRUE)
#                        if(input$cb.mirrorLogic){
#                          shiny::updateCheckboxInput(session, "cb.mirrorLogic", value = FALSE)
#                        }
#                        else{
#                          shiny::updateCheckboxInput(session, "cb.mirrorLogic", value = TRUE)
#                        }
#                      }
# 
#                      # log result to screen
#                      output$tbl.featureTransformFit <- DT::renderDataTable({normalizer()$featureFitResult(feature = input$si.transformationFeature) %>%
#                          format.data.frame(scientific = TRUE, digits = 4) %>%
#                          DT::datatable(options = list(scrollX = TRUE, scrollY='350px', paging=FALSE))})
#                      output$tbl.featureTransformTest <- DT::renderDataTable({normalizer()$featureTestResult(feature = input$si.transformationFeature) %>%
#                          format.data.frame(scientific = TRUE, digits = 4) %>%
#                          DT::datatable(options = list(scrollX = TRUE, scrollY='350px', paging=FALSE))})
#                      output$plt.featureTransformFit <- renderPlot({normalizer()$plotFeatureFitResult(feature = input$si.transformationFeature)})
#                    })
#                  },
#                  "Transformation Parameter" = {
#                    output$tbl.transformationParameter <- DT::renderDataTable(transformator()$getTrafoPara() %>%
#                                                                                format.data.frame(scientific = TRUE, digits = 4),
#                                                                              options = list(scrollX = TRUE, scrollY='350px', paging=FALSE))},
#                  "Model Parameter" = {
#                    output$tbl.modelParameter <- DT::renderDataTable(normalizer()$modelParameter %>%
#                                                                       dplyr::select(features, expMu:expSigma) %>%
#                                                                       format.data.frame(scientific = TRUE, digits = 4),
#                                                                     options = list(scrollX = TRUE, scrollY='350px', paging=FALSE))},
#                  "Model Quality" = {
#                    output$tbl.modelQuality <- DT::renderDataTable(normalizer()$modelParameter %>%
#                                                                     dplyr::select(features, dataPoints:rmse) %>%
#                                                                     format.data.frame(scientific = TRUE, digits = 4),
#                                                                   options = list(scrollX = TRUE, scrollY='350px', paging=FALSE))},
#                  "Test Results" = {
#                    output$tbl.testResults <- DT::renderDataTable(normalizer()$modelParameter %>%
#                                                                    dplyr::select(features, w.shapiro:p.anderson) %>%
#                                                                    format.data.frame(scientific = TRUE, digits = 4),
#                                                                  options = list(scrollX = TRUE, scrollY='350px', paging=FALSE))})})
#       }
#       else{
#         errorMessage(sprintf("No data loaded. Import a valid file."))
#         shiny::showNotification(errorMessage(),
#                                 duration = 10,
#                                 type = "error",
#                                 closeButton = TRUE)
#       }
#     }
#     else if (input$menue == "tab_tidy"){
#       if(importer()$fileTypeIsValid()){
#         ########################
#         # update data
#         ########################
#         filteredData(rawData() %>%
#                        filterSet()$filterData())
#         tfData(filteredData() %>%
#                  transformator()$model() %>%
#                  normalizer()$scaleData())
# 
#         tfData()$update()
# 
#         # cleaner()$update(dataObj = tfData(), modelObj = normalizer())
#         cleaner()$update(obj = tfData())
#         cleaner()$findCandidates(obj = tfData())
#         cleaner()$setSeed(value = input$ni.nanSeed)
#         cleanedData(tfData() %>%
#                       cleaner()$cleanData() %>%
#                       normalizer()$inverseScaleData() %>%
#                       transformator()$inverseModel())
#         cleanedData()$update()
# 
#         shiny::observeEvent(input$tabsetTidy,{
#           switch(input$tabsetTidy,
#                  Summary = {
#                    output$plt.nanSummary <- renderPlot({tfData()$nanHeatmap()})
#                    output$tbl.nanSummary <- DT::renderDataTable({tfData() %>%
#                        transformator()$inverseTransformData() %>%
#                        chooseFeatureNanTable(choice = input$si.nanSummary) %>%
#                        DT::datatable(options = list(scrollX = TRUE, scrollY='350px', paging=FALSE))})
#                  },
#                  Tidy = {
#                    shiny::updateSelectInput(session, "si.nanHandleMethod", selected = (cleaner()$getCleaningAgent()))
#                    shiny::updateNumericInput(session, "ni.nanSeed", value=cleaner()$getSeed())
# 
#                    shiny::observeEvent(input$ni.nanSeed,{
#                      cleaner()$setSeed(value = input$ni.nanSeed)
#                      cleanedData(tfData() %>%
#                                    cleaner()$cleanData() %>%
#                                    normalizer()$inverseScaleData() %>%
#                                    transformator()$inverseModel())
#                      cleanedData()$update()
#                      output$plt.nanCleaningSummary <- renderPlot({cleaner()$featurePlot(obj = cleanedData(), feature = input$si.nanHandleFeature)})
#                      output$tbl.nanCleaningSummary <- DT::renderDataTable({cleanedData()$printSubset(indices = cleaner()$candidateIndices()) %>%
#                          format.data.frame(scientific = TRUE, digits = 4) %>%
#                          DT::datatable(options = list(scrollX = TRUE, scrollY='350px', paging=FALSE))
#                      })
#                    })
#                    shiny::observeEvent(input$si.nanHandleMethod,{
#                      cleaner()$setCleaningAgent(value = input$si.nanHandleMethod)
#                      cleanedData(tfData() %>%
#                                    cleaner()$cleanData() %>%
#                                    normalizer()$inverseScaleData() %>%
#                                    transformator()$inverseModel())
#                      cleanedData()$update()
#                      output$plt.nanCleaningSummary <- renderPlot({cleaner()$featurePlot(obj = cleanedData(), feature = input$si.nanHandleFeature)})
#                      output$tbl.nanCleaningSummary <- DT::renderDataTable({cleanedData()$printSubset(indices = cleaner()$candidateIndices()) %>%
#                          format.data.frame(scientific = TRUE, digits = 4) %>%
#                          DT::datatable(options = list(scrollX = TRUE, scrollY='350px', paging=FALSE))})
#                    })
#                  })
#         })
#       }
#       else{
#         errorMessage(sprintf("No data loaded. Import a valid file."))
#         shiny::showNotification(errorMessage(),
#                                 duration = 10,
#                                 type = "error",
#                                 closeButton = TRUE)
#       }
#     }
#     else if (input$menue == "tab_revise"){
#       if(importer()$fileTypeIsValid()){
#         ########################
#         # update data
#         ########################
#         filteredData(rawData() %>%
#                        filterSet()$filterData())
# 
#         tfData(filteredData() %>%
#                  transformator()$model() %>%
#                  normalizer()$scaleData())
#         tfData()$update()
# 
#         #optimizer()$update(dataObj = tfData(), modelObj = normalizer())
#         optimizer()$update(obj = tfData())
#         optimizer()$findCandidates(obj = tfData())
#         optimizer()$setSeed(value = input$ni.outlierSeed)
#         cleanedData(tfData() %>%
#                       optimizer()$cleanData() %>%
#                       normalizer()$inverseScaleData() %>%
#                       transformator()$inverseModel())
#         cleanedData()$update()
# 
#         shiny::observeEvent(input$tabsetRevise,{
#           switch(input$tabsetRevise,
#                  "Summary" = {
#                    output$tbl.outlierSummary <- DT::renderDataTable({chooseFeatureOutlierTable(obj = tfData(), optimizer = optimizer(), choice = input$si.outlierSummaryTbl)})
#                    output$plt.outlierHist <- renderPlot({chooseFeatureOutlierPlot(obj = tfData(), optimizer = optimizer(), choice = input$si.outlierSummaryPlt)})},
#                  "Revise" = {
#                    shiny::updateSelectInput(session, "si.outlierHandleMethod", selected = (optimizer()$cleaningAgent))
#                    shiny::updateNumericInput(session, "ni.outlierSeed", value=optimizer()$seed)
#                    shiny::observeEvent(input$ni.outlierSeed,{
#                      optimizer()$setSeed(value = input$ni.outlierSeed)
#                      cleanedData(tfData() %>%
#                                    optimizer()$cleanData() %>%
#                                    normalizer()$inverseScaleData() %>%
#                                    transformator()$inverseModel())
#                      cleanedData()$update()
#                      output$plt.outlierCleaningSummary <- renderPlot({optimizer()$featurePlot(obj = cleanedData(), feature = input$si.outlierHandleFeature)})
#                      output$tbl.outlierCleaningSummary <- DT::renderDataTable({cleanedData()$printSubset(indices = optimizer()$candidateIndices()) %>%
#                          format.data.frame(scientific = TRUE, digits = 4) %>%
#                          DT::datatable(options = list(scrollX = TRUE, scrollY='350px', paging=FALSE))})
#                    })
#                    shiny::observeEvent(input$si.outlierHandleMethod,{
#                      optimizer()$setCleaningAgent(value = input$si.outlierHandleMethod)
#                      cleanedData(tfData() %>%
#                                    optimizer()$cleanData() %>%
#                                    normalizer()$inverseScaleData() %>%
#                                    transformator()$inverseModel())
#                      cleanedData()$update()
#                      output$plt.outlierCleaningSummary <- renderPlot({optimizer()$featurePlot(obj = cleanedData(), feature = input$si.outlierHandleFeature)})
#                      output$tbl.outlierCleaningSummary <- DT::renderDataTable({cleanedData()$printSubset(indices = optimizer()$candidateIndices()) %>%
#                          format.data.frame(scientific = TRUE, digits = 4) %>%
#                          DT::datatable(options = list(scrollX = TRUE, scrollY='350px', paging=FALSE))})
#                    })
#                  })
#         })
#       }
#       else{
#         errorMessage(sprintf("No data loaded. Import a valid file."))
#         shiny::showNotification(errorMessage(),
#                                 duration = 10,
#                                 type = "error",
#                                 closeButton = TRUE)
#       }
#     }
#     else if (input$menue == "tab_correlate"){
#       if(importer()$fileTypeIsValid()){
#         ########################
#         # update data
#         ########################
#         filteredData(rawData() %>%
#                        filterSet()$filterData())
#         filteredData()$update()
#         tfData(filteredData() %>%
#                  transformator()$model())
#         tfData()$update()
#         cleanedData(tfData() %>%
#                       cleaner()$cleanData() %>%
#                       optimizer()$cleanData() %>%
#                       transformator()$inverseModel())
#         cleanedData()$update()
#         regressor(pgu.regressor$new(names = filterSet()$getNumericFeatureNames()))
#         regressor()$createModelMatrix(obj = cleanedData())
#         regressor()$setAbscissa(filterSet()$getNumericFeatureNames()[1])
#         regressor()$setOrdinate(filterSet()$getNumericFeatureNames()[2])
#         regressor()$createModel(obj = cleanedData())
#         correlator(pgu.correlator$new(names = filterSet()$getNumericFeatureNames(), method = "Spearman"))
#         correlator()$createCorrelationMatrix(obj = cleanedData())
#         ########################
#         # update all ui elements
#         ########################
#         updateSelectInput(session, "si.regressionAbs", choices = (regressor()$getFeatureNames()))
#         updateSelectInput(session, "si.regressionAbs", selected = (regressor()$getAbscissa()))
#         updateSelectInput(session, "si.regressionOrd", choices = (regressor()$getValidOrdinates()))
#         updateSelectInput(session, "si.regressionOrd", selected = (regressor()$getOrdinate()))
#         #calculate regression
#         shiny::observeEvent(input$si.regressionAbs, {
#           regressor()$setAbscissa(input$si.regressionAbs)
#           updateSelectInput(session, "si.regressionOrd", choices = (regressor()$getValidOrdinates()))
#           updateSelectInput(session, "si.regressionOrd", selected = (regressor()$getValidOrdinates()[1]))
#           regressor()$createModel(obj = cleanedData())
#           output$plt.regression <- renderPlot({regressor()$plotResult()})
#           output$tbl.individualRegression <- DT::renderDataTable({regressor()$printModel() %>%
#               merge(correlator()$printTestResult(abscissa = input$si.regressionAbs, ordinate = input$si.regressionOrd)) %>%
#               format.data.frame(scientific = TRUE, digits = 4) %>%
#               DT::datatable(options = list(scrollX = TRUE, scrollY=TRUE, paging=FALSE))})
#         })
# 
#         shiny::observeEvent(input$si.regressionOrd, {
#           regressor()$setOrdinate(input$si.regressionOrd)
#           regressor()$createModel(obj = cleanedData())
#           output$plt.regression <- renderPlot({regressor()$plotResult()})
#           output$tbl.individualRegression <- DT::renderDataTable({regressor()$printModel() %>%
#               merge(correlator()$printTestResult(abscissa = input$si.regressionAbs, ordinate = input$si.regressionOrd)) %>%
#               format.data.frame(scientific = TRUE, digits = 4) %>%
#               DT::datatable(options = list(scrollX = TRUE, scrollY=TRUE, paging=FALSE))})
#         })
#         shiny::observeEvent(input$si.correlationStat,{
#           output$tbl.correlationMatrix <- DT::renderDataTable({chooseCorrelationMatrixTable(regObj = regressor(), corObj = correlator(), choice = input$si.correlationStat) %>%
#               format.data.frame(scientific = TRUE, digits = 4) %>%
#               DT::datatable(options = list(scrollX = TRUE, scrollY=TRUE, paging=FALSE))})
# 
#         })
#       }
#       else{
#         errorMessage(sprintf("No data loaded. Import a valid file."))
#         shiny::showNotification(errorMessage(),
#                                 duration = 10,
#                                 type = "error",
#                                 closeButton = TRUE)
#       }
#     }
#     else if (input$menue == "tab_overview"){
#       if(importer()$fileTypeIsValid()){
#         ########################
#         # update data
#         ########################
#         filteredData(rawData() %>%
#                        filterSet()$filterData())
#         filteredData()$update()
#         tfData(filteredData() %>%
#                  transformator()$model() %>%
#                  normalizer()$scaleData())
#         tfData()$update()
#         cleanedData(tfData() %>%
#                       cleaner()$cleanData() %>%
#                       optimizer()$cleanData() %>%
#                       normalizer()$inverseScaleData() %>%
#                       transformator()$inverseModel())
#         cleanedData()$update()
#         ########################
#         # update all ui elements
#         ########################
#         shiny::updateSelectInput(session, "si.overviewContView", choices = (filteredData()$featureNames))
#         shiny::updateSelectInput(session, "si.overviewContView", selected = filteredData()$featureNames[1])
# 
#         shiny::observeEvent(input$tabsetOverview,{
#           switch(input$tabsetOverview,
#                  Data = {
#                    shiny::observeEvent(input$si.overviewDataData,{
#                      switch(input$si.overviewDataData,
#                             Raw = {output$tbl.overviewData <- DT::renderDataTable({filteredData()$getRawData() %>%
#                                 format.data.frame(scientific = TRUE, digits = 4) %>%
#                                 DT::datatable(options = list(scrollX = TRUE, scrollY='350px', paging=FALSE))})},
#                             Transformed = {output$tbl.overviewData <- DT::renderDataTable({tfData()$getRawData() %>%
#                                 format.data.frame(scientific = TRUE, digits = 4) %>%
#                                 DT::datatable(options = list(scrollX = TRUE, scrollY='350px', paging=FALSE))})},
#                             Tidy = {output$tbl.overviewData <- DT::renderDataTable({cleanedData()$getRawData() %>%
#                                 format.data.frame(scientific = TRUE, digits = 4) %>%
#                                 DT::datatable(options = list(scrollX = TRUE, scrollY='350px', paging=FALSE))})})})},
#                  Frequency = {
#                    shiny::observeEvent(input$si.overviewFreqData,{
#                      switch(input$si.overviewFreqData,
#                             Raw = {output$plt.overviewFreq <- renderPlot({chooseFeatureSummaryPlot(obj = filteredData(), choice=input$si.overviewFreqView)}, height = ceiling(filteredData()$numberOfFeatures()/5)*150)},
#                             Transformed = {output$plt.overviewFreq <- renderPlot({chooseFeatureSummaryPlot(obj = tfData(), choice=input$si.overviewFreqView)}, height = ceiling(tfData()$numberOfFeatures()/5)*150)},
#                             Tidy = {output$plt.overviewFreq <- renderPlot({chooseFeatureSummaryPlot(obj = cleanedData(), choice=input$si.overviewFreqView)}, height = ceiling(cleanedData()$numberOfFeatures()/5)*150)})})},
#                  Continuous = {
#                    shiny::observeEvent(input$si.overviewContData,{
#                      switch (input$si.overviewContData,
#                              Raw = {
#                                output$plt.overviewCont <- renderPlot({filteredData()$plotFeatureOverview(abscissae = input$si.overviewContView)}, height = ceiling(filteredData()$numberOfFeatures()/5)*150)},
#                              Transformed = {
#                                output$plt.overviewCont <- renderPlot({tfData()$plotFeatureOverview(abscissae = input$si.overviewContView)}, height = ceiling(tfData()$numberOfFeatures()/5)*150)},
#                              Tidy ={
#                                output$plt.overviewCont <- renderPlot({cleanedData()$plotFeatureOverview(abscissae = input$si.overviewContView)}, height = ceiling(cleanedData()$numberOfFeatures()/5)*150)})})},
#                  Statistics = {
#                    shiny::observeEvent(input$si.overviewStatisticsData,{
#                      switch(input$si.overviewStatisticsData,
#                             Raw = {output$tbl.overviewStatistics <- DT::renderDataTable({filteredData()$dataStatistics() %>%
#                                 format.data.frame(scientific = TRUE, digits = 4) %>%
#                                 DT::datatable(options = list(scrollX = TRUE, scrollY='350px', paging=FALSE))})},
#                             Transformed = {output$tbl.overviewStatistics <- DT::renderDataTable({tfData()$dataStatistics() %>%
#                                 format.data.frame(scientific = TRUE, digits = 4) %>%
#                                 DT::datatable(options = list(scrollX = TRUE, scrollY='350px', paging=FALSE))})},
#                             Tidy = {output$tbl.overviewStatistics <- DT::renderDataTable({cleanedData()$dataStatistics() %>%
#                                 format.data.frame(scientific = TRUE, digits = 4) %>%
#                                 DT::datatable(options = list(scrollX = TRUE, scrollY='350px', paging=FALSE))})}
#                      )})})})
#       }
#       else{
#         errorMessage(sprintf("No data loaded. Import a valid file."))
#         shiny::showNotification(errorMessage(),
#                                 duration = 10,
#                                 type = "error",
#                                 closeButton = TRUE)
#       }
#     }
#     else if (input$menue == "tab_export"){
#       if(importer()$fileTypeIsValid()){
#         ########################
#         # update data
#         ########################
#         filteredData(rawData() %>%
#                        filterSet()$filterData())
#         filteredData()$update()
#         tfData(filteredData() %>%
#                  transformator()$model() %>%
#                  normalizer()$scaleData() %>%
#                  cleaner()$cleanData() %>%
#                  optimizer()$cleanData())
#         tfData()$update()
#         cleanedData(tfData() %>%
#                       normalizer()$inverseScaleData() %>%
#                       transformator()$inverseModel())
#         cleanedData()$update()
#         #################
#         # update exporter
#         #################
#         exporter()$setUploadFileName(name = importer()$getUploadFileName())
#         exporter()$setFileName(name = importer()$getFileName())
#         exporter()$splitFileName()
#         exporter()$extractFolderName()
#         exporter()$setSuffix(value = input$si.exportFormat)
#         exporter()$setContent(value = input$si.exportType)
#         exporter()$createOutFileName()
#         ##################
#         # init hdf support
#         ##################
#         hdf(pgu.hdf5$new())
#         exporter()$setHdf(hdf())
# 
#         shiny::observeEvent(input$si.exportType,{
#           ########################
#           # update all ui elements
#           ########################
#           if (input$si.exportType == "Complete"){
#             shiny::updateSelectInput(session, "si.exportFormat", choices = exporter()$getComprehensiveSuffixList())
#           }
#           else{
#             shiny::updateSelectInput(session, "si.exportFormat", choices = exporter()$getIndividualSuffixList())
#           }
#           #shiny::updateSelectInput(session, "si.exportFormat", selected = "csv")
#           #################
#           # update exporter
#           #################
#           exporter()$setSuffix(value = input$si.exportFormat)
#           exporter()$setContent(value = input$si.exportType)
#           exporter()$createOutFileName()
#           #cat(exporter()$instanceSummary())
#         })
# 
#         shiny::observeEvent(input$si.exportFormat,{
#           #################
#           # update exporter
#           #################
#           exporter()$setSuffix(value = input$si.exportFormat)
#           exporter()$setContent(value = input$si.exportType)
#           exporter()$createOutFileName()
#           #cat(exporter()$instanceSummary())
#         })
# 
#         output$db.export <- shiny::downloadHandler(
#           filename = function(){exporter()$getOutFileName()},
#           content = function(file){
#             exporter()$setUploadFileName(file)
#             exporter()$export(obj = c(filteredData(),tfData(),cleanedData()), transformator = transformator(), model = normalizer(), cleaner = cleaner(), optimizer = optimizer())
#           }
#         )
#       }
#       else{
#         errorMessage(sprintf("No data loaded. Import a valid file."))
#         shiny::showNotification(errorMessage(),
#                                 duration = 10,
#                                 type = "error",
#                                 closeButton = TRUE)
#       }
#     }
#   })
# })
