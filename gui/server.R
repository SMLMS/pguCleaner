library(shiny)
library(shinydashboard)
library(tidyverse)
library(DT)
#library(promises)
#library(future)
#library(future.callr)
#plan(callr)
source(file = "../R/pguGlobals.R", local=TRUE)
source(file = "../R/pguFile.R", local=TRUE)
source(file = "../R/pguImporter.R", local=TRUE)
source(file = "../R/pguData.R", local=TRUE)
source(file = "../R/pguFilter.R", local=TRUE)
source(file = "../R/pguPlot.R", local=TRUE)
source(file = "../R/pguTransformator.R", local=TRUE)
source(file = "../R/pguModel.R", local=TRUE)
source(file = "../R/pguNormDist.R", local=TRUE)
source(file = "../R/pguOptimizer.R", local=TRUE)
source(file = "../R/pguMissings.R", local=TRUE)
source(file = "../R/pguOutliers.R", local=TRUE)
source(file = "../R/pguCorrelator.R", local=TRUE)
source(file = "../R/pguRegressor.R", local=TRUE)
source(file = "../R/pguExporter.R", local=TRUE)
# source(file = "../R/pgu_hdf5.R", local=TRUE)

shinyServer(function(input, output, session) {
  ######################
  # init reactive values
  ######################
  dataLoaded <- shiny::reactiveVal(value = FALSE)
  modelOptimized <- shiny::reactiveVal(value = FALSE)
  modelDefined <- shiny::reactiveVal(value = FALSE)
  nanCleaned <- shiny::reactiveVal(value = FALSE)
  outlierDetected <- shiny::reactiveVal(value = FALSE)
  outlierRevised <- shiny::reactiveVal(value = FALSE)
  dataCorrelated <- shiny::reactiveVal(value = FALSE)
  ############
  # dataFrames
  ############
  rawData <- pgu.data$new()
  filteredData <- pgu.data$new()
  transformedData <- pgu.data$new()
  scaledData <- pgu.data$new()
  cleanedData <- pgu.data$new()
  
  inFile <- pgu.file$new()
  outFile <- pgu.file$new()
  importer <- pgu.importer$new()
  filterSet <- pgu.filter$new()
  plt <- pgu.plot$new()
  transformator <- pgu.transformator$new()
  model <- pgu.model$new()
  featureModel <- pgu.normDist$new()
  optimizer <- pgu.optimizer$new()
  missings <- pgu.missings$new()
  outliers <- pgu.outliers$new()
  correlator <- pgu.correlator$new()
  regressor <- pgu.regressor$new()
  exporter <- pgu.exporter$new()
  
  
  ###############
  # import button
  ###############
  shiny::observeEvent(input$ab.import,{
    modelOptimized(FALSE)
    if (length(input$fi.import$datapath) > 0){
      inFile$setUploadFileName <- input$fi.import$datapath
      inFile$setFileName <- input$fi.import$name
      inFile$splitFileName()
      inFile$setSheetIndex <- input$ni.import
      inFile$setSkipRows <- input$ni.skipRows
      inFile$setSeparator <-input$rb.separator
      inFile$setHeader <- input$cb.header
      outFile$setBaseName <- inFile$baseName
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
        modelOptimized(FALSE)
        modelDefined(FALSE)
        nanCleaned(FALSE)
        outlierDetected(FALSE)
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
      modelOptimized(FALSE)
      modelDefined(FALSE)
      nanCleaned(FALSE)
      outlierDetected(FALSE)
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
      modelOptimized(FALSE)
      modelDefined(FALSE)
      nanCleaned(FALSE)
      outlierDetected(FALSE)
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
  shiny::observeEvent(input$ab.wizardOptimize,{
    if(dataLoaded()){
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
      filteredData$setRawData <- rawData$rawData %>%
        filterSet$filter()
      optimizer$resetOptimizer(data =filteredData$rawData)
      optimizer$setTrafoAlphabet <- trafoAlphabet
      optimizer$setMirror <- input$cb.wizardMirror
      progress <- shiny::Progress$new(session, min = 1, max = length(optimizer$trafoAlphabet)*2)
      progress$set(message = "Optimizing model parameters", value = 1)
      optimizer$optimize(data = filteredData$rawData, progress = progress)
      modelOptimized(TRUE)
      on.exit(progress$close())
      
      output$tbl.optimizedTypes <- DT::renderDataTable(
        optimizer$optTypes %>%
          DT::datatable(
            extensions = "Buttons",
            options = list(
              scrollX = TRUE,
              scrollY = '350px',
              paging = FALSE,
              dom = "Blfrtip",
              buttons = list(list(
                extend = 'csv',
                filename = outFile$bluntFileName("optTrafoTypes"),
                text = "Download"
              ))
            ))
      )
      output$tbl.optimizedValues <- DT::renderDataTable(
        optimizer$optParameter %>%
          DT::datatable(
            extensions = "Buttons",
            options = list(
              scrollX = TRUE,
              scrollY = '350px',
              paging = FALSE,
              dom = "Blfrtip",
              buttons = list(list(
                extend = 'csv',
                filename = outFile$bluntFileName("optTrafoParameter"),
                text = "Download"
              ))
            ))
      )
    }
  })
  
  shiny::observeEvent(input$ab.wizardReset,{
    if(dataLoaded()){
      trafoAlphabet <- optimizer$trafoAlphabet
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
                               value = optimizer$mirror)
      }
  })
  ######################
  # select trafo feature
  ######################
  shiny::observeEvent(input$si.transformationFeature,{
    if(modelDefined()){
      shiny::updateSelectInput(session, "si.transformationType", selected = transformator$trafoType(feature = input$si.transformationFeature))
      shiny::updateCheckboxInput(session, "cb.mirrorLogic", value = transformator$mirrorLogic(feature = input$si.transformationFeature))

      # log result to screen
      output$tbl.featureModelParameter <-
        DT::renderDataTable({
          model$fitResultFeature(feature = input$si.transformationFeature) %>%
            transformTibble() %>%
            format.data.frame(scientific = TRUE, digits = 4) %>%
            DT::datatable(options = list(
              scrollX = TRUE,
              scrollY = '350px',
              paging = FALSE
            ))
        })
      
      output$tbl.featureModelQuality <-
        DT::renderDataTable({
          model$testResultFeature(feature = input$si.transformationFeature) %>%
            transformTibble() %>%
            format.data.frame(scientific = TRUE, digits = 4) %>%
            DT::datatable(options = list(
              scrollX = TRUE,
              scrollY = '350px',
              paging = FALSE
            ))
        })
      output$plt.featureTransformFit <-
        renderPlot(model$plotModel(feature = input$si.transformationFeature))
    }
  })
  
  #####################
  # reset trafo type
  #####################
  shiny::observeEvent(input$ab.trafoReset,{
    if(modelDefined()){
      shiny::updateSelectInput(session, "si.transformationType", selected = transformator$trafoType(feature = input$si.transformationFeature))
      shiny::updateCheckboxInput(session, "cb.mirrorLogic", value = transformator$mirrorLogic(feature = input$si.transformationFeature))
    }
  })
  
  ################
  # set trafo type
  ################
  shiny::observeEvent(input$ab.trafoSet,{
    if(modelDefined()){
      transformator$setTrafoType(feature = input$si.transformationFeature,
        type = input$si.transformationType)
      
      transformator$setMirrorLogic(feature = input$si.transformationFeature,
                                   logic = input$cb.mirrorLogic)
      
      filteredData$rawData %>%
        transformator$estimateTrafoParameter()
      
      featureModel$resetNormDist(data = filteredData$rawData %>%
                                   transformator$mutateData() %>%
                                   dplyr::select(input$si.transformationFeature)
      )
      
      tryCatch({
        featureModel$fit()
      },
      error = function(e) {
        #shiny::showNotification(paste(e),type = "error", duration = 10)
        errorString <- sprintf("Error: could not optimize model parameters for  %s transformation of feature %s. Trafo type is reset to 'none'",
                               input$si.transformationType,
                               input$si.transformationFeature)
        shiny::showNotification(paste(errorString),type = "error", duration = 10)
        
        shiny::updateSelectInput(session, "si.transformationType", selected = "none")
        shiny::updateCheckboxInput(session, "cb.mirrorLogic", value = FALSE)
        
        transformator$setTrafoType(feature = input$si.transformationFeature,
                                   type = "none")
        transformator$setMirrorLogic(feature = input$si.transformationFeature,
                                     logic = FALSE)

        filteredData$rawData %>%
          transformator$estimateTrafoParameter()
        
        featureModel$resetNormDist(data = filteredData$rawData %>%
                                     transformator$mutateData() %>%
                                     dplyr::select(input$si.transformationFeature)
        )
        featureModel$fit()
      }
      )
      
      model$setNormDist(data = featureModel, feature = input$si.transformationFeature) 
      
      # log result to screen
      output$tbl.featureModelParameter <-DT::renderDataTable({
          model$fitResultFeature(feature = input$si.transformationFeature) %>%
            transformTibble() %>%
            format.data.frame(scientific = TRUE, digits = 4) %>%
            DT::datatable(options = list(
              scrollX = TRUE,
              scrollY = '350px',
              paging = FALSE
            ))
        })
      
      output$tbl.featureModelQuality <- DT::renderDataTable({
          model$testResultFeature(feature = input$si.transformationFeature) %>%
            transformTibble() %>%
            format.data.frame(scientific = TRUE, digits = 4) %>%
            DT::datatable(options = list(
              scrollX = TRUE,
              scrollY = '350px',
              paging = FALSE
            ))
        })
      output$plt.featureTransformFit <-
        renderPlot(model$plotModel(feature = input$si.transformationFeature))
      
      output$tbl.transformationParameter <- DT::renderDataTable(transformator$trafoParameter %>%
                                                                  format.data.frame(scientific = TRUE, digits = 4),
                                                                extensions = "Buttons",
                                                                options = list(
                                                                  scrollX = TRUE,
                                                                  scrollY='350px',
                                                                  paging=FALSE,
                                                                  dom = "Blfrtip",
                                                                  buttons = list(list(
                                                                    extend = 'csv',
                                                                    filename = outFile$bluntFileName("trafoPara"),
                                                                    text = "Download"
                                                                  ))))

      output$tbl.modelParameter <- DT::renderDataTable(model$modelParameterData() %>%
                                                         format.data.frame(scientific = TRUE, digits = 4),
                                                       extensions = "Buttons",
                                                       options = list(
                                                         scrollX = TRUE,
                                                         scrollY = '350px',
                                                         paging = FALSE,
                                                         dom = "Blfrtip",
                                                         buttons = list(list(
                                                           extend = 'csv',
                                                           filename = outFile$bluntFileName("modelPara"),
                                                           text = "Download"
                                                         ))))
      
      output$tbl.modelQuality <- DT::renderDataTable(model$modelQualityData() %>%
                                                       format.data.frame(scientific = TRUE, digits = 4),
                                                     extensions = "Buttons",
                                                     options = list(
                                                       scrollX = TRUE,
                                                       scrollY = '350px',
                                                       paging = FALSE,
                                                       dom = "Blfrtip",
                                                       buttons = list(list(
                                                         extend = 'csv',
                                                         filename = outFile$bluntFileName("modelQuality"),
                                                         text = "Download"
                                                       ))))
      
      output$tbl.testResults <- DT::renderDataTable(model$testResultData() %>%
                                                      format.data.frame(scientific = TRUE, digits = 4),
                                                    extensions = "Buttons",
                                                    options = list(
                                                      scrollX = TRUE,
                                                      scrollY = '350px',
                                                      paging = FALSE,
                                                      dom = "Blfrtip",
                                                      buttons = list(list(
                                                        extend = 'csv',
                                                        filename = outFile$bluntFileName("modelTests"),
                                                        text = "Download"
                                                      ))))
    }
  })
  
  
  ######################
  # apply trafo globally
  ######################
  shiny::observeEvent(input$ab.trafoSetGlobal,{
    if(dataLoaded()){
      nanCleaned(FALSE)
      outlierDetected(FALSE)
      outlierRevised(FALSE)
      progress <- shiny::Progress$new(session, min = 1, max = length(filteredData$numericFeatureNames))
      progress$set(message = "Fitting model parameters", value = 1)
      on.exit(progress$close())

      for (feature in filteredData$numericFeatureNames){
        transformator$setTrafoType(feature = feature,
                                   type = input$si.transformationType)
        transformator$setMirrorLogic(feature = feature,
                                     logic = input$cb.mirrorLogic)
      }
      
      filteredData$rawData %>%
        transformator$estimateTrafoParameter()
      
      filteredData$rawData %>%
        transformator$mutateData() %>%
        model$resetModel(progress)
      modelDefined(TRUE)
      
      # log result to screen
      shiny::updateSelectInput(session, "si.transformationType", selected = transformator$trafoType(feature = input$si.transformationFeature))
      shiny::updateCheckboxInput(session, "cb.mirrorLogic", value = transformator$mirrorLogic(feature = input$si.transformationFeature))
      
      output$tbl.featureModelParameter <-DT::renderDataTable({
        model$fitResultFeature(feature = input$si.transformationFeature) %>%
          transformTibble() %>%
          format.data.frame(scientific = TRUE, digits = 4) %>%
          DT::datatable(options = list(
            scrollX = TRUE,
            scrollY = '350px',
            paging = FALSE
          ))
      })
      
      output$tbl.featureModelQuality <- DT::renderDataTable({
        model$testResultFeature(feature = input$si.transformationFeature) %>%
          transformTibble() %>%
          format.data.frame(scientific = TRUE, digits = 4) %>%
          DT::datatable(options = list(
            scrollX = TRUE,
            scrollY = '350px',
            paging = FALSE
          ))
      })
      output$plt.featureTransformFit <-
        renderPlot(model$plotModel(feature = input$si.transformationFeature))
      
      output$tbl.transformationParameter <- DT::renderDataTable(transformator$trafoParameter %>%
                                                                  format.data.frame(scientific = TRUE, digits = 4),
                                                                extensions = "Buttons",
                                                                options = list(
                                                                  scrollX = TRUE,
                                                                  scrollY='350px',
                                                                  paging=FALSE,
                                                                  dom = "Blfrtip",
                                                                  buttons = list(list(
                                                                    extend = 'csv',
                                                                    filename = outFile$bluntFileName("trafoPara"),
                                                                    text = "Download"
                                                                  ))))
      
      output$tbl.modelParameter <- DT::renderDataTable(model$modelParameterData() %>%
                                                         format.data.frame(scientific = TRUE, digits = 4),
                                                       extensions = "Buttons",
                                                       options = list(
                                                         scrollX = TRUE,
                                                         scrollY = '350px',
                                                         paging = FALSE,
                                                         dom = "Blfrtip",
                                                         buttons = list(list(
                                                           extend = 'csv',
                                                           filename = outFile$bluntFileName("modelPara"),
                                                           text = "Download"
                                                         ))))
      
      output$tbl.modelQuality <- DT::renderDataTable(model$modelQualityData() %>%
                                                       format.data.frame(scientific = TRUE, digits = 4),
                                                     extensions = "Buttons",
                                                     options = list(
                                                       scrollX = TRUE,
                                                       scrollY = '350px',
                                                       paging = FALSE,
                                                       dom = "Blfrtip",
                                                       buttons = list(list(
                                                         extend = 'csv',
                                                         filename = outFile$bluntFileName("modelQuality"),
                                                         text = "Download"
                                                       ))))
      
      output$tbl.testResults <- DT::renderDataTable(model$testResultData() %>%
                                                      format.data.frame(scientific = TRUE, digits = 4),
                                                    extensions = "Buttons",
                                                    options = list(
                                                      scrollX = TRUE,
                                                      scrollY = '350px',
                                                      paging = FALSE,
                                                      dom = "Blfrtip",
                                                      buttons = list(list(
                                                        extend = 'csv',
                                                        filename = outFile$bluntFileName("modelTests"),
                                                        text = "Download"
                                                      ))))
    }
  })

  #######################
  # reset missing choices
  #######################
  shiny::observeEvent(input$ab.missingsReset,{
    shiny::updateSelectInput(session,
                             "si.nanHandleMethod",
                             selected = missings$cleaningAgent)
    
    shiny::updateNumericInput(session,
                              "ni.nanSeed",
                              value = missings$seed)
  })
  
  ####################
  # missing statistics
  ####################
  shiny::observeEvent(input$si.nanSummary,{
    if(modelDefined()){
      switch (input$si.nanSummary,
              "Statistics" = output$tbl.nanSummary <- DT::renderDataTable(missings$nanDistribution(data = filteredData$rawData)%>%
                                                                            DT::datatable(
                                                                              extensions = "Buttons",
                                                                              options = list(
                                                                              scrollX = TRUE,
                                                                              scrollY = '350px',
                                                                              paging = FALSE,
                                                                              dom = "Blfrtip",
                                                                              buttons = list(list(
                                                                                extend = 'csv',
                                                                                filename = outFile$bluntFileName("nanSummary"),
                                                                                text = "Download"))
                                                                            ))),
              "Details" = output$tbl.nanSummary <- DT::renderDataTable(missings$nanPositives(data = filteredData$rawData)%>%
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
                                                                             filename = outFile$bluntFileName("nanDetails"),
                                                                             text = "Download"))
                                                                         )))
      )
    }
  })
  
  #############################
  # update nan cleaning results
  #############################
  shiny::observeEvent(input$si.nanHandleFeature,{
    if(nanCleaned()){
      output$tbl.nanCleaningSummary <- DT::renderDataTable(cleanedData$rawData %>%
                                                             dplyr::select(input$si.nanHandleFeature) %>%
                                                             dplyr::slice(missings$missingsIdxByFeature(feature = input$si.nanHandleFeature)) %>%
                                                             format.data.frame(scientific = TRUE, digits = 4) %>%
                                                             DT::datatable(options = list(
                                                               scrollX = TRUE,
                                                               scrollY = '350px',
                                                               paging = FALSE)))
      
      output$plt.nanCleaningSummary <- shiny::renderPlot(missings$featurePlot(data = cleanedData$rawData, feature = input$si.nanHandleFeature))
    }
  })
  
  ###############
  # fill missings
  ###############
  shiny::observeEvent(input$ab.fillMissings,{
    if(modelDefined()){
      missings$setCleaningAgent <- input$si.nanHandleMethod
      missings$setSeed <- input$ni.nanSeed
      
      progress <- shiny::Progress$new(session, min = 1, max = length(filteredData$numericFeatureNames))
      progress$set(message = "Handle NANs", value = 1)
      on.exit(progress$close())
      
      cleanedData$setRawData <- scaledData$rawData %>%
        missings$handleMissings(progress = progress) %>%
        model$rescaleData() %>%
        transformator$reverseMutateData()
      
      output$tbl.nanCleaningSummary <- DT::renderDataTable(cleanedData$rawData %>%
                                                             dplyr::select(input$si.nanHandleFeature) %>%
                                                             dplyr::slice(missings$missingsIdxByFeature(feature = input$si.nanHandleFeature)) %>%
                                                            format.data.frame(scientific = TRUE, digits = 4) %>%
                                                            DT::datatable(options = list(
                                                              scrollX = TRUE,
                                                              scrollY = '350px',
                                                              paging = FALSE)))
      
      output$plt.nanCleaningSummary <- shiny::renderPlot(missings$featurePlot(data = cleanedData$rawData, feature = input$si.nanHandleFeature))
      nanCleaned(TRUE)
    }
  })
  
  ########################
  # reset outliers choices
  ########################
  shiny::observeEvent(input$ab.outliersReset,{
    shiny::updateSelectInput(session,
                             "si.outHandleMethod",
                             selected = outliers$cleaningAgent)
    
    shiny::updateNumericInput(session,
                              "ni.outSeed",
                              value = outliers$seed)
  })
  
  #################
  # detect outliers
  #################
  shiny::observeEvent(input$ab.detectOutliers,{
    if(modelDefined()){
      progress <- shiny::Progress$new(session, min = 1, max = length(scaledData$numericFeatureNames))
      progress$set(message = "Detecting Outliers", value = 1)
      on.exit(progress$close())
      
      scaledData$rawData %>%
        outliers$resetOutliersParameter(progress = progress)
      
      switch (input$si.outSummary,
              "Statistics" = output$tbl.outSummary <- DT::renderDataTable(outliers$outliersStatistics %>%
                                                                            format.data.frame(scientific = FALSE, digits = 4) %>%
                                                                            DT::datatable(
                                                                              extensions = "Buttons",
                                                                              options = list(
                                                                              scrollX = TRUE,
                                                                              scrollY = '350px',
                                                                              paging = FALSE,
                                                                              dom = "Blfrtip",
                                                                              buttons = list(list(
                                                                                extend = 'csv',
                                                                                filename = outFile$bluntFileName("outlierSummary"),
                                                                                text = "Download"))
                                                                            ))),
              "Outliers" = output$tbl.outSummary <- DT::renderDataTable(outliers$outlierTable(data = filteredData$rawData) %>%
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
                                                                                filename = outFile$bluntFileName("outlierSummary"),
                                                                                text = "Download"))
                                                                          ))),
              "Data" = output$tbl.outSummary <- DT::renderDataTable(outliers$DataTable(data = filteredData$rawData) %>%
                                                                      format.data.frame(scientific = TRUE, digits = 4) %>%
                                                                      DT::datatable(
                                                                        options = list(
                                                                          scrollX = TRUE,
                                                                          scrollY = '350px',
                                                                          paging = FALSE
                                                                        )))
      )
      output$plt.outSummary <- shiny::renderPlot(outliers$plotOutliersDistribution())
      outlierDetected(TRUE)
    }
  })
  
  #################
  # revise outliers
  #################
  shiny::observeEvent(input$ab.reviseOutliers,{
    if(!outlierDetected()){
      errorMessage <- sprintf("No outliers detected.", inFile$suffix)
      shiny::showNotification(paste(errorMessage),type = "error", duration = 10)
    }
    else{
      progress <- shiny::Progress$new(session, min = 1, max = length(scaledData$numericFeatureNames)*2)
      progress$set(message = "Handle Outliers", value = 1)
      on.exit(progress$close())
      
      outliers$setCleaningAgent <- input$si.outHandleMethod
      outliers$setSeed <- input$ni.outSeed
      cleanedData$setRawData <- scaledData$rawData %>%
        missings$handleMissings(progress = progress) %>%
        outliers$handleOutliers(progress = progress) %>%
        model$rescaleData() %>%
        transformator$reverseMutateData()
      
      outlierRevised(TRUE)
      
      output$tbl.outCleaningSummary <- DT::renderDataTable(cleanedData$rawData %>%
                                                             dplyr::select(input$si.outHandleFeature) %>%
                                                             dplyr::slice(outliers$outliersIdxByFeature(feature = input$si.outHandleFeature)) %>%
                                                             format.data.frame(scientific = TRUE, digits = 4) %>%
                                                             DT::datatable(options = list(
                                                               scrollX = TRUE,
                                                               scrollY = '350px',
                                                               paging = FALSE)))
      output$plt.outCleaningSummary <- shiny::renderPlot(outliers$featurePlot(data = cleanedData$rawData, feature = input$si.outHandleFeature))
      
      shiny::updateSelectInput(session, "si.outHandleFeature",
                               choices = cleanedData$numericFeatureNames,
                               selected = cleanedData$numericFeatureNames[1])
    }
  })
  
  #################################
  # update outlier revision results
  #################################
  shiny::observeEvent(input$si.outHandleFeature,{
    if(outlierRevised()){
      output$tbl.outCleaningSummary <- DT::renderDataTable(cleanedData$rawData %>%
                                                             dplyr::select(input$si.outHandleFeature) %>%
                                                             dplyr::slice(outlierss$outlierssIdxByFeature(feature = input$si.outHandleFeature)) %>%
                                                             format.data.frame(scientific = TRUE, digits = 4) %>%
                                                             DT::datatable(options = list(
                                                               scrollX = TRUE,
                                                               scrollY = '350px',
                                                               paging = FALSE)))
      
      output$plt.outCleaningSummary <- shiny::renderPlot(outliers$featurePlot(data = cleanedData$rawData, feature = input$si.outHandleFeature))
    }
    else{
      output$tbl.outCleaningSummary <- DT::renderDataTable(NULL)
      output$plt.outCleaningSummary <- shiny::renderPlot(NULL)
    }
  })
  
  #############################
  # display outlier information
  #############################
  shiny::observeEvent(input$si.outSummary,{
    if(outlierDetected()){
      switch (input$si.outSummary,
              "Statistics" = output$tbl.outSummary <- DT::renderDataTable(outliers$outliersStatistics %>%
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
                                                                                filename = outFile$bluntFileName("outlierSummary"),
                                                                                text = "Download"))
                                                                            ))),
              "Outliers" = output$tbl.outSummary <- DT::renderDataTable(outliers$outlierTable(data = filteredData$rawData) %>%
                                                                          DT::datatable(
                                                                            extensions = "Buttons",
                                                                            options = list(
                                                                              scrollX = TRUE,
                                                                              scrollY = '350px',
                                                                              paging = FALSE,
                                                                              dom = "Blfrtip",
                                                                              buttons = list(list(
                                                                                extend = 'csv',
                                                                                filename = outFile$bluntFileName("outlierDetails"),
                                                                                text = "Download"))
                                                                            )
                                                                          )),
              "Data" = output$tbl.outSummary <- DT::renderDataTable(outliers$DataTable(data = filteredData$rawData) %>%
                                                                      format.data.frame(scientific = TRUE, digits = 3) %>%
                                                                      DT::datatable(
                                                                        options = list(
                                                                          scrollX = TRUE,
                                                                          scrollY = '350px',
                                                                          paging = FALSE
                                                                        )
                                                                      ))
      )
    }
  })
  
  shiny::observeEvent(input$si.outHandleFeature,{
    if(outlierDetected()){
      output$tbl.outCleaningSummary <- DT::renderDataTable(cleanedData$rawData %>%
                                                             dplyr::select(input$si.outHandleFeature) %>%
                                                             dplyr::slice(outliers$outliersIdxByFeature(feature = input$si.outHandleFeature)) %>%
                                                             format.data.frame(scientific = TRUE, digits = 4) %>%
                                                             DT::datatable(
                                                               options = list(
                                                                 scrollX = TRUE,
                                                                 scrollY = '350px',
                                                                 paging = FALSE
                                                               )))
      
      
      output$plt.outCleaningSummary <- shiny::renderPlot(outliers$featurePlot(data = cleanedData$rawData, feature = input$si.outHandleFeature))
    }
  })
  
  
  ##############################
  # calculate correlation Matrix
  ##############################
  shiny::observeEvent(input$ab.resetRegression,{
    if(dataCorrelated()){
      shiny::updateSelectInput(session, "si.regressionAbs",
                               choices = filteredData$numericFeatureNames,
                               selected = filteredData$numericFeatureNames[1])
      shiny::updateSelectInput(session, "si.regressionOrd",
                               choices = filteredData$numericFeatureNames [! filteredData$numericFeatureNames %in% input$si.regressionAbs],
                               selected = filteredData$numericFeatureNames [! filteredData$numericFeatureNames %in% input$si.regressionAbs][1])
      shiny::updateSelectInput(session, "si.regressionStats",
                               selected = "Intercept")
      shiny::updateSelectInput(session, "si.regressionStats",
                               selected = "Intercept")
    }
  })
  
  shiny::observeEvent(input$ab.regression,{
    if (outlierRevised()){
      progress <- shiny::Progress$new(session, min = 1, max = length(scaledData$numericFeatureNames)*length(scaledData$numericFeatureNames)*4)
      progress$set(message = "Calculate Regression", value = 1)
      on.exit(progress$close())
      regressor$resetRegressor(data = cleanedData$rawData, progress = progress)
      correlator$resetCorrelator(data = cleanedData$rawData, progress = progress)
      
      shiny::updateSelectInput(
        session,
        "si.regressionAbs",
        choices = filteredData$numericFeatureNames,
        selected = filteredData$numericFeatureNames[1]
      )
      shiny::updateSelectInput(
        session,
        "si.regressionOrd",
        choices = filteredData$numericFeatureNames [!filteredData$numericFeatureNames %in% input$si.regressionAbs],
        selected = filteredData$numericFeatureNames [!filteredData$numericFeatureNames %in% input$si.regressionAbs][1]
      )
      dataCorrelated(TRUE)
      
      shiny::updateSelectInput(session, "si.regressionStats",
                               selected = "Intercept")

      output$tbl.regressionMatrix <- DT::renderDataTable(
        regressor$printInterceptTbl() %>%
          format.data.frame(scientific = TRUE, digits = 4) %>%
          DT::datatable(
            extensions = 'Buttons',
            options = list(
              dom = "Blfrtip",
              scrollX = TRUE,
              scrollY = '350px',
              paging = FALSE,
              buttons = c('csv')
            )
          )
      )
    }
    
  })
  
  shiny::observeEvent(input$si.regressionAbs,{
    if(dataCorrelated()){
      shiny::updateSelectInput(session, "si.regressionOrd",
                               choices = filteredData$numericFeatureNames [! filteredData$numericFeatureNames %in% input$si.regressionAbs],
                               selected = filteredData$numericFeatureNames [! filteredData$numericFeatureNames %in% input$si.regressionAbs][1])

      regressor$setAbscissa <- input$si.regressionAbs
      regressor$setOrdinate <- input$si.regressionOrd
      
      regressor$createFeatureModel(data = cleanedData$rawData)
      
      output$plt.regressionFeature <-
        shiny::renderPlot(regressor$plotResult())
      
      output$tbl.regressionFeature <- DT::renderDataTable({
        regressor$printModel() %>%
          format.data.frame(scientific = TRUE, digits = 4) %>%
          DT::datatable(
            extensions = 'Buttons',
            options = list(
              dom = "Blfrtip",
              scrollX = TRUE,
              scrollY = TRUE,
              paging = FALSE,
              buttons = c('csv')
            )
          )
      })
      
      correlator$setAbscissa <- input$si.regressionAbs
      correlator$setOrdinate <- input$si.regressionOrd
      
      output$tbl.correlationFeature <- DT::renderDataTable(
        correlator$printFeature() %>%
          format.data.frame(scientific = TRUE, digits = 4) %>%
          DT::datatable(
            extensions = 'Buttons',
            options = list(
              dom = "Blfrtip",
              scrollX = TRUE,
              scrollY = TRUE,
              paging = FALSE,
              buttons = c('csv')
            )
          )
      )
    }
  })
  
  shiny::observeEvent(input$si.regressionOrd,{
    if(dataCorrelated()){
      regressor$setAbscissa <- input$si.regressionAbs
      regressor$setOrdinate <- input$si.regressionOrd
      
      regressor$createFeatureModel(data = cleanedData$rawData)
      
      output$plt.regressionFeature <-
        shiny::renderPlot(regressor$plotResult())
      
      output$tbl.regressionFeature <- DT::renderDataTable({
        regressor$printModel() %>%
          format.data.frame(scientific = TRUE, digits = 4) %>%
          DT::datatable(
            extensions = 'Buttons',
            options = list(
              dom = "Blfrtip",
              scrollX = TRUE,
              scrollY = TRUE,
              paging = FALSE,
              buttons = c('csv')
            )
          )
      })
      
      correlator$setAbscissa <- input$si.regressionAbs
      correlator$setOrdinate <- input$si.regressionOrd
      
      output$tbl.correlationFeature <- DT::renderDataTable(
        correlator$printFeature() %>%
          format.data.frame(scientific = TRUE, digits = 4) %>%
          DT::datatable(
            extensions = 'Buttons',
            options = list(
              dom = "Blfrtip",
              scrollX = TRUE,
              scrollY = TRUE,
              paging = FALSE,
              buttons = c('csv')
            )
          ))
    }
  })
  
  shiny::observeEvent(input$ab.refreshRegression,{
    if(dataCorrelated()){
      regressor$setAbscissa <- input$si.regressionAbs
      regressor$setOrdinate <- input$si.regressionOrd
      
      regressor$createFeatureModel(data = cleanedData$rawData)
      
      output$plt.regressionFeature <- shiny::renderPlot(
        regressor$plotResult()
      )
      
      output$tbl.regressionFeature <- DT::renderDataTable({
        regressor$printModel() %>%
          format.data.frame(scientific = TRUE, digits = 4) %>%
          DT::datatable(
            extensions = 'Buttons',
            options = list(
              dom = "Blfrtip",
              scrollX = TRUE,
              scrollY = TRUE,
              paging = FALSE,
              buttons = c('csv')
            )
          )})
      
      correlator$setAbscissa <- input$si.regressionAbs
      correlator$setOrdinate <- input$si.regressionOrd
      
      output$tbl.correlationFeature <- DT::renderDataTable(
        correlator$printFeature() %>%
          format.data.frame(scientific = TRUE, digits = 4) %>%
          DT::datatable(
            extensions = 'Buttons',
            options = list(
              dom = "Blfrtip",
              scrollX = TRUE,
              scrollY = TRUE,
              paging = FALSE,
              buttons = c('csv')
            )
          ))
    }
  })
  
  shiny::observeEvent(input$si.regressionStats,{
    if(dataCorrelated()){
      output$tbl.regressionMatrix <- DT::renderDataTable(
        switch (input$si.regressionStats,
                "Intercept" = regressor$printInterceptTbl(),
                "p.Intercept" = regressor$printPInterceptTbl(),
                "Slope" = regressor$printSlopeTbl(),
                "p.Slope" = regressor$printPSlopeTbl(),
                "r" = correlator$printRTbl(),
                "p.Pearson" = correlator$printPPearsonTbl(),
                "tau" = correlator$printTauTbl(),
                "p.Kendall" = correlator$printPKendallTbl(),
                "rho" = correlator$printRhoTbl(),
                "p.Spearman" = correlator$printPSpearmanTbl()
                ) %>% 
          format.data.frame(scientific = TRUE, digits = 4) %>%
          DT::datatable(
            extensions = 'Buttons',
            options = list(
              dom = "Blfrtip",
              scrollX = TRUE,
              scrollY = '350px',
              paging = FALSE,
              buttons = c('csv'))))
    }
  })
  
  ####################
  # export ui elements
  ####################
  shiny::observeEvent(input$si.exportType,{
    if(grepl(input$si.exportType, "Complete")){
      shiny::updateSelectInput(session, "si.suffix",
                               choices = exporter$suffixAlphabet)
    }
    else{
      shiny::updateSelectInput(session, "si.suffix",
                               choices = exporter$reducedSuffixAlphabet)
    }
    outFile$setExportType <- input$si.exportType
    outFile$updateTimeString()
    outFile$mergeFileName()
    exporter$setExportType <- input$si.exportType
  })
  
  shiny::observeEvent(input$si.suffix,{
    outFile$setSuffix <- input$si.suffix
    outFile$updateTimeString()
    outFile$mergeFileName()
    exporter$setExportType <- input$si.exportType
  })
  
  ########################
  # export download button
  ########################
  output$db.export <- shiny::downloadHandler(
    filename = function() {
      outFile$fileName
    },
    content =  function(uploadFileName){
      # update
      exporter$setOutFileName <- uploadFileName
      exporter$export(filteredObj = filteredData,
                      transformedObj = transformedData,
                      scaledObj = scaledData,
                      revisedObj = cleanedData,
                      transformatorObj = transformator,
                      modelObj = model,
                      naObj = missings,
                      outlierObj = outliers,
                      regressionObj = regressor,
                      correlationObj = correlator,
                      optimObj = optimizer)
    }
  )
  
  
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
              DT::datatable(
                extensions = "Buttons",
                options = list(
                scrollX = TRUE,
                scrollY = '350px',
                paging = FALSE,
                dom = "Blfrtip",
                buttons = list(list(
                  extend = 'csv',
                  filename = outFile$bluntFileName("Statistics"),
                  text = "Download"
                ))
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
    if (input$menue == "tab_wizard"){
      if (!dataLoaded()){
        errorMessage <- sprintf("No data loaded.", inFile$suffix)
        shiny::showNotification(paste(errorMessage),type = "error", duration = 10)
      }
      if(!modelOptimized()){
        output$tbl.optimizedTypes <- DT::renderDataTable(NULL)
        output$tbl.optimizedValues <- DT::renderDataTable(NULL)
      }
    }
    if (input$menue == "tab_trafo"){
      if(!dataLoaded()){
        output$plt.featureTransformFit <- shiny::renderPlot(NULL)
        output$tbl.featureModelParameter <- DT::renderDataTable(NULL)
        output$tbl.featureModelQuality <- DT::renderDataTable(NULL)
        output$tbl.transformationParameter <- DT::renderDataTable(NULL)
        output$tbl.modelParameter <- DT::renderDataTable(NULL)
        output$tbl.modelQuality <- DT::renderDataTable(NULL)
        output$tbl.modelTestResults <- DT::renderDataTable(NULL)
        
        errorMessage <- sprintf("No data loaded.", inFile$suffix)
        shiny::showNotification(paste(errorMessage),type = "error", duration = 10)
      }
      else if (!modelDefined()){
        filteredData$setRawData <- rawData$rawData %>%
          filterSet$filter()
        transformator$resetTrafoParameter(data = filteredData$rawData)
        transformator$estimateTrafoParameter(data = filteredData$rawData)
        
        shiny::updateSelectInput(session, "si.transformationFeature",
                                 choices = filteredData$numericFeatureNames,
                                 selected = filteredData$numericFeatureNames[1])
        shiny::updateSelectInput(session, "si.transformationType",
                                 choices = transformator$trafoAlphabet,
                                 selected = transformator$trafoType(feature = filteredData$numericFeatureNames[1]))
        
        errorMessage <- sprintf("No Model defined. Choose a global transformation type to start..")
        shiny::showNotification(paste(errorMessage),type = "error", duration = 10)
      }
    }
    if (input$menue == "tab_tidy"){
      shiny::updateSelectInput(session,
                               "si.nanHandleMethod",
                               choices = outliers$cleaningAgentAlphabet,
                               selected = outliers$cleaningAgent)
      
      if(!dataLoaded()){
        output$plt.nanSummary <- shiny::renderPlot(NULL)
        output$tbl.nanSummary <- DT::renderDataTable(NULL)
        output$plt.nanCleaningSummary <- shiny::renderPlot(NULL)
        output$tbl.nanCleaningSummary <- DT::renderDataTable(NULL)
        errorMessage <- sprintf("No data loaded.")
        shiny::showNotification(paste(errorMessage),type = "error", duration = 10)
      }
      else if(!modelDefined()){
        shiny::updateSelectInput(session, "si.nanHandleFeature",
                                 choices = filteredData$numericFeatureNames,
                                 selected = filteredData$numericFeatureNames[1])
        
        output$plt.nanSummary <- shiny::renderPlot(NULL)
        output$tbl.nanSummary <- DT::renderDataTable(NULL)
        output$plt.nanCleaningSummary <- shiny::renderPlot(NULL)
        output$tbl.nanCleaningSummary <- DT::renderDataTable(NULL)
        errorMessage <- sprintf("No transformed dataset available.")
        shiny::showNotification(paste(errorMessage),type = "error", duration = 10)
      }
      else if (!nanCleaned()){
        output$plt.nanCleaningSummary <- shiny::renderPlot(NULL)
        output$tbl.nanCleaningSummary <- DT::renderDataTable(NULL)
        
        
        transformedData$setRawData  <- filteredData$rawData %>%
          transformator$mutateData()
        scaledData$setRawData <- transformedData$rawData %>%
          model$scaleData()

        missings$resetMissingsParameter(data = scaledData$rawData)
        
        shiny::updateSelectInput(session, "si.nanHandleFeature",
                                 choices = filteredData$numericFeatureNames,
                                 selected = filteredData$numericFeatureNames[1])
        
        output$plt.nanSummary <- shiny::renderPlot(missings$nanHeatMap())
        switch (input$si.nanSummary,
                "Statistics" = output$tbl.nanSummary <- DT::renderDataTable(missings$nanDistribution(data = filteredData$rawData)%>%
                                                                              DT::datatable(
                                                                                extensions = "Buttons",
                                                                                options = list(
                                                                                scrollX = TRUE,
                                                                                scrollY = '350px',
                                                                                paging = FALSE,
                                                                                dom = "Blfrtip",
                                                                                buttons = list(list(
                                                                                  extend = 'csv',
                                                                                  filename = outFile$bluntFileName("nanSummary"),
                                                                                  text = "Download"))
                                                                              ))),
                "Details" = output$tbl.nanSummary <- DT::renderDataTable(missings$nanPositives(data = filteredData$rawData)%>%
                                                                           format.data.frame(scientific = TRUE, digits = 4) %>%
                                                                           DT::datatable(
                                                                             extensions = "Buttons",
                                                                             options = list(
                                                                             scrollX = TRUE,
                                                                             scrollY = '350px',
                                                                             paging = FALSE,
                                                                             buttons = list(list(
                                                                               extend = 'csv',
                                                                               filename = outFile$bluntFileName("nanDetails"),
                                                                               text = "Download"))
                                                                           )))
        )
        
      }
    }
    if (input$menue == "tab_revise"){
      shiny::updateSelectInput(session,
                               "si.outHandleMethod",
                               choices = outliers$cleaningAgentAlphabet,
                               selected = outliers$cleaningAgent)
      
      if(!dataLoaded()){
        output$plt.outSummary <- shiny::renderPlot(NULL)
        output$tbl.outSummary <- DT::renderDataTable(NULL)
        output$plt.outCleaningSummary <- shiny::renderPlot(NULL)
        output$tbl.outCleaningSummary <- DT::renderDataTable(NULL)
        errorMessage <- sprintf("No data loaded.")
        shiny::showNotification(paste(errorMessage),type = "error", duration = 10)
      }
      else if(!modelDefined()){
        shiny::updateSelectInput(session, "si.outHandleFeature",
                                 choices = filteredData$numericFeatureNames,
                                 selected = filteredData$numericFeatureNames[1])
        output$plt.outSummary <- shiny::renderPlot(NULL)
        output$tbl.outSummary <- DT::renderDataTable(NULL)
        output$plt.outCleaningSummary <- shiny::renderPlot(NULL)
        output$tbl.outCleaningSummary <- DT::renderDataTable(NULL)
        errorMessage <- sprintf("No transformed dataset available.")
        shiny::showNotification(paste(errorMessage),type = "error", duration = 10)
      }
      else if (!outlierDetected()){
        output$plt.outCleaningSummary <- shiny::renderPlot(NULL)
        output$tbl.outCleaningSummary <- DT::renderDataTable(NULL)
        output$plt.outSummary <- shiny::renderPlot(NULL)
        output$tbl.outSummary <- DT::renderDataTable(NULL)

        
        transformedData$setRawData  <- filteredData$rawData %>%
          transformator$mutateData()
        scaledData$setRawData <- transformedData$rawData %>%
          model$scaleData()
        
      }
    }
    if(input$menue == "tab_regression"){
      if(!outlierRevised()){
        output$plt.regressionFeature <- shiny::renderPlot(NULL)
        output$tbl.regressionFeature <- DT::renderDataTable(NULL)
        output$tbl.correlationFeature <- DT::renderDataTable(NULL)
        output$tbl.regressionMatrix <- DT::renderDataTable(NULL)
        errorMessage <- sprintf("No Cleaned data set.")
        shiny::showNotification(paste(errorMessage),type = "error", duration = 10)

      }
      else if (!dataCorrelated()){
        output$plt.regressionFeature <- shiny::renderPlot(NULL)
        output$tbl.regressionFeature <- DT::renderDataTable(NULL)
        output$tbl.correlationFeature <- DT::renderDataTable(NULL)
        output$tbl.regressionMatrix <- DT::renderDataTable(NULL)
        errorMessage <- sprintf("So far, no correlation analysis has been performed for the currently selected data set.")
        shiny::showNotification(paste(errorMessage),type = "error", duration = 10)
      }
    }
    if(input$menue =="tab_export"){
      if(!dataLoaded()){
        errorMessage <- sprintf("No data loaded.")
        shiny::showNotification(paste(errorMessage), type = "error", duration = 10)
      }
      else{
        outFile$setBaseName <- inFile$baseName
        shiny::updateSelectInput(session, "si.exportType",
                                 choices = exporter$exportTypeAlphabet,
                                 selected = exporter$exportTypeAlphabet[1])
        shiny::updateSelectInput(session, "si.suffix",
                                 choices = exporter$reducedSuffixAlphabet,
                                 selected = exporter$reducedSuffixAlphabet[1])
        outFile$setExportType <- input$si.exportType
        outFile$setSuffix <- input$si.suffix
        outFile$updateTimeString()
        outFile$mergeFileName()
        exporter$setExportType <- input$si.exportType
      }
    }
  })
})