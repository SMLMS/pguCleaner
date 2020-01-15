library("R6")
library("tidyverse")
library("utils")
library("writexl")
source(file = "../R/pguData.R", local=TRUE)
source(file = "../R/pguTransformator.R", local=TRUE)
source(file = "../R/pguModel.R", local=TRUE)
#source(file = "../R/pguNormDist.R", local=TRUE)
source(file = "../R/pguOptimizer.R", local=TRUE)
source(file = "../R/pguMissings.R", local=TRUE)
source(file = "../R/pguOutliers.R", local=TRUE)
source(file = "../R/pguCorrelator.R", local=TRUE)
source(file = "../R/pguRegressor.R", local=TRUE)

pgu.exporter <- R6::R6Class("pgu.exporter",
                        ####################
                        # instance variables
                        ####################
                        private = list(
                          .outFileName = "character",
                          .suffixAlphabet  = "character",
                          .reducedSuffixAlphabet  = "character",
                          .suffix = "factor",
                          .exportTypeAlphabet = "character",
                          .exportType = "factor",
                          .naChar = "character"
                        ),
                        ##################
                        # accessor methods
                        ##################
                        active = list(
                          outFileName = function(){
                            return(private$.outFileName)
                          },
                          setOutFileName = function(name = "character"){
                            private$.outFileName <- name
                            self$extractSuffix()
                          },
                          # baseName = function(){
                          #   return(private$.baseName)
                          # },
                          # folderName = function(){
                          #   return(private$.folderName)
                          # },
                          suffixAlphabet = function(){
                            return(private$.suffixAlphabet)
                          },
                          reducedSuffixAlphabet = function(){
                            return(private$.reducedSuffixAlphabet)
                          },
                          suffix = function(){
                            return(as.character(private$.suffix))
                          },
                          setSuffix = function(value = "character"){
                            private$.suffix <- factor(value,
                                                      levels = self$suffixAlphabet)
                          },
                          exportTypeAlphabet = function(){
                            return(private$.exportTypeAlphabet)
                          },
                          exportType = function(){
                            return(as.character(private$.exportType))
                          },
                          setExportType = function(value = "character"){
                            private$.exportType  <- factor(value,
                                                           levels = self$exportTypeAlphabet)
                          },
                          naChar = function(){
                            return(private$.naChar)
                          }
                        ),
                        ###################
                        # memory management
                        ###################
                        public = list(
                          initialize = function(name = "character") {
                            private$.suffixAlphabet <- c("csv","h5", "xlsx")
                            private$.reducedSuffixAlphabet <- c("csv", "xlsx")
                            self$updateExportTypeAlphabet()
                            # self$setExportType <- "Filtered"
                            private$.naChar <- "NA"
                          },
                          finalize = function() {
                            print("Instance of pgu.exporter removed from heap")
                          },
                          ##########################
                          # print instance variables
                          ##########################
                          print = function() {
                            rString <- sprintf("\npgu.exporter\n")
                            cat(rString)
                            eString <- sprintf("\noutFileName: %s\nsuffix: %s\nexportType: %s\nnaChar: %s\n",
                                               self$outFileName,
                                               self$suffix,
                                               self$exportType,
                                               self$naChar)
                            cat(eString)
                            cat("\n\n")
                            invisible(self)
                          }
                        )
)
####################
# public functions
####################
pgu.exporter$set("public", "updateExportTypeAlphabet", function(dataLoaded = FALSE,
                                                                modelOptimized = FALSE,
                                                                modelDefined = FALSE,
                                                                nanCleaned = FALSE,
                                                                outlierDetected = FALSE,
                                                                outlierRevised = FALSE,
                                                                dataCorrelated = FALSE,
                                                                dataRegression = FALSE){
  alphabet <- character(0)
  if(dataLoaded){
    alphabet <- c(alphabet, "Complete", "Filtered-Data", "Filtered-Statistics")
  }
  if(modelOptimized){
    alphabet <- c(alphabet, "Tested-Transformations","Optimal-Transformations", "Optimized-Transformation-Parameter")
  }
  if(modelDefined){
    alphabet <- c(alphabet , "Transformed-Data", "Transformation-Parameter", "Model-Parameter", "Model-Quality", "Model-Test")
  }
  if(nanCleaned){
    alphabet <- c(alphabet, "NA-Statistics", "NA-Details", "NA-Cleaned-Data")
  }
  if(outlierDetected){
    alphabet <- c(alphabet, "Outlier-Statistics", "Outlier-Details")
  }
  if(outlierRevised){
    alphabet <- c(alphabet, "Outlier-Corrected-Data")
  }
  if(dataCorrelated){
    alphabet <- c(alphabet,
                  "Pearson-R",
                  "Pearson-P",
                  "Kendall-Tau",
                  "Kendall-P",
                  "Spearman-Rho",
                  "Spearman-P")
  }
  if(dataRegression){
    alphabet <- c(alphabet,
                  "Intercept",
                  "Intercept-P",
                  "Slope",
                  "Slope-P")
  }
  private$.exportTypeAlphabet <- alphabet
})

pgu.exporter$set("public", "extractSuffix", function(){
  self$setSuffix <- tools::file_ext(self$outFileName)
})

#################
# write functions
#################
pgu.exporter$set("public", "writeDataToCsv", function(obj = "tbl_df"){
  utils::write.csv(obj,
                   self$outFileName,
                   row.names = FALSE,
                   na = self$naChar)
})

pgu.exporter$set("public", "writeDataToXlsx", function(obj = "tbl_df"){
  writexl::write_xlsx(obj,
                      path = self$outFileName,
                      col_names = TRUE,
                      format_headers = TRUE)
})

##################
# export functions
##################
pgu.exporter$set("public", "exportData", function(obj = "tbl_df"){
  switch (self$suffix,
          "csv" = {self$writeDataToCsv(obj = obj)},
          "xlsx" = {self$writeDataToXlsx(obj = obj)},
          stop({cat(sprintf("Error: Files of type %s are not supported.", self$suffix))})
      )
})

####################
# compound functions
####################
pgu.exporter$set("public", "export", function(filteredObj = "pgu.data",
                                              transformedObj = "pgu.data",
                                              scaledObj = "pgu.data",
                                              revisedObj = "pgu.data",
                                              transformatorObj = "pgu.transformator",
                                              modelObj = "pgu.model",
                                              naObj = "pgu.missings",
                                              outlierObj = "pgu.outliers",
                                              regressionObj = "pgu.regressor",
                                              correlationObj = "pgu.correlator",
                                              optimizerObj = "pgu.optimizer"){
  switch (self$exportType,
          "Filtered-Data" = {self$exportData(filteredObj$rawData)},
          "Filtered-Statistics" = {self$exportData(filteredObj$dataStatistics())},
          "Tested-Transformations" = {self$exportData(optimizerObj$trafoAlpahbetTblDf())},
          "Optimal-Transformations" = {self$exportData(optimizerObj$optTypes)},
          "Optimized-Transformation-Parameter" = {self$exportData(optimizerObj$optParameter)},
          "Transformed-Data" = {
            self$exportData(transformedObj$rawData)
            transformedObj$rawData %>%
              head() %>%
              print()
            },
          "Transformation-Parameter" = {self$exportData(transformatorObj$trafoParameter)},
          
          # "Transformed" = {self$exportData(transformedObj$rawData)},
          # "Scaled" = {self$exportData(scaledObj$rawData)},
          # "Revised" = {self$exportData(revisedObj$rawData)},
          # "Transformation" = {self$exportData(transformatorObj$trafoParameter)},
          # "Model" = {self$exportData(modelObj$modelParameter)},
          # "Missings" = {self$exportData(naObj$missings)},
          # "Missing-Statistics" = {self$exportData(naObj$missingsParameter)},
          # "Outliers" = {self$exportData(outlierObj$outliers %>%
          #                                 dplyr::select(-(color)))},
          # "Outlier-Statistics" = {self$exportData(outlierObj$outliersStatistics)},
          # "Regression" = {print("regression")},
          # "Correlation" = {print("correlation")},
          # "Complete" = {print("complete")},
          # "Complete" = {.self$exportDataAnalysis(obj = obj, transformator = transformator, model = model, cleaner = cleaner, optimizer = optimizer)},
          # "Raw" = {.self$exportData(obj = obj[[1]])},
          # "Transformed" = {.self$exportData(obj = obj[[2]])},
          # "Tidy" = {.self$exportData(obj = obj[[3]])},
          # "Model" = {.self$exportModel(obj = model)},
          # "Transformator" = {.self$exportTransformator(obj = transformator)},
          # "NANs" = {.self$exportNaCandidates(obj = cleaner)},
          # "Outliers" = {.self$exportOutlierCandidates(obj = optimizer)},
          stop({sprintf("Error: Content of type %s are not supported.", self$exportType)})
  )
})