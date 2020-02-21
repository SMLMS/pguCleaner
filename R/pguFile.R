library("R6")
library("tools")

pgu.file <- R6::R6Class("pgu.file",
                        ####################
                        # instance variables
                        ####################
                        private = list(
                          .uploadFileName = "character",
                          .fileName = "character",
                          .baseName = "character",
                          .folderName = "character",
                          .suffix = "character",
                          .exportType = "character",
                          .timeString = "character",
                          .sheetIndex = "numeric",
                          .dataSheet = "character",
                          .loqSheet = "characetr",
                          .metadataSheet = "character",
                          .separator = "character",
                          .skipRows = "numeric",
                          .header = "logical",
                          .naChar ="character"
                        ),
                        ##################
                        # accessor methods
                        ##################
                        active = list(
                          uploadFileName = function(){
                            return(private$.uploadFileName)
                          },
                          setUploadFileName = function(val = "character"){
                            private$.uploadFileName <- val
                          },
                          fileName = function(){
                            return(private$.fileName)
                          },
                          setFileName = function(val = "character"){
                            private$.fileName <- val
                          },
                          baseName = function(){
                            return(private$.baseName)
                          },
                          setBaseName = function(val = "chatacter"){
                            private$.baseName <- val
                          },
                          folderName = function(){
                            return(private$.folderName)
                          },
                          setFolderName = function(val = "character"){
                            private$.folderName <- val
                          },
                          suffix = function(){
                            return(private$.suffix)
                          },
                          setSuffix = function(val = "character"){
                            private$.suffix <- val
                          },
                          exportType = function(){
                            return(private$.exportType)
                          },
                          setExportType = function(val = "character"){
                            private$.exportType <- val
                          },
                          timeString = function(){
                            return(private$.timeString)
                          },
                          sheetIndex = function(){
                            return(private$.sheetIndex)
                          },
                          setSheetIndex = function(val = "numeric"){
                            private$.sheetIndex <- val
                          },
                          dataSheet = function(){
                            return(private$.dataSheet)
                          },
                          setDataSheet = function(val = "character"){
                            private$.dataSheet <- val
                          },
                          loqSheet = function(){
                            return(private$.loqSheet)
                          },
                          setLoqSheet = function(val = "character"){
                            private$.loqSheet <- val
                          },
                          metadataSheet = function(){
                            return(private$.metadataSheet)
                          },
                          setMetadataSheet = function(val = "character"){
                            private$.metadataSheet <- val
                          },
                          separator = function(){
                            return(private$.separator)
                          },
                          setSeparator = function(val = "character"){
                            private$.separator <- val
                          },
                          skipRows = function(){
                            return(private$.skipRows)
                          },
                          setSkipRows = function(val = "numeric"){
                            private$.skipRows <- val
                          },
                          header = function(){
                            return(private$.header)
                          },
                          setHeader = function(val = "logical"){
                            private$.header <- val
                          },
                          naChar = function(){
                            return(private$.naChar)
                          },
                          setNaChar = function(val = "character"){
                            private$.naChar <- val
                          }
                        ),
                        ###################
                        # memory management
                        ###################
                        public = list(
                          initialize = function(){
                            private$.uploadFileName <- character(length(0))
                            private$.fileName <- character(length(0))
                            private$.baseName <- character(length(0))
                            private$.folderName <- character(length(0))
                            private$.suffix <- character(length(0))
                            private$.exportType <- character(length(0))
                            private$.sheetIndex <- 0
                            private$.dataSheet <- "raw_data"
                            private$.loqSheet <- "loq"
                            private$.metadataSheet <- "metadata"
                            private$.separator <- character(length(0))
                            private$.skipRows <- 0
                            private$.header = TRUE
                            private$.naChar <- "NA"
                            self$updateTimeString()
                          },
                          finalize = function(){
                            print("Instance of pgu.fileDesign removed from heap")
                          },
                          ##########################
                          # print instance variables
                          ##########################
                          print = function(){
                            rString <- sprintf("\npgu.file\n")
                            cat(rString)
                            fString <- sprintf("uploadFileName: %s\nfileName: %s\nbaseName: %s\nsuffix: %s\nexportType: %s\ntimeString: %s\nsheetIndex: %i\nseparator: %s\nskipRows: %i\nheader: %s\nnaChar: %s\n",
                                               self$uploadFileName,
                                               self$fileName,
                                               self$baseName,
                                               self$suffix,
                                               self$exportType,
                                               self$timeString,
                                               self$sheetIndex,
                                               self$separator,
                                               self$skipRows,
                                               self$header,
                                               self$naChar
                            )
                            cat(fString)
                            cat("\n\n")
                            invisible(self)
                          }
                        )
)
####################
# public functions
####################
pgu.file$set("public", "splitFileName", function(){
  self$setBaseName <- tools::file_path_sans_ext(basename(self$fileName))
  self$setFolderName <- dirname(self$fileName)
  self$setSuffix <- tools::file_ext(self$fileName)
})

pgu.file$set("public", "updateTimeString", function(){
  private$.timeString <- Sys.time() %>%
    format("%y%m%d-%H%M%S")
})

pgu.file$set("public", "mergeFileName", function(){
  private$.fileName <- sprintf("%s_%s_%s.%s",
                                  self$baseName,
                                  self$exportType,
                                  self$timeString,
                                  self$suffix)
})

pgu.file$set("public", "bluntFileName", function(value = "character"){
  self$updateTimeString()
  sprintf("%s_%s_%s",
          self$baseName,
          value,
          self$timeString) %>%
    return()
})