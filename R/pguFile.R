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
                          #.suffixList = "character",
                          .content = "character",
                          #.contentList = "character",
                          #.outFileName = "character",
                          .sheetIndex = "numeric",
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
                          content = function(){
                            return(private$.content)
                          },
                          setContent = function(val = "character"){
                            private$.content <- val
                          },
                          sheetIndex = function(){
                            return(private$.sheetIndex)
                          },
                          setSheetIndex = function(val = "numeric"){
                            private$.sheetIndex <- val
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
                            private$.content <- character(length(0))
                            private$.sheetIndex <- 0
                            private$.separator <- character(length(0))
                            private$.skipRows <- 0
                            private$.header = FALSE
                            private$.naChar <- character(length(0))
                          },
                          finalize = function(){
                            print("Instance of pgu.fileDesign removed from heap")
                          },
                          ##########################
                          # print instance variables
                          ##########################
                          print = function(){
                            rString <- sprintf("\npgu.file\nuploadFileName: %s\nfileName: %s\nbaseName: %s\nfolderName: %s\nsuffix: %s\ncontent: %s\nsheetIndex: %i\nseparator: %s\nskipRows: %i\nheader: %s\nnchar: %s\n\n",
                                               self$uploadFileName,
                                               self$fileName,
                                               self$baseName,
                                               self$folderName,
                                               self$suffix,
                                               self$content,
                                               self$sheetIndex,
                                               self$separator,
                                               self$skipRows,
                                               self$header,
                                               self$naChar
                            )
                            cat(rString)
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