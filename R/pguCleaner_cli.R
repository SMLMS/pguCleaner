rm(list=ls())
library("getopt")
source(file = "R/pguFile.R", local=TRUE)
source(file = "R/pguImporter.R", local=TRUE)
EXIT_SUCCESS = 0
EXIT_FAILURE = 1



main <- function(){
  spec = matrix(c(
    "fileName", "f", 1, "character",
    "sheetIndex", "i", 1, "numeric",
    "separator", "s", 1, "character",
    "topHeader", "t", 0, "logical",
    "nanChar", "n", 1, "character",
    "verbose", "v", 0, "logical"
  ), byrow=TRUE, ncol=4)
  
  opt = getopt(spec);
  if(is.null(opt$fileName)){
    print("No fileName provided. Going to sleep")
    return(EXIT_FAILURE)
  }
  
  if(is.null(opt$sheetIndex)){opt$sheetIndex <- 1}
  if(is.null(opt$separator)){opt$separator <- ","}
  if(is.null(opt$topHeader)){opt$topHeader <- FALSE}
  if(is.null(opt$nanChar)){opt$nanChar <- "nan"}
    
  
  suffixList <- c("csv","h5", "hdf", "hdf5", "txt", "xls", "xlsx")

  inFile <- pgu.file$new()
  importer <- pgu.importer$new()
  
  inFile$setFileName <- opt$fileName
  inFile$splitFileName()
  inFile$setSheetIndex <- opt$sheetIndex
  inFile$setSeparator <-opt$separator
  inFile$setNaChar <- opt$nanChar
  inFile$setHeader <- opt$topHeader
  print(inFile)
  
  print(importer$suffixIsKnown(obj = inFile))
  rawData <- importer$importHdf5(obj = inFile)
  print(head(rawData))
  return(EXIT_SUCCESS)
}

main()
