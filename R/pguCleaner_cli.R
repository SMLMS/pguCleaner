rm(list=ls())
library("getopt")
source(file = "R/pguFile.R", local=TRUE)
source(file = "R/pguImporter.R", local=TRUE)
source(file = "R/pguFilter.R", local=TRUE)
source(file = "R/pguTransformator.R", local=TRUE)
source(file = "R/pguModel.R", local=TRUE)
source(file = "R/pguOptimizer.R", local=TRUE)
source(file = "R/pguMissings.R", local=TRUE)
source(file = "R/pguOutliers.R", local=TRUE)
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
  filterSet <- pgu.filter$new(data = rawData)
  colIdx <- seq(1,3,1)
  rowIdx <- seq(1,5,1)
  #filterSet$setColIdx <- colIdx
  #filterSet$setRowIdx <- rowIdx
  print(filterSet)
  rawData %>%
    filterSet$filter() %>%
    head() %>%
    print()
  transformator <- pgu.transformator$new(data = rawData)
  #transformator$setMirrorLogic(feature = "TG_42.0", logic = TRUE)
  #transformator$setMirrorLogic(feature = "TG_46.1", logic = TRUE)
  transformator$setTrafoType(feature = "TG_42.0", type = "logNorm")
  transformator$setTrafoType(feature = "TG_42.1", type = "logNorm")
  transformator$setTrafoType(feature = "TG_44.0", type = "logNorm")
  transformator$setTrafoType(feature = "TG_44.1", type = "logNorm")
  transformator$setTrafoType(feature = "TG_44.2", type = "logNorm")
  transformator$setTrafoType(feature = "TG_46.0", type = "logNorm")
  transformator$setTrafoType(feature = "TG_46.1", type = "logNorm")
  transformator$setTrafoType(feature = "TG_46.2", type = "logNorm")
  rawData %>%
   transformator$estimateTrafoParameter()
  # print(transformator)
  # rawData %>%
  #   dplyr::select_if(is.numeric) %>%
  #   transformator$mirrorData() %>%
  #   transformator$translateData() %>%
  #   print()

  # rawData %>%
  #   transformator$mutateData() %>%
  #   transformator$reverseMutateData() %>%
  #   print()
  # model <- pgu.model$new(data = rawData)
  # normDist <- pgu.normDist$new(rawData %>%
  #                                #transformator$mutateData() %>%
  #                                dplyr::select("TG_42.0"))
  # normDist$fit()
  # plot(normDist)
 
  
  # optimizer <- pgu.optimizer$new(data = rawData)
  # optimizer$optimize(rawData)
  # optimizer$optParameter %>%
  #   dplyr::select(features,
  #                 w.shapiro,
  #                 p.shapiro,
  #                 d.kolmogorow,
  #                 p.kolmogorow,
  #                 a.anderson,
  #                 p.anderson) %>%
  #   print()
  # 
  # optimizer$optTypes %>%
  #   dplyr::select(features,
  #                 w.shapiro,
  #                 p.shapiro,
  #                 d.kolmogorow,
  #                 p.kolmogorow,
  #                 a.anderson,
  #                 p.anderson) %>%
  #   print()
  
  transformedData  <- rawData %>%
    transformator$mutateData()
  model <- pgu.model$new(data = transformedData)
  model$fit()
  scaledData <- transformedData %>%
    model$scaleData()
  # missings <- pgu.missings$new(data = scaledData)
  # missings$setCleaningAgent <- "mean"
  # print(missings$handleMissings(data = scaledData))
  outliers <- pgu.outliers$new(data =rawData)
  print(outliers)
  return(EXIT_SUCCESS)
}

main()
