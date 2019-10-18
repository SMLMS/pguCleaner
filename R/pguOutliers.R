library("R6")
library("tidyverse")
library("outliers")
library("grDevices")
library("gplots")
library("DT")

pgu.outliers <- R6::R6Class("pgu.outliers",
                              ####################
                              # instance variables
                              ####################
                               private = list(
                                 .outliersParameter = "tbl_df",
                                 .outliers = "tbl_df",
                                 .cleaningAgentAlphabet = "character",
                                 .cleaningAgent = "factor",
                                 .seed = "numeric",
                                 .featureData = "numeric",
                                 .alpha = "numeric",
                                 .minSamples = "numeric",
                                 .outliersStatistics = "tbl_df"
                               ),
                              ##################
                              # accessor methods
                              ##################
                               active = list(
                                 outliersParameter = function(){
                                   return(private$.outliersParameter)
                                 },
                                 outliers = function(){
                                   return(private$.outliers)
                                 },
                                 cleaningAgentAlphabet = function(){
                                   return(private$.cleaningAgentAlphabet)
                                 },
                                 cleaningAgent = function(){
                                   return(as.character(private$.cleaningAgent))
                                 },
                                 setCleaningAgent = function(agent = "character"){
                                   private$.cleaningAgent <- factor(agent, levels = self$cleaningAgentAlphabet)
                                 },
                                 seed = function(){
                                   return(private$.seed)
                                 },
                                 setSeed = function(value = "numeric"){
                                   private$.seed <- value
                                 },
                                 featureData = function(){
                                   return(private$.featureData)
                                 },
                                 alpha = function(){
                                   return(private$.alpha)
                                 },
                                 setAlpha = function(value = "numeric"){
                                   private$.alpha <- value
                                 },
                                 minSamples = function(){
                                   return(private$.minSamples)
                                 },
                                 outliersStatistics = function(){
                                   return(private$.outliersStatistic)
                                 }
                                 ),
                              ###################
                              # memory management
                              ###################
                               public = list(
                                 initialize = function(data = "tbl_df"){
                                   self$setAlpha <- 0.05
                                   private$.minSamples <- 6
                                   private$.cleaningAgentAlphabet <- c("none", "median", "mean", "mu", "mc", "knn", "pmm", "cart", "rf", "amelia")
                                   self$setSeed <- 42.0
                                   self$setCleaningAgent <- self$cleaningAgentAlphabet[1]
                                   self$resetOutliersParameter(data)
                                 },
                                 finalize = function(){
                                   print("Instance of pgu.outliers removed from heap")
                                 },
                                 ##########################
                                 # print instance variables
                                 ##########################
                                 print = function(){
                                   rString <- sprintf("\npgu.outliers\n")
                                   cat(rString)
                                   uString <- sprintf("\nseed: %.3e\ncleaningAgent: %s\noutliers:\n", self$seed, self$cleaningAgent)
                                   cat(uString)
                                   print(self$outliers)
                                   print(self$outliersParameter)
                                   cat("\n\n")
                                   invisible(self)
                                 }
                                 )
)
####################
# public functions
####################
pgu.outliers$set("public", "resetOutliersParameter", function(data = "tbl_df"){
  numericData <- data %>%
    dplyr::select_if(is.numeric)
  features <- numericData %>%
    colnames()
  measurements <- c(rep(0.0, length(features)))
  existings <- c(rep(0.0, length(features)))
  outliers <- c(rep(0.0, length(features)))
  fractionOfOutliers <- c(rep(0.0, length(features)))
  private$.outliersParameter <- tibble::tibble(features, measurements, existings, outliers, fractionOfOutliers)
  private$.outliers <- tibble::tibble(measurement = numeric(0),
                                     feature = character(0),
                                     values = numeric(0),
                                     type = character(0),
                                     color = character(0)) %>%
    dplyr::mutate_if(is.numeric, round, 8)
  self$findOutliers(data)
})
##################
# helper functions
##################
pgu.outliers$set("public", "featureIdx", function(feature = "character"){
  idx <- match(feature, self$modelParameter[["features"]])
  if(is.na(idx)){
    rString <- sprintf("\nWarning in pgu.outliers: feature %s is not known\n",
                       feature)
    cat(rString)
  }
  return(idx)
})

#################
# detect outliers
#################
pgu.outliers$set("public", "runGrubbs", function(data = "tbl_df", feature = "character"){
  result <- outliers::grubbs.test(self$featureData, opposite = FALSE)
  if(result$p.value < self$alpha){
    if (grepl("lowest", result$alternative)){
      col <- "blue"
      t <- "min"
      idx <- which.min(self$featureData)
    }
    else{
      col <- "firebrick"
      t <- "max"
      idx <- which.max(self$featureData)
    }
    value <- data[[idx, feature]]
    private$.outliers <- tibble::add_row(self$outliers,
                                        measurement = as.numeric(idx),
                                        feature = as.character(feature),
                                        values = as.numeric(value),
                                        type = as.character(t),
                                        color = as.character(col))

    private$.featureData[idx] <- NA
    return (TRUE)
  }
  else{
    return (FALSE)
  }
})

pgu.outliers$set("public", "findOutliers", function(data = "tbl_df"){
  for (feature in self$outliersParameter[["features"]]){
    private$.featureData <- data[[feature]]
    foundOutlier <- TRUE
    while (foundOutlier) {
      foundOutlier <- self$runGrubbs(data, feature)
    }
  }
  #.self$calcStatistics(obj = obj)
})