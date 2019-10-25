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
                                   return(private$.outliersStatistics)
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
                                   print(self$outliersStatistics)
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

pgu.outliers$set("public", "filterFeatures", function(data = "tbl_df"){
  data %>%
    dplyr::select(private$.outliersParameter[["features"]]) %>%
    return()
})

pgu.outliers$set("public", "outliersIdxByFeature", function(featureName = "character"){
  self$outliers %>%
    dplyr::filter(feature == featureName) %>%
    dplyr::pull(measurement) %>%
    as.integer() %>%
    return()
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
  self$outlierStatistics(data)
})

####################
# outlier statistics
####################
pgu.outliers$set("public", "outlierStatistics", function(data = "tbl_df"){
  absCount <- data %>%
    self$filterFeatures() %>%
    dplyr::summarise_all(~sum(!is.na(.)))
  private$.outliersStatistics <- tibble::tibble(features = colnames(absCount),
                                              absCount = absCount %>%
                                                unlist() %>%
                                                as.numeric())
  outlierCount <- c(rep(0.0, length(self$outliersParameter[["features"]])))
  minCount <- c(rep(0.0, length(self$outliersParameter[["features"]])))
  maxCount <- c(rep(0.0, length(self$outliersParameter[["features"]])))
  if(nrow(self$outliers) > 0){
    for (i in seq(from = 1,to = length(self$outliersParameter[["features"]]), by =1)) {
      outlierCount[i] <- sum(self$outliers["feature"] == self$outliersParameter[[i, "features"]])
      minCount[i] <- self$outliers %>%
        dplyr::filter(type == "min" & feature == self$outliersParameter[[i, "features"]]) %>%
        dplyr::select("feature") %>%
        dplyr::count() %>%
        unlist() %>%
        as.numeric()
      maxCount[i] <- self$outliers %>%
        dplyr::filter(type == "max" & feature == self$outliersParameter[[i, "features"]]) %>%
        dplyr::select("feature") %>%
        dplyr::count() %>%
        unlist() %>%
        as.numeric()
      
    }
  }
  private$.outliersStatistics <- self$outliersStatistics %>%
    dplyr::mutate(outlierCount = outlierCount,
                  low = minCount,
                  high = maxCount,
                  outlierFraction = 100* outlierCount/absCount) %>%
    dplyr::mutate(minFraction = 100 * low/absCount,
                  maxFraction = 100 * high/absCount)
})

#################
# handle outliers
#################
pgu.outliers$set("public", "insertImputationSites", function(data = "tbl_df"){
  for(feature in self$outliersParameter[["features"]]){
    indices <- self$outliers %>%
      dplyr::filter(!!feature == feature) %>%
      dplyr::pull(measurement) %>%
      as.integer()
    data <- data %>%
      dplyr::mutate(!!feature := replace(x = !!as.name(feature),
                                         list = indices,
                                         values = NA))
  }
  return(data)
})

pgu.outliers$set("public", "handleOutliers", function(data = "tbl_df"){
  cleanedData <- switch(self$cleaningAgent,
                       "none" = {data},
                       "median" = {data %>%
                         self$filterFeatures() %>%
                         self$insertImputationSites() %>%
                         self$cleanByMedian()},
                       "mean" = {data %>%
                         self$filterFeatures() %>%
                         self$insertImputationSites() %>%
                         self$cleanByMean()},
                       "mu" = {data %>%
                         self$filterFeatures() %>%
                         self$insertImputationSites() %>%
                         self$cleanByExpectationValue()},
                       "mc" = {data %>%
                         self$filterFeatures() %>%
                         self$insertImputationSites() %>%
                         self$cleanByMC()},
                       "knn" = {data %>%
                         self$filterFeatures() %>%
                         self$insertImputationSites() %>%
                         self$cleanByKnn()},
                       "pmm" = {data %>%
                         self$filterFeatures() %>%
                         self$insertImputationSites() %>%
                         self$cleanByPmm()},
                       "cart" = {data %>%
                         self$filterFeatures() %>%
                         self$insertImputationSites() %>%
                         self$cleanByCart()},
                       "rf" = {data %>%
                         self$filterFeatures() %>%
                         self$insertImputationSites() %>%
                         self$cleanByRf()},
                       "amelia" = {data %>%
                         self$filterFeatures() %>%
                         self$insertImputationSites() %>%
                         self$cleanByAmelia()}
  )
  data %>%
    dplyr::select(-dplyr::one_of(self$outliersParameter[["features"]])) %>%
    dplyr::bind_cols(cleanedData) %>%
    dplyr::select(colnames(data)) %>%
    return()
})

pgu.outliers$set("public", "cleanByMedian", function(data = "tbl_df"){
  for (feature in self$outliersParameter[["features"]]){
    indices <- self$outliersIdxByFeature(feature)
    data <- data %>%
      dplyr::mutate(!!feature := replace(!!as.name(feature),
                                         indices,
                                         stats::median(!!as.name(feature), na.rm = TRUE)))
  }
  return(data)
})

pgu.outliers$set("public", "cleanByMean", function(data = "tbl_df"){
  for (feature in self$outliersParameter[["features"]]){
    indices <- self$outliersIdxByFeature(feature)
    data <- data %>%
      dplyr::mutate(!!feature := replace(!!as.name(feature),
                                         indices,
                                         mean(!!as.name(feature), na.rm = TRUE)))
  }
  return(data)
})

pgu.outliers$set("public", "cleanByExpectationValue", function(data = "tbl_df"){
  for (feature in self$outliersParameter[["features"]]){
    indices <- self$outliersIdxByFeature(feature)
    data <- data %>%
      dplyr::mutate(!!feature := replace(!!as.name(feature),
                                         indices,
                                         0.0))
  }
  return(data)
})

pgu.outliers$set("public", "cleanByMC", function(data = "tbl_df"){
  for (feature in self$outliersParameter[["features"]]){
    indices <- self$outliersIdxByFeature(feature)
    mcVal <- stats::rnorm(n = length(indices),
                          mean = 0.0,
                          sd = 1.0)
    data <- data %>%
      dplyr::mutate(!!feature := replace(!!as.name(feature),
                                         indices,
                                         mcVal))
  }
  return(data)
})


pgu.outliers$set("public", "cleanByKnn", function(data = "tbl_df"){
  cleanedData <- data %>%
    as.data.frame() %>%
    DMwR::knnImputation(k=3,
                        scale = TRUE,
                        meth = "weighAvg",
                        distData = NULL) %>%
      tibble::as_tibble()
    
    for (feature in self$outliersParameter[["features"]]){
      indices <- self$outliersIdxByFeature(feature)
      data %>%
        dplyr::slice(indices) %>%
        dplyr::pull(feature)

      
      data <- data %>%
        dplyr::mutate(!!feature := replace(!!as.name(feature),
                                           indices,
                                           cleanedData %>%
                                             dplyr::slice(indices) %>%
                                             dplyr::pull(feature)))
    }
    return(data)
})


pgu.outliers$set("public", "cleanByPmm", function(data = "tbl_df"){
  cleanedData <- data %>%
    as.data.frame() %>%
    mice::mice(m=5,
               maxit = 5,
               method = "pmm",
               seed = self$seed) %>%
    mice::complete(action = 1) %>%
    tibble::as_tibble()
  
  for (feature in self$outliersParameter[["features"]]){
    indices <- self$outliersIdxByFeature(feature)
    data %>%
      dplyr::slice(indices) %>%
      dplyr::pull(feature)
    
    data <- data %>%
      dplyr::mutate(!!feature := replace(!!as.name(feature),
                                         indices,
                                         cleanedData %>%
                                           dplyr::slice(indices) %>%
                                           dplyr::pull(feature)))
  }
  return(data)
})

pgu.outliers$set("public", "cleanByCart", function(data = "tbl_df"){
  cleanedData <- data %>%
    as.data.frame() %>%
    mice::mice(minbucket = 3,
               method = "cart",
               seed = self$seed) %>%
    mice::complete(action = 1) %>%
    tibble::as_tibble()
  
  for (feature in self$outliersParameter[["features"]]){
    indices <- self$outliersIdxByFeature(feature)
    data %>%
      dplyr::slice(indices) %>%
      dplyr::pull(feature)
    
    data <- data %>%
      dplyr::mutate(!!feature := replace(!!as.name(feature),
                                         indices,
                                         cleanedData %>%
                                           dplyr::slice(indices) %>%
                                           dplyr::pull(feature)))
  }
  return(data)
})

pgu.outliers$set("public", "cleanByRf", function(data = "tbl_df"){
  cleanedData <- data %>%
    as.data.frame() %>%
    mice::mice(ntree = 3,
               method = "rf",
               seed = self$seed) %>%
    mice::complete(action = 1) %>%
    tibble::as_tibble()
  
  for (feature in self$outliersParameter[["features"]]){
    indices <- self$outliersIdxByFeature(feature)
    data %>%
      dplyr::slice(indices) %>%
      dplyr::pull(feature)
    
    data <- data %>%
      dplyr::mutate(!!feature := replace(!!as.name(feature),
                                         indices,
                                         cleanedData %>%
                                           dplyr::slice(indices) %>%
                                           dplyr::pull(feature)))
  }
  return(data)
})

pgu.outliers$set("public", "cleanByAmelia", function(data = "tbl_df"){
  ameliaOutput <- data %>%
    Amelia::amelia(m = 1, parallel = "multicore", ncpus = 8)
  cleanedData <- ameliaOutput$imputations[[1]] %>%
    tibble::as_tibble()
  
  for (feature in self$outliersParameter[["features"]]){
    indices <- self$outliersIdxByFeature(feature)
    data %>%
      dplyr::slice(indices) %>%
      dplyr::pull(feature)
    
    data <- data %>%
      dplyr::mutate(!!feature := replace(!!as.name(feature),
                                         indices,
                                         cleanedData %>%
                                           dplyr::slice(indices) %>%
                                           dplyr::pull(feature)))
  }
  return(data)
})