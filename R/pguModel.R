library("R6")
library("tidyverse")
source(file = "R/pguNormDist.R", local=TRUE)

pgu.model <- R6::R6Class("pgu.model",
                              ####################
                              # instance variables
                              ####################
                               private = list(
                                 .modelList = "pgu.normDist",
                                 .modelParameter = "tbl_df"
                               ),
                              ##################
                              # accessor methods
                              ##################
                               active = list(
                                 modelList = function(){
                                   return(private$.modelList)
                                 },
                                 modelParameter = function(){
                                   return(private$.modelParameter)
                                 }
                                 ),
                              ###################
                              # memory management
                              ###################
                               public = list(
                                 initialize = function(data = "tbl_df"){
                                   self$resetModelParameter(data)
                                   self$resetModelList(data)
                                 },
                                 finalize = function(){
                                   print("Instance of pgu.model removed from heap")
                                 },
                                 ##########################
                                 # print instance variables
                                 ##########################
                                 print = function(){
                                   rString <- sprintf("\npgu.model\n")
                                   cat(rString)
                                   cat("modelParameter:\n")
                                   print(self$modelParameter)
                                   cat("\n\n")
                                   invisible(self)
                                 }
                                 )
)
####################
# public functions
####################
pgu.model$set("public", "resetModelParameter", function(data  = "tbl_df"){
  features <- data %>%
    dplyr::select_if(is.numeric) %>%
    colnames()
  
  mu <- c(rep(0.0, length(features)))
  sigma <- c(rep(0.0, length(features)))
  # fit quality
  dataPoints <- c(rep(0.0, length(features)))
  logLikelihood <-  c(rep(0.0, length(features)))
  degOfFreedom <- c(rep(0.0, length(features)))
  bic <- c(rep(0.0, length(features)))
  aic <- c(rep(0.0, length(features)))
  aicc <- c(rep(0.0, length(features)))
  rmse <- c(rep(0.0, length(features)))
  # shapiro wilk test parameters
  w.shapiro <- c(rep(0.0, length(features)))
  p.shapiro <- c(rep(0.0, length(features)))
  # kolmogorow smirnow test parameters
  d.kolmogorow <- c(rep(0.0, length(features)))
  p.kolmogorow <- c(rep(0.0, length(features)))
  # anderson darling test parameters
  a.anderson <- c(rep(0.0, length(features)))
  p.anderson <- c(rep(0.0, length(features)))
  # create modelParameter
  private$.modelParameter <- tibble::tibble(features, mu, sigma, dataPoints, logLikelihood, degOfFreedom, bic, aic, aicc, rmse,
                                         w.shapiro, p.shapiro, d.kolmogorow, p.kolmogorow, a.anderson, p.anderson)

})

pgu.model$set("public", "resetModelList", function(data = "tbl_df"){
  results=list()
  for (feature in self$modelParameter[["features"]]){
    idx <- self$featureIdx(feature)
    if(!is.na(idx)){
      results[feature] <- list(pgu.normDist$new(data[feature]))
    }
  }
  private$.modelList <- results
})

##################
# helper functions
##################
pgu.model$set("public", "featureIdx", function(feature = "character"){
  idx <- match(feature, self$modelParameter[["features"]])
  if(is.na(idx)){
    rString <- sprintf("\nWarning in pgu.model: feature %s is not known\n",
                       feature)
    cat(rString)
  }
  return(idx)
})

###############
# fit functions
###############
pgu.model$set("public", "fitFeature", function(feature = "character"){
  idx <- match(feature, self$modelParameter[["features"]])
  if(!is.na(idx)){
    private$.modelList[[feature]]$fit()
  }
})

pgu.model$set("public", "fitData", function(){
  for (feature in self$modelParameter[["features"]]){
    idx <- self$featureIdx(feature)
    if(!is.na(idx)){
      self$fitFeature(feature)
    }
  }
})

#############
# log results
#############
pgu.model$set("public", "logFitResultsFeature", function(feature = "character"){
  idx <- self$featureIdx(feature)
  if(!is.na(idx)){
    private$.modelParameter[idx, "mu"] <- private$.modelList[[feature]]$expMu
    private$.modelParameter[idx, "sigma"] <- private$.modelList[[feature]]$expSigma
    private$.modelParameter[idx, "logLikelihood"] <- private$.modelList[[feature]]$logLikelihood
    private$.modelParameter[idx, "dataPoints"] <- private$.modelList[[feature]]$dataPoints
    private$.modelParameter[idx, "degOfFreedom"] <- private$.modelList[[feature]]$degOfFreedom
    private$.modelParameter[idx, "bic"] <- private$.modelList[[feature]]$bic
    private$.modelParameter[idx, "aic"] <- private$.modelList[[feature]]$aic
    private$.modelParameter[idx, "aicc"] <- private$.modelList[[feature]]$aicc
    private$.modelParameter[idx, "rmse"] <- private$.modelList[[feature]]$rmse
    private$.modelParameter[idx, "w.shapiro"] <- private$.modelList[[feature]]$w.shapiro
    private$.modelParameter[idx, "p.shapiro"] <- private$.modelList[[feature]]$p.shapiro
    private$.modelParameter[idx, "d.kolmogorow"] <- private$.modelList[[feature]]$d.kolmogorow
    private$.modelParameter[idx, "p.kolmogorow"] <- private$.modelList[[feature]]$p.kolmogorow
    private$.modelParameter[idx, "a.anderson"] <- private$.modelList[[feature]]$a.anderson
    private$.modelParameter[idx, "p.anderson"] <- private$.modelList[[feature]]$p.anderson
  }
})

pgu.model$set("public", "logFitResultsData", function(){
  for (feature in self$modelParameter[["features"]]){
    idx <- self$featureIdx(feature)
    if(!is.na(idx)){
      self$logFitResultsFeature(feature)
    }
  }
})

####################
# compound functions
####################
pgu.model$set("public", "fit", function(){
  self$fitData()
  self$logFitResultsData()
})

############
# scale data
############
pgu.model$set("public", "scaleNumeric", function(value = "numeric", feature = "character"){
  idx <- self$featureIdx(feature)
  if(!is.na(idx)){
    mu <- private$.modelParameter[[idx, "mu"]]
    sigma <- private$.modelParameter[[idx, "sigma"]]
    value <- (value - mu)/sigma
  }
  return(value)
})

pgu.model$set("public", "scaleData", function(data = "tbl_df"){
  for (feature in self$modelParameter[["features"]]){
    idx <- self$featureIdx(feature)
    if(!is.na(idx)){
      data[feature] <- self$scaleNumeric(data[[feature]], feature)
    }
  }
  return(data)
})
##############
# rescale data
##############
pgu.model$set("public", "rescaleNumeric", function(value = "numeric", feature = "character"){
  idx <- self$featureIdx(feature)
  if(!is.na(idx)){
    mu <- private$.modelParameter[[idx, "mu"]]
    sigma <- private$.modelParameter[[idx, "sigma"]]
    value <- (value * sigma) + mu
  }
  return(value)
})

pgu.model$set("public", "rescaleData", function(data = "tbl_df"){
  for (feature in self$modelParameter[["features"]]){
    idx <- self$featureIdx(feature)
    if(!is.na(idx)){
      data[feature] <- self$rescaleNumeric(data[[feature]], feature)
    }
  }
  return(data)
})

###############
# print results
###############

##############
# plot results
##############
