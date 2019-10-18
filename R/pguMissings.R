library("R6")
library("tidyverse")
library("mice")
library("VIM")
library("DMwR")
library("Amelia")

pgu.missings <- R6::R6Class("pgu.missings",
                              ####################
                              # instance variables
                              ####################
                               private = list(
                                 .missingsParameter = "tbl_df",
                                 .missings = "tbl_df",
                                 .cleaningAgentAlphabet = "character",
                                 .cleaningAgent = "factor",
                                 .seed = "numeric",
                                 .amv = "ANY"
                               ),
                              ##################
                              # accessor methods
                              ##################
                               active = list(
                                 missingsParameter = function(){
                                   return(private$.missingsParameter)
                                 },
                                 missings = function(){
                                   return(private$.missings)
                                 },
                                 cleaningAgentAlphabet = function(){
                                   return(private$.cleaningAgentAlphabet)
                                 },
                                 cleaningAgent = function(){
                                   return(as.character(private$.cleaningAgent))
                                 },
                                 setCleaningAgent = function(agent = "character") {
                                   private$.cleaningAgent <- factor(agent, levels = self$cleaningAgentAlphabet)
                                 },
                                 seed = function(){
                                   return(private$.seed)
                                 },
                                 setSeed = function(value = "numeric"){
                                   private$.seed <- value
                                 },
                                 amv = function(){
                                   return(private$.amv)
                                 }
                                 ),
                              ###################
                              # memory management
                              ###################
                               public = list(
                                 initialize = function(data = "tbl_df"){
                                   private$.cleaningAgentAlphabet <- c("none", "median", "mean", "mu", "mc", "knn", "pmm", "cart", "rf", "amelia", "amelia_bound")
                                   self$setSeed <- 42.0
                                   self$setCleaningAgent <- self$cleaningAgentAlphabet[1]
                                   self$resetMissingsParameter(data)
                                 },
                                 finalize = function(){
                                   print("Instance of pgu.missings removed from heap")
                                 },
                                 ##########################
                                 # print instance variables
                                 ##########################
                                 print = function(){
                                   rString <- sprintf("\npgu.missings\n")
                                   cat(rString)
                                   uString <- sprintf("\nseed: %.3e\ncleaningAgent: %s\nmissings:\n", self$seed, as.character(self$cleaningAgent))
                                   cat(uString)
                                   print(self$missingsParameter)
                                   print(self$missings)
                                   cat("\n\n")
                                   invisible(self)
                                 }
                                 )
)
####################
# public functions
####################
pgu.missings$set("public", "resetMissingsParameter", function(data = "tbl_df"){
  numericData <- data %>%
    dplyr::select_if(is.numeric)
  features <- numericData %>%
    colnames()
  measurements <- c(rep(0.0, length(features)))
  existings <- c(rep(0.0, length(features)))
  missings <- c(rep(0.0, length(features)))
  fractionOfMissings <- c(rep(0.0, length(features)))
  private$.missingsParameter <- tibble::tibble(features, measurements, existings, missings, fractionOfMissings)
  private$.amv <- VIM::aggr(numericData, plot=FALSE)
  self$gatherMissingsStatistics(data)
  self$findMissings(data)
})
##################
# helper functions
##################
pgu.missings$set("public", "featureIdx", function(feature = "character"){
  idx <- match(feature, self$missingsParameter[["features"]])
  if(is.na(idx)){
    rString <- sprintf("\nWarning in pgu.missings: feature %s is not known\n",
                       feature)
    cat(rString)
  }
  return(idx)
})

pgu.missings$set("public", "filterFeatures", function(data = "tbl_df"){
  data %>%
    dplyr::select(private$.missingsParameter[["features"]]) %>%
    return()
})

pgu.missings$set("public", "missingsIdxByFeature", function(featureName = "character"){
  self$missings %>%
    dplyr::filter(features == featureName) %>%
    dplyr::select(row) %>%
    unlist() %>%
    unname() %>%
    as.integer() %>%
    return()
})
#####################
# missings statistics
#####################
pgu.missings$set("public", "gatherMeasurements", function(value = "numeric"){
  return(length(value))
})

pgu.missings$set("public", "gatherMissings", function(value = "numeric"){
  y <- sum(is.na(value))
  return(y)
})

pgu.missings$set("public", "gatherExistings", function(value = "numeric"){
  y <- sum(!is.na(value))
  return(y)
})

pgu.missings$set("public", "gatherFractionOfMissings",  function(value = "numeric"){
  y <- 100.0*sum(is.na(value))/length(value)
  return(y)
})

pgu.missings$set("public", "gatherMissingsStatistics", function(data = "tbl_df"){
  filteredData <- data %>%
    self$filterFeatures()
  private$.missingsParameter["measurements"] <- filteredData %>%
    apply(MARGIN=2, FUN=self$gatherMeasurements)
  private$.missingsParameter["existings"] <- filteredData %>%
    apply(MARGIN=2, FUN=self$gatherExistings)
  private$.missingsParameter["missings"] <- filteredData %>%
    apply(MARGIN=2, FUN=self$gatherMissings)
  private$.missingsParameter["fractionOfMissings"] <- filteredData %>%
    apply(MARGIN=2, FUN=self$gatherFractionOfMissings)
})

################
# detect missing
################
pgu.missings$set("public", "findMissings", function(data = "tbl_df"){
  private$.missings <- data %>%
    self$filterFeatures() %>%
    is.na() %>%
    which(arr.ind=TRUE) %>%
    tibble::as_tibble() %>%
    dplyr::mutate(features = self$missingsParameter[["features"]][col])
})

#################
# handle missings
#################
pgu.missings$set("public", "handleMissings", function(data = "tbl_df"){
  if(is.na(self$cleaningAgent)){
    print("Warning: Error in pgu.missings cleaningAgent is not valid. Will be set to none.")
    self$setCleaningAgent <- "none"
  }
  cleanedData <- switch((self$cleaningAgent),
                       "none" = data,
                       "median" = self$cleanByMedian(data),
                       "mean" = self$cleanByMean(data),
                       "mu" = self$cleanByExpectationValue(data),
                       "mc" = self$cleanByMC(data),
                       "knn" = self$cleanByKnn(data),
                       "pmm" = self$cleanByPmm(data),
                       "cart" = self$cleanByCart(data),
                       "rf" = self$cleanByRf(data),
                       "amelia" = self$cleanByAmelia(data),
                       "amelia_bound" = self$cleanByAmeliaBound(data)
  )
  return(cleanedData)
})

pgu.missings$set("public", "cleanByMedian", function(data = "tbl_df"){
  for (feature in self$missingsParameter[["features"]]){
    indices <- self$missingsIdxByFeature(feature)
    data <- data %>%
      dplyr::mutate(!!feature := replace(!!as.name(feature),
                                         indices,
                                         stats::median(!!as.name(feature), na.rm = TRUE)))
  }
  return(data)
})

pgu.missings$set("public", "cleanByMean", function(data = "tbl_df"){
  for (feature in self$missingsParameter[["features"]]){
    indices <- self$missingsIdxByFeature(feature)
    data <- data %>%
      dplyr::mutate(!!feature := replace(!!as.name(feature),
                                         indices,
                                         mean(!!as.name(feature), na.rm = TRUE)))
  }
  return(data)
})

pgu.missings$set("public", "cleanByExpectationValue", function(data = "tbl_df"){
  for (feature in self$missingsParameter[["features"]]){
    indices <- self$missingsIdxByFeature(feature)
    data <- data %>%
      dplyr::mutate(!!feature := replace(!!as.name(feature),
                                         indices,
                                         0.0))
  }
  return(data)
})

pgu.missings$set("public", "cleanByMC", function(data = "tbl_df"){
  for (feature in self$missingsParameter[["features"]]){
    indices <- self$missingsIdxByFeature(feature)
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


pgu.missings$set("public", "cleanByKnn", function(data = "tbl_df"){
  data %>%
    as.data.frame() %>%
    DMwR::knnImputation(k=3,
                        scale = TRUE,
                        meth = "weighAvg",
                        distData = NULL) %>%
    tibble::as_tibble() %>%
    return()
})


pgu.missings$set("public", "cleanByPmm", function(data = "tbl_df"){
  data %>%
    as.data.frame() %>%
    mice::mice(m=5,
               maxit = 5,
               method = "pmm",
               seed = self$seed) %>%
    mice::complete(action = 1) %>%
    tibble::as_tibble() %>%
    return()
})

pgu.missings$set("public", "cleanByCart", function(data = "tbl_df"){
  data %>%
    as.data.frame() %>%
    mice::mice(minbucket = 3,
               method = "cart",
               seed = self$seed) %>%
    mice::complete(action = 1) %>%
    tibble::as_tibble() %>%
    return()
})

pgu.missings$set("public", "cleanByRf", function(data = "tbl_df"){
  data %>%
    as.data.frame() %>%
    mice::mice(ntree = 3,
               method = "rf",
               seed = self$seed) %>%
    mice::complete(action = 1) %>%
    tibble::as_tibble() %>%
    return()
})

pgu.missings$set("public", "cleanByAmelia", function(data = "tbl_df"){
  ameliaOutput <- data %>%
    Amelia::amelia(m = 1, parallel = "multicore", ncpus = 8)
  ameliaOutput$imputations[[1]] %>%
    tibble::as_tibble() %>%
    return()
})

pgu.missings$set("public", "cleanByAmeliaBound", function(data = "tbl_df"){
  tblBounds = data.frame(column=c(1:ncol(data)),
                         lower=c(0),
                         upper=sapply(data,stats::quantile,probs = c(0.25),names=FALSE, na.rm = TRUE)) %>%
    as.matrix()
  ameliaOutput <- data %>%
    Amelia::amelia(m = 1,bounds=tblBounds, parallel = "multicore", ncpus = 8)
  ameliaOutput$imputations[[1]] %>%
    tibble::as_tibble() %>%
    return()
})

########
# output
########
pgu.missings$set("public", "nanDistribution", function(data = "tbl_df"){
  d <- data %>%
    self$filterFeatures() %>%
    mice::md.pattern(plot=FALSE)
  return(d)
})

pgu.missings$set("public", "nanPositives", function(data = "tbl_df"){
  data %>%
    self$filterFeatures() %>%
    dplyr::filter_all(dplyr::any_vars(is.na(.))) %>%
    return()
})

################
# plot functions
################
pgu.missings$set("public", "nanHeatmap", function(){
  p <- plot(self$amv,
            col=c('navyblue','red'),
            numbers=TRUE,
            sortVars=TRUE,
            labels=self$missingsParameter[["features"]],
            cex.axis=.7,
            gap=3,
            ylab=c("Histogram of missing data","Pattern"))
  return(p)
})
