library("R6")
library("tidyverse")
library("rcompanion")
library("MASS")
#library("forcats")

pgu.transformator <- R6::R6Class("pgu.transformator",
                              ####################
                              # instance variables
                              ####################
                               private = list(
                                 .trafoAlphabet = "character",
                                 .trafoParameter = "tbl_df",
                                 .modelParameter = "tbl_df"
                               ),
                              ##################
                              # accessor methods
                              ##################
                               active = list(
                                 trafoAlphabet = function(){
                                   return(private$.trafoAlphabet)
                                 },
                                 trafoParameter = function(){
                                   return(private$.trafoParameter)
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
                                   if(class(data) != "tbl_df"){
                                     data <- tibble::tibble(names <- "none",
                                                            values <- c(NA))
                                   }
                                   private$.trafoAlphabet <-c("none", "log2", "logNorm", "log10", "squareRoot", "cubeRoot", "arcsine", "inverse", "tukeyLOP", "boxCox")
                                   self$resetTrafoParameter(data)
                                 },
                                 finalize = function(){
                                   print("Instance of pgu.transformator removed from heap")
                                 },
                                 ##########################
                                 # print instance variables
                                 ##########################
                                 print = function(){
                                   rString <- sprintf("\npgu.transformator\n")
                                   cat(rString)
                                   cat("trafoAlphabet:\n")
                                   print(private$.trafoAlphabet)
                                   cat("\ntrafoParameter:\n")
                                   print(private$.trafoParameter)
                                   cat("\nmodelParameter\n")
                                   print(private$.modelParameter)
                                   cat("\n\n")
                                   invisible(self)
                                 }
                                 )
)
####################
# public functions
####################
pgu.transformator$set("public", "resetTrafoParameter", function(data  = "tbl_df"){
  features <- data %>%
    dplyr::select_if(is.numeric) %>%
    colnames()
  trafoType <- c(rep(self$trafoAlphabet[1], length(features))) %>%
    factor(level = self$trafoAlphabet)
  mirrorLogic <- c(rep(FALSE, length(features )))
  addConst <- c(rep(0.0, length(features )))
  lambda <- c(rep(1.0, length(features )))
  private$.trafoParameter <- tibble::tibble(features = as.character(features),
                                        trafoType = as.factor(trafoType),
                                        addConst = as.numeric(addConst),
                                        mirrorLogic = as.logical(mirrorLogic),
                                        lambda = as.numeric(lambda))
})

##########################
# trafoParameter accessors
##########################
pgu.transformator$set("public", "trafoType", function(feature = "character"){
  t <- "none"
  idx <- self$featureIdx(feature)
  if(!is.na(idx)){
    t <- self$trafoParameter[[idx, "trafoType"]]
  }
  return(t)
})

pgu.transformator$set("public", "setTrafoType", function(feature = "character", type = "character"){
  idx <- self$featureIdx(feature)
  t <- factor(type, levels = self$trafoAlphabet)
  if(is.na(t)){
    rString <- sprintf("\nWarning in pgu.transformator$setTrafoType: type %s is not known\n",
                       type)
    cat(rString)
  }
  else if (!is.na(idx)){
    private$.trafoParameter[idx, "trafoType"] <- t
  }
})

pgu.transformator$set("public", "addConstant", function(feature = "character"){
  c <- 0.0
  idx <- self$featureIdx(feature)
  if(!is.na(idx)){
    c <- self$trafoParameter[idx, "addConst"]
  }
  return(c)
})

pgu.transformator$set("public", "mirrorLogic", function(feature = "character"){
  l <- FALSE
  idx <- self$featureIdx(feature)
  if(!is.na(idx)){
    l <- self$trafoParameter[idx, "mirrorLogic"]
  }
  return(l)
})

pgu.transformator$set("public", "setMirrorLogic", function(feature = "character", logic = "logical"){
  idx <- self$featureIdx(feature)
  if(!is.na(idx)){
    private$.trafoParameter[idx, "mirrorLogic"] <- logic
  }
})


pgu.transformator$set("public", "lambda", function(feature = "character"){
  l <- 0.0
  idx <- self$featureIdx(feature)
  if(!is.na(idx)){
    l <- self$trafoParameter[idx, "lambda"]
  }
  return(l)
})

##################
# helper functions
##################
pgu.transformator$set("public", "featureIdx", function(feature = "character"){
  idx <- match(feature, self$trafoParameter[["features"]])
  if(is.na(idx)){
    rString <- sprintf("\nWarning in pgu.transformator: feature %s is not known\n",
                       feature)
    cat(rString)
  }
  return(idx)
})

pgu.transformator$set("public", "addConstGenerator", function(value = "numeric"){
  c <- 0.0
  if (value <= 0.0){
    c <- (-1.0 * value) + .Machine$double.xmin
  }
  return(c)
})

##########################
# mirror functions
##########################
pgu.transformator$set("public", "mirrorNumeric", function(value = "numeric"){
  return(-1.0 * value)
})

pgu.transformator$set("public", "mirrorData", function(data = "tbl_df"){
  #tbl <- data
  for (feature in self$trafoParameter[["features"]]){
    idx <- self$featureIdx(feature)
    if((!is.na(idx)) & (as.logical(self$trafoParameter[idx,"mirrorLogic"]))){
      data[feature] <- self$mirrorNumeric(data[[feature]])
    }
  }
  return(data)
})

##########################
# translation functions
##########################
pgu.transformator$set("public", "calculateAddConst", function(data = "tbl_df"){
  private$.trafoParameter["addConst"] <- data %>%
    dplyr::select(self$trafoParameter[["features"]]) %>%
    self$mirrorData() %>%
    dplyr::summarise_all(min, na.rm=TRUE) %>%
    dplyr::mutate_all(self$addConstGenerator) %>%
    t() %>%
    as.numeric()
})

pgu.transformator$set("public", "translateNumeric", function(value = "numeric", const = "numeric"){
  return(value + const)
})

pgu.transformator$set("public", "translateData", function(data = "tbl_df"){
  for (feature in self$trafoParameter[["features"]]){
    idx <- self$featureIdx(feature)
    if(!is.na(idx)){
      data[feature] <- self$translateNumeric(data[[feature]],
                                             as.numeric(self$trafoParameter[idx, "addConst"]))
    }
  }
  return(data)
})

pgu.transformator$set("public", "backTranslateNumeric", function(value = "numeric", const = "numeric"){
  return(value - const)
})

pgu.transformator$set("public", "backTranslateData", function(data = "tbl_df"){
  for (feature in self$trafoParameter[["features"]]){
    idx <- self$featureIdx(feature)
    if(!is.na(idx)){
      data[feature] <- self$backTranslateNumeric(data[[feature]],
                                                 as.numeric(self$trafoParameter[idx, "addConst"]))
    }
  }
  return(data)
})


###################
# lambda estimation
###################
pgu.transformator$set("public", "lambdaEstimator", function(value = "numeric", feature = "character"){
  lambda <- 1.0
  idx <- self$featureIdx(feature)
  if(!is.na(idx)){
    method <- self$trafoParameter[[idx, "trafoType"]]
    switch(as.character(method),
           none = {lambda <- 1.0},
           log2 = {lambda <- 1.0},
           logNorm = {lambda <- 1-0},
           log10 = {lambda <- 1.0},
           squareRoot = {lambda <- 1.0},
           cubeRoot = {lambda <- 1.0},
           arcsine = {lambda <- self$normalizeArcSine(value)},
           inverse = {lambda <- 1.0},
           tukeyLOP = {lambda <- self$optimizeTukeyLadderOfPowers(value)},
           boxCox = {lambda <- self$optimizeBoxCox(value)},
           {rString <- sprintf("\nWarning in pgu.transformator: trafoType %s is not known\n",as.character(method))
           cat(rString)}
    )
  }
  return(lambda)
})

pgu.transformator$set("public", "estimateLambda", function(data = "tbl_df"){
  tempData <- data %>%
    dplyr::select(self$trafoParameter[["features"]]) %>%
    self$mirrorData() %>%
    self$translateData()
  private$.trafoParameter["lambda"] <- lapply(self$trafoParameter[["features"]], function(x){self$lambdaEstimator(value = tempData[[x]], feature = x)}) %>%
    t() %>%
    as.numeric()
})


pgu.transformator$set("public", "normalizeArcSine", function(value  = "numeric"){
  return(max(value, na.rm=TRUE))
})

pgu.transformator$set("public", "optimizeTukeyLadderOfPowers", function(value = "numeric"){
  lambda <- 1.0
  tryCatch({
    lambda <- rcompanion::transformTukey(value,
                                         start = -10,
                                         end = 10,
                                         int = 0.025,
                                         plotit = FALSE,
                                         verbose = FALSE,
                                         quiet = TRUE,
                                         statistic = 1,
                                         returnLambda = TRUE)
    return(lambda)
  },
  warning = function(w) {
    warningMessage <- sprintf("Warning: Could not optimize Tukey Ladder Of Powers Lambda is set to 1.0:\n%s", w)
    warning(warningMessage)
    return(lambda)
  },
  error = function(e) {
    warningMessage <- sprintf("Warning: Could not optimize Tukey Ladder Of Powers Lambda is set to 1.0:\n%s", e)
    warning(warningMessage)
    return(lambda)
  })
})

pgu.transformator$set("public", "optimizeBoxCox", function(value = "numeric"){
  lambda <- 1.0
  tryCatch({
    logLikelihoodVector <- MASS::boxcox(value ~ 1,
                                        lambda = seq(-10, 10, 0.1),
                                        plotit = FALSE)
    logLikelihood <- tibble::tibble(logLikelihoodVector$x, logLikelihoodVector$y)
    colnames(logLikelihood) <- c("lambda", "logLikelihood")
    maxLikelihoodIdx <- which(logLikelihood[, "logLikelihood"] == max(logLikelihood[, "logLikelihood"], na.rm = TRUE), arr.ind = TRUE)
    lambda <- logLikelihood[[maxLikelihoodIdx[1], "lambda"]]
    return(lambda)
  },
  warning = function(w) {
    errorMessage <- sprintf("Warning: Could not optimize Box Cox Lambda is set to 1.0:\n%s", w)
    warning(errorMessage)
    return(lambda)
  },
  error = function(e) {
    errorMessage <- sprintf("Warning: Could not optimize Box Cox Lambda is set to 1.0:\n%s", e)
    warning(errorMessage)
    return(lambda)
  })
})

##########################
# transformation functions
##########################
pgu.transformator$set("public", "transformNumeric", function(value = "numeric", feature = "character"){
  tf <- numeric()
  idx <- self$featureIdx(feature)
  if(!is.na(idx)){
    method <- self$trafoParameter[[idx, "trafoType"]]
      switch(as.character(method),
             none = {tf <- value},
             log2 = {tf <- self$transformLogModulus(value, base=2)},
             logNorm = {tf <- self$transformLogModulus(value, base=exp(1))},
             log10 = {tf <- self$transformLogModulus(value, base=10)},
             squareRoot = {tf <- self$transformSquareRoot(value)},
             cubeRoot = {tf <- self$transformCubeRoot(value)},
             arcsine = {tf <- self$transformArcsine(value, lambda=self$trafoParameter[[idx,"lambda"]])},
             inverse = {tf <- self$transformInverse(value)},
             tukeyLOP = {tf <- self$transformTukeyLadderOfPowers(value, lambda=self$trafoParameter[[idx,"lambda"]])},
             boxCox = {tf <- self$transformBoxCox(value, lambda=self$trafoParameter[[idx,"lambda"]])},
             {private$.trafoParameter[idx, "trafoType"] <- "none"
             tf <- value
             rString <- sprintf("\nWarning in pgu.transformator: trafoType %s is not known\n",as.character(method))
             cat(rString)}
      )
  }
  return (tf)
})

pgu.transformator$set("public", "transformData", function(data = "tbl_df"){
  for (feature in self$trafoParameter[["features"]]){
    idx <- self$featureIdx(feature)
    if(!is.na(idx)){
      data[feature] <- self$transformNumeric(data[[feature]], feature)
    }
  }
  return(data)
})

pgu.transformator$set("public", "transformLogModulus", function(value = "numeric", base="numeric"){
  return(log(value, base=base))
})

pgu.transformator$set("public", "transformSquareRoot", function(value = "numeric"){
  return(sqrt(value))
})

pgu.transformator$set("public", "transformCubeRoot", function(value = "numeric"){
  return((value)^(1/3))
})

pgu.transformator$set("public", "transformArcsine", function(value = "numeric", lambda="numeric"){
  return(asin(sqrt((value)/lambda)))
})

pgu.transformator$set("public", "transformInverse", function(value = "numeric"){
  return(1.0/(value))
})

pgu.transformator$set("public", "transformTukeyLadderOfPowers", function(value = "numeric", lambda="numeric"){
  if(lambda > 0){
    return((value)^lambda)
  }else if(lambda == 0){
    return(self$transformLogModulus(value, base=exp(1)))
  }else {
    return(-1.0*((value)^lambda))
  }
})

pgu.transformator$set("public", "transformBoxCox", function(value = "numeric", lambda="numeric"){
  if (lambda == 0){
    return(self$transformLogModulus(value, base=exp(1)))
  } else{
    return(((value)^lambda -1) / lambda)
  }
})
##################################
# inverse transfromation functions
##################################
pgu.transformator$set("public", "inverseTransformNumeric", function(value = "numeric", feature = "character"){
  tf <- numeric()
  idx <- self$featureIdx(feature)
  if(!is.na(idx)){
    method <- self$trafoParameter[[idx, "trafoType"]]
    switch(as.character(method),
           none = {tf <- value},
           log2 = {tf <- self$inverseTransformLogModulus(value, base=2)},
           logNorm = {tf <- self$inverseTransformLogModulus(value, base=exp(1))},
           log10 = {tf <- self$inverseTransformLogModulus(value, base=10)},
           squareRoot = {tf <- self$inverseTransformSquareRoot(value)},
           cubeRoot = {tf <- self$inverseTransformCubeRoot(value)},
           arcsine = {tf <- self$inverseTransformArcsine(value, lambda=self$trafoParameter[[idx,"lambda"]])},
           inverse = {tf <- self$inverseTransformInverse(value)},
           tukeyLOP = {tf <- self$inverseTransformTukeyLadderOfPowers(value, lambda=self$trafoParameter[[idx,"lambda"]])},
           boxCox = {tf <- self$inverseTransformBoxCox(value, lambda=self$trafoParameter[[idx,"lambda"]])},
           {private$.trafoParameter[idx, "trafoType"] <- "none"
           tf <- value
           rString <- sprintf("\nWarning in pgu.transformator: trafoType %s is not known\n",as.character(method))
           cat(rString)}
    )
  }
  return (tf)
})

pgu.transformator$set("public", "inverseTransformData", function(data = "tbl_df"){
  for (feature in self$trafoParameter[["features"]]){
    idx <- self$featureIdx(feature)
    if(!is.na(idx)){
      data[feature] <- self$inverseTransformNumeric(data[[feature]], feature)
    }
  }
  return(data)
})

pgu.transformator$set("public", "inverseTransformLogModulus", function(value = "numeric", base="numeric"){
  return (base^value)
})

pgu.transformator$set("public", "inverseTransformSquareRoot", function(value = "numeric"){
  return(value^2)
})

pgu.transformator$set("public", "inverseTransformCubeRoot", function(value = "numeric"){
  return(value^3)
})

pgu.transformator$set("public", "inverseTransformArcsine", function(value = "numeric", lambda="numeric"){
  return(((sin(value))^2.0)*lambda)
})

pgu.transformator$set("public", "inverseTransformInverse", function(value = "numeric"){
  return(1.0/value)
})

pgu.transformator$set("public", "inverseTransformTukeyLadderOfPowers", function(value = "numeric", lambda="numeric"){
  if(lambda > 0){
    return(value^(1/lambda))
  }else if(lambda == 0){
    return(self$inverseTransformLogModulus(value, base=exp(1)))
  }else {
    return(((-1.0*value)^(1/lambda)))
  }
})

pgu.transformator$set("public", "inverseTransformBoxCox", function(value = "numeric", lambda="numeric"){
  if (lambda == 0){
    return(self$inverseTransformLogModulus(value, base=exp(1)))
  } else{
    return(((value*lambda)+1)^(1/lambda))
  }
})

##################
# compound methods
##################
pgu.transformator$set("public", "estimateTrafoParameter", function(data = "tbl_df"){
  data %>%
    self$calculateAddConst()
  data %>%
    self$estimateLambda()
})

pgu.transformator$set("public", "mutateData", function(data = "tbl_df"){
  data %>%
    dplyr::select(self$trafoParameter[["features"]]) %>%
    self$mirrorData() %>%
    self$translateData() %>%
    self$transformData() %>%
    return()
})

pgu.transformator$set("public", "reverseMutateData", function(data = "tbl_df"){
  data %>%
    dplyr::select(self$trafoParameter[["features"]]) %>%
    self$inverseTransformData() %>%
    self$backTranslateData() %>%
    self$mirrorData() %>%
    return()
})