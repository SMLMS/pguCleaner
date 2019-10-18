library("R6")
library("tidyverse")
source(file = "R/pguTransformator.R", local=TRUE)
source(file = "R/pguModel.R", local=TRUE)

pgu.optimizer <- R6::R6Class("pgu.optimizer",
                              ####################
                              # instance variables
                              ####################
                              private = list(
                                .features = "character",
                                .optParameter = "tbl_df",
                                .optTypes = "tbl_df"
                                
                              ),
                              ##################
                              # accessor methods
                              ##################
                              active = list(
                                features = function(){
                                  return(private$.features)
                                },
                                optParameter = function(){
                                  return(private$.optParameter)
                                },
                                optTypes = function(){
                                  return(private$.optTypes)
                                }
                              ),
                              ###################
                              # memory management
                              ###################
                              public = list(
                                initialize = function(data = "tbl_df"){
                                  self$resetFeatures(data)
                                  self$resetOptParameter(data)
                                  self$resetOptTypes(data)
                                },
                                finalize = function(){
                                  print("Instance of pgu.optimizer removed from heap")
                                },
                                ##########################
                                # print instance variables
                                ##########################
                                print = function(){
                                  rString <- sprintf("\npgu.optimizer\n")
                                  cat(rString)
                                  cat("\noptParameter\n")
                                  print(private$.optParameter)
                                  cat("\noptTypes\n")
                                  print(private$.optTypes)
                                  cat("\n\n")
                                  invisible(self)
                                }
                              )
)
####################
# public functions
####################
pgu.optimizer$set("public", "resetFeatures", function(data = "tbl_df"){
  private$.features <- data %>%
    dplyr::select_if(is.numeric) %>%
    colnames()
})

pgu.optimizer$set("public", "resetOptParameter", function(data = "tbl_df"){
  features <- append(self$features,self$features)
  mirrorLogic <- append(c(rep(FALSE, length(self$features))),  c(rep(TRUE, length(self$features))))
  logLikelihood <- as.numeric(c(rep(NA, length(features))))
  bic <- as.numeric(c(rep(NA, length(features))))
  aic <- as.numeric(c(rep(NA, length(features))))
  aicc <- as.numeric(c(rep(NA, length(features))))
  rmse <- as.numeric(c(rep(NA, length(features))))
  w.shapiro <- as.numeric(c(rep(NA, length(features))))
  p.shapiro <- as.numeric(c(rep(NA, length(features))))
  d.kolmogorow <- as.numeric(c(rep(NA, length(features))))
  p.kolmogorow <- as.numeric(c(rep(NA, length(features))))
  a.anderson <- as.numeric(c(rep(NA, length(features))))
  p.anderson <- as.numeric(c(rep(NA, length(features))))
  private$.optParameter <- tibble::tibble(features, mirrorLogic, logLikelihood, bic, aic, aicc, rmse,
                                          w.shapiro, p.shapiro, d.kolmogorow, p.kolmogorow, a.anderson, p.anderson)
})

pgu.optimizer$set("public", "resetOptTypes", function(data = "tbl_df"){
  features <- append(self$features,self$features)
  mirrorLogic <- append(c(rep(FALSE, length(self$features))),  c(rep(TRUE, length(self$features))))
  logLikelihood <- c(rep("none", length(features)))
  bic <- c(rep("none", length(features)))
  aic <- c(rep("none", length(features)))
  aicc <- c(rep("none", length(features)))
  rmse <- c(rep("none", length(features)))
  w.shapiro <- c(rep("none", length(features)))
  p.shapiro <- c(rep("none", length(features)))
  d.kolmogorow <- c(rep("none", length(features)))
  p.kolmogorow <- c(rep("none", length(features)))
  a.anderson <- c(rep("none", length(features)))
  p.anderson <- c(rep("none", length(features)))
  private$.optTypes <- tibble::tibble(features, mirrorLogic, logLikelihood, bic, aic, aicc, rmse,
                                      w.shapiro, p.shapiro, d.kolmogorow, p.kolmogorow, a.anderson, p.anderson)
})
##################
# helper functions
##################
pgu.optimizer$set("public", "featureIdx", function(){
  idx <- match(feature, self$features)
  if(is.na(idx)){
    rString <- sprintf("\nWarning in pgu.optimizer: feature %s is not known\n",
                       feature)
    cat(rString)
  }
  return(idx)
})

pgu.optimizer$set("public", "modelParameterIsBigger", function(modelParameter = "numeric", referenceParameter = "numeric"){
  result <- FALSE
  if ((!is.na(modelParameter)) &&
      ((referenceParameter < modelParameter) ||
       (is.na(referenceParameter)))){
    result <- TRUE
  }
  return(result)
})

pgu.optimizer$set("public", "modelParameterIsSmaller", function(modelParameter = "numeric", referenceParameter = "numeric"){
  result <- FALSE
  if ((!is.na(modelParameter)) &&
      ((referenceParameter > modelParameter) ||
       (is.na(referenceParameter)))){
    result <- TRUE
  }
  return(result)
})

#####################
# iteration functions
#####################
pgu.optimizer$set("public", "updateTrafoType", function(transformator = "pgu.transformator", type = "character"){
  for (feature in self$features){
    transformator$setTrafoType(feature, type)
  }
  return(transformator)
})

pgu.optimizer$set("public", "updateMirrorLogic", function(transformator = "pgu.transformator", logic = "logical"){
  for (feature in self$features){
    transformator$setMirrorLogic(feature, logic)
  }
  return(transformator)
})

##################
# update Functions
##################
pgu.optimizer$set("public", "updateOptParameter", function(model = "pgu.model", type = "character", logic = "character"){
  modelParameter <- model$modelParameter
  referenceParameter <- self$optParameter %>%
    tidyr::unite(features, features, mirrorLogic, sep="/")
  referenceTypes <- self$optTypes %>%
    tidyr::unite(features, features, mirrorLogic, sep="/")
  for(feature in model$modelParameter[["features"]]){
    referenceFeature <- paste(feature, as.character(logic), sep = "/")
    referenceIdx <- match(referenceFeature, referenceTypes[["features"]])
    modelIdx = match(feature, modelParameter[["features"]])
    for (test in c("logLikelihood", "p.shapiro", "p.kolmogorow", "p.anderson", "w.shapiro")){
      if (self$modelParameterIsBigger(modelParameter[modelIdx, test],
                                      referenceParameter[referenceIdx, test])){
        referenceTypes[referenceIdx, test] <- type
        referenceParameter[referenceIdx, test] <- modelParameter[modelIdx, test]
      }
    }
    for (test in c("bic", "aic", "aicc", "rmse", "d.kolmogorow", "a.anderson")){
      if (self$modelParameterIsSmaller(modelParameter[modelIdx, test],
                                       referenceParameter[referenceIdx, test])){
        referenceTypes[referenceIdx, test] <- type
        referenceParameter[referenceIdx, test] <- modelParameter[modelIdx, test]
      }
    }
  }
  private$.optParameter <- referenceParameter %>%
    tidyr::separate(features, into = c("features", "mirrorLogic"), sep="/") %>%
    dplyr::mutate(mirrorLogic = as.logical(mirrorLogic))
  
  private$.optTypes <- referenceTypes %>%
    tidyr::separate(features, into = c("features", "mirrorLogic"), sep="/")
})

########################
# optimization functions
########################
pgu.optimizer$set("public", "optimize", function(data  = "tbl_df"){
  transformator <- pgu.transformator$new(data)
  for (logic in c(TRUE, FALSE)){
    transformator <- self$updateMirrorLogic(transformator, logic)
    for (type  in transformator$trafoAlphabet){
    #for (type in c("none", "log2", "squareRoot")){
      transformator <- self$updateTrafoType(transformator, type)
      transformator$estimateTrafoParameter(data)
      model <- pgu.model$new(data %>%
                               transformator$mutateData())
      tryCatch({
        model$fit()
      },
      error = function(e){
        print(e)
        warning("ops")
      })
      self$updateOptParameter(model, type, logic)
    }
  }
})