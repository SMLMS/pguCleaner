library("R6")
library("tidyverse")
library("stats")
library("robust")
library("DT")
library("gridExtra")

pgu.regressor <- R6::R6Class("pgu.regressor",
                              ####################
                              # instance variables
                              ####################
                               private = list(
                                 .featureNames = "character",
                                 .intercept = "matrix",
                                 .pIntercept = "matrix",
                                 .slope = "matrix",
                                 .pSlope = "matrix",
                                 .model = "lmRob"
                                 #.model = "lmRob"
                               ),
                              ##################
                              # accessor methods
                              ##################
                               active = list(
                                 featureNames = function(){
                                   return(private$.featureNames)
                                 },
                                 setFeatureNames = function(names = "character"){
                                   private$.featureNames <- names
                                   private$.intercept <- self$resetMatrix(value = 0)
                                   private$.pIntercept <- self$resetMatrix(value = 1)
                                   private$.slope <- self$resetMatrix(value = 0)
                                   private$.pSlope <- self$resetMatrix(value = 1)
                                 },
                                 ordinateFeatureNames = function(){
                                   return(private$.ordinateFeatureNames)
                                 },
                                 intercept = function(){
                                   return(private$.intercept)
                                 },
                                 pIntercept = function(){
                                   return(private$.pIntercept)
                                 },
                                 slope = function(){
                                   return(private$.slope)
                                 },
                                 pSlope = function(){
                                   return(private$.pSlope)
                                 },
                                 model = function(){
                                   return(private$.model)
                                 }
                                 ),
                              ###################
                              # memory management
                              ###################
                               public = list(
                                 initialize = function(data = "tbl_df"){
                                   if(class(data)[1] != "tbl_df"){
                                     data <- tibble::tibble(names <- "none",
                                                            values <- c(NA))
                                   }
                                   self$resetRegressor(data)
                                 },
                                 finalize = function(){
                                   print("Instance of pgu.regressor removed from heap")
                                 },
                                 ##########################
                                 # print instance variables
                                 ##########################
                                 print = function(){
                                   rString <- sprintf("\npgu.regressor\n")
                                   cat(rString)
                                   fString <- sprintf("\nfeatureNames:\n")
                                   cat(fString)
                                   print(self$featureNames)
                                   iString <- sprintf("\nintercept matrix:\n")
                                   cat(iString)
                                   print(self$intercept)
                                   sString <- sprintf("\nslope matrix:\n")
                                   cat(sString)
                                   print(self$slope)
                                   pString <- sprintf("\np-Value matrix:\n")
                                   cat(pString)
                                   print(self$pValue)
                                   cat("\n\n")
                                   invisible(self)
                                 }
                                 )
)
####################
# public functions
####################
pgu.regressor$set("public", "resetRegressor", function(data = "tbl_df", progress = "Progress"){
  self$setFeatureNames <- data %>%
    dplyr::select_if(is.numeric) %>%
    colnames()
  self$createRegressionMatrix(data, progress)
})

pgu.regressor$set("public", "resetDiagonal", function(data = "matrix"){
  if(nrow(data) == ncol(data)){
    for (i in 1:nrow(data)){
      data[i,i] <- NA
    }
  }
  else{
    print("Warning: Regressor matrix needs to be square")
  }
  return(data)
})

pgu.regressor$set("public", "resetMatrix", function(value = "numeric"){
  n = length(self$featureNames)
  df <- matrix(data = value,
               nrow = n,
               ncol = n,
               dimnames = list(self$featureNames, self$featureNames))
  if(sum(dim(df)) > 0){
    self$resetDiagonal(df)
  }
  return(df)
})
##################
# helper functions
##################
pgu.regressor$set("public", "featureIdx", function(feature = "character"){
  idx <- match(c(feature), self$featureNames)
  if(is.na(idx)){
    rString <- sprintf("\nWarning in pgu.regressor: feature %s is not known\n",
                       feature)
    cat(rString)
  }
  return(idx)
})

pgu.regressor$set("public", "featureIsValid", function(feature = "character"){
  idx <- self$featureIdx(feature)
  if(is.na(idx)){
    return(FALSE)
  }
  else{
    return(TRUE)
  }
})

pgu.regressor$set("public", "featurePairIsValid", function(abscissa = "character", ordinate = "character"){
  val <- TRUE
  if(!self$featureIsValid(abscissa)){val <- FALSE}
  if(!self$featureIsValid(ordinate)){val <- FALSE}
  if(abscissa == ordinate){val <- FALSE}
  return(val)
})

####################################
# robust libear regression functions
####################################
pgu.regressor$set("public", "createModel", function(data = "tbl_df", abscissa = "character", ordinate = "character"){
  if(self$featurePairIsValid(abscissa, ordinate)){
    ord <- paste0("`", ordinate, "`")
    abs <- paste0("`", abscissa, "`")
    private$.model <- paste(ord, abs, sep = "~") %>%
      stats::as.formula() %>%
      # stats::lm(data, na.action = na.omit)
      robust::lmRob(data, na.action = na.omit)
  }
})


pgu.regressor$set("public", "createRegressionMatrix", function(data = "tbl_df", progress = "Progress"){
  for (abscissa in self$featureNames){
    for (ordinate in self$featureNames){
      if(("shiny" %in% (.packages())) & (class(progress)[1] == "Progress")){
        progress$inc(1)
      }
      abs <- sym(abscissa)
      ord <- sym(ordinate)
      if(self$featurePairIsValid(abscissa, ordinate)){
        self$createModel(data, abscissa, ordinate)
        private$.intercept[[as.name(ord), as.name(abs)]] <- as.numeric(c(summary(self$model)$coefficients[1,1]))
        private$.pIntercept[[as.name(ord), as.name(abs)]] <-as.numeric(c(summary(self$model)$coefficients[1,4]))
        private$.slope[[as.name(ord), as.name(abs)]] <- as.numeric(c(summary(self$model)$coefficients[2,1]))
        private$.pSlope[[as.name(ord), as.name(abs)]] <-as.numeric(c(summary(self$model)$coefficients[2,4]))
      }
      else{
        private$.intercept[[as.name(ord), as.name(abs)]] <- NA
        private$.pIntercept[[as.name(ord), as.name(abs)]] <-NA
        private$.slope[[as.name(ord), as.name(abs)]] <- NA
        private$.pSlope[[as.name(ord), as.name(abs)]] <-NA
      }
    }
  }
})

#################
# print functions
#################
pgu.regressor$set("public","printModel", function(abscissa = "character", ordinate = "character"){
  t <- NULL
  if(self$featurePairIsValid(abscissa, ordinate)){
  abs <- sym(abscissa)
  ord <- sym(ordinate)
  para <- c("abscissa", "ordinate", "intercept", "p.intercept", "slope", "p.slope")
  val <- c(abscissa,
           ordinate,
           sprintf("%.3e", self$intercept[[as.name(ord), as.name(abs)]]),
           sprintf("%.3e", self$pIntercept[[as.name(ord), as.name(abs)]]),
           sprintf("%.3e", self$slope[[as.name(ord), as.name(abs)]]),
           sprintf("%.3e", self$pSlope[[as.name(ord), as.name(abs)]])
             )
  t <- tibble::tibble(parameter = para,
                 vlaue = val)
  }
  return(t)
})

pgu.regressor$set("public", "printInterceptTbl", function(){
  self$intercept %>%
    tibble::as_tibble() %>%
    dplyr::mutate(features = self$featureNames) %>%
    dplyr::select(features, dplyr::everything()) %>%
    return()
})

pgu.regressor$set("public", "printPInterceptTbl", function(){
  self$pIntercept %>%
    tibble::as_tibble() %>%
    dplyr::mutate(features = self$featureNames) %>%
    dplyr::select(features, dplyr::everything()) %>%
    return()
})

pgu.regressor$set("public", "printSlopeTbl", function(){
  self$slope %>%
    tibble::as_tibble() %>%
    dplyr::mutate(features = self$featureNames) %>%
    dplyr::select(features, dplyr::everything()) %>%
    return()
})

pgu.regressor$set("public", "printPSlopeTbl", function(){
  self$pSlope %>%
    tibble::as_tibble() %>%
    dplyr::mutate(features = self$featureNames) %>%
    dplyr::select(features, dplyr::everything()) %>%
    return()
})


################
# plot functions
################
pgu.regressor$set("private", "plotRegression", function(){
  abs <- sym(names(self$model$model)[2])
  ord <- sym(names(self$model$model)[1])
  p <- ggplot2::ggplot(data = self$model$model,
                       aes_string(x=as.name(abs),
                                  y=as.name(ord)),
                       na.rm = TRUE)+
    ggplot2::geom_point()+
    ggplot2::stat_smooth(method = "lm") +
    ggplot2::ggtitle("Robust Model\nLinear Regression")
  return(p)
})

pgu.regressor$set("private", "plotResidualDist", function(){
  p <- tibble::enframe(self$model$residuals, name=c("index")) %>%
    ggplot2::ggplot(mapping=ggplot2::aes_string(x="value"), na.rm=TRUE)+
    ggplot2::geom_histogram() +
    ggplot2::ggtitle("Residuals\nBar Plot") +
    ggplot2::theme(axis.title.y = element_blank())
  return(p)
})

pgu.regressor$set("private", "plotResidualBox", function(){
  p <- tibble::enframe(self$model$residuals, name=c("index")) %>%
    ggplot2::ggplot(mapping=ggplot2::aes_string(y="value"), na.rm=TRUE)+
    ggplot2::geom_boxplot()+
    ggplot2::ggtitle("Residuals\nBox Plot") +
    ggplot2::theme(axis.title.x = element_blank(),
                   axis.text.x = element_blank(),
                   axis.ticks.x = element_blank(),
                   axis.title.y = element_blank())
  return(p)
})

pgu.regressor$set("public", "plotModel", function(data = "tbl_df", abscissa = "character", ordinate = "character"){
  p <- NULL
  if(self$featurePairIsValid(abscissa, ordinate)){
    self$createModel(data, abscissa, ordinate)
    p1 <- private$plotRegression()
    p2 <- private$plotResidualBox()
    p3 <- private$plotResidualDist()
    p <- gridExtra::grid.arrange(p1,p2,p3, layout_matrix = rbind(c(1,1,1,2),c(1,1,1,3)))
  }
  return(p)
})