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
                                 .abscissa = "character",
                                 .ordinate = "character",
                                 .robust = "logical",
                                 # .model = "lm"
                                 .model = "lmRob"
                               ),
                              ##################
                              # accessor methods
                              ##################
                               active = list(
                                 featureNames = function(){
                                   return(private$.featureNames)
                                 },
                                 setFeaturNames = function(names = "character"){
                                   private$.featureNames <- names
                                   private$.intercept <- self$resetMatrix(value = 0)
                                   private$.pIntercept <- self$resetMatrix(value = 1)
                                   private$.slope <- self$resetMatrix(value = 0)
                                   private$.pSlope <- self$resetMatrix(value = 1)
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
                                 abscissa = function(){
                                   return(private$.abscissa)
                                 },
                                 setAbscissa = function(feature = "character"){
                                   private$.abscissa <- feature
                                 },
                                 ordinate = function(){
                                   return(private$.ordinate)
                                 },
                                 setOrdinate = function(feature = "character"){
                                   private$.ordinate <- feature
                                 },
                                 robust = function(){
                                   return(private$.robust)
                                 },
                                 setRobust = function(value = "logical") {
                                   private$.robust <- value
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
                                   self$setRobust <- FALSE
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
pgu.regressor$set("public", "resetRegressor", function(data = "tbl_df"){
  private$.featureNames <- data %>%
    dplyr::select_if(is.numeric) %>%
    colnames()
  private$.intercept <- self$resetMatrix(value = 0)
  private$.pIntercept <- self$resetMatrix(value = 1)
  private$.slope <- self$resetMatrix(value = 0)
  private$.pSlope <- self$resetMatrix(value = 1)
  if(self$robust){
    self$createRobustRegressionMatrix(data)
  }
  else{
    self$createRegressionMatrix(data)
  }
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
  idx <- match(feature, self$featureNames)
  if(is.na(idx)){
    rString <- sprintf("\nWarning in pgu.optimizer: feature %s is not known\n",
                       feature)
    cat(rString)
  }
  return(idx)
})


#############################
# linear regression functions
#############################
pgu.regressor$set("public", "createModel", function(data = "tbl_df"){
  if(self$abscissa != self$ordinate){
    private$.model <- paste(self$ordinate, self$abscissa, sep = "~") %>%
      stats::as.formula() %>%
      stats::lm(data, na.action = na.omit)
  }
})


pgu.regressor$set("public", "createRegressionMatrix", function(data = "tbl_df"){
  for (abs in self$featureNames){
    for (ord in self$featureNames){
      private$.abscissa <- abs
      private$.ordinate <- ord
      if(abs != ord){
        self$createModel(data)
        private$.intercept[ord, abs] <- as.numeric(c(summary(self$model)$coefficients[1,1]))
        private$.pIntercept[ord, abs] <-as.numeric(c(summary(self$model)$coefficients[1,4]))
        private$.slope[ord, abs] <- as.numeric(c(summary(self$model)$coefficients[2,1]))
        private$.pSlope[ord, abs] <-as.numeric(c(summary(self$model)$coefficients[2,4]))
      }
    }
  }
})


####################################
# robust libear regression functions
####################################
pgu.regressor$set("public", "createRobustModel", function(data = "tbl_df"){
  if(self$abscissa != self$ordinate){
    private$.model <- paste(self$ordinate, self$abscissa, sep = "~") %>%
      stats::as.formula() %>%
      robust::lmRob(data, na.action = na.omit)
  }
})


pgu.regressor$set("public", "createRobustRegressionMatrix", function(data = "tbl_df"){
  for (abs in self$featureNames){
    for (ord in self$featureNames){
      private$.abscissa <- abs
      private$.ordinate <- ord
      if(abs != ord){
        self$createRobustModel(data)
        private$.intercept[ord, abs] <- as.numeric(c(summary(self$model)$coefficients[1,1]))
        private$.pIntercept[ord, abs] <-as.numeric(c(summary(self$model)$coefficients[1,4]))
        private$.slope[ord, abs] <- as.numeric(c(summary(self$model)$coefficients[2,1]))
        private$.pSlope[ord, abs] <-as.numeric(c(summary(self$model)$coefficients[2,4]))
      }
    }
  }
})

######################
# single feature Model
######################
pgu.regressor$set("public", "createFeatureModel", function(data = "tbl_df"){
  if(self$robust){
    self$createRobustModel(data)
  }
  else{
    self$createModel(data)
  }
})

#################
# print functions
#################
pgu.regressor$set("public","printModel", function(){
  df <- data.frame(
    abscissa = self$abscissa,
    ordinate = self$ordinate,
    intercept = self$intercept[self$ordinate, self$abscissa],
    p.Intercept = self$pIntercept[self$ordinate, self$abscissa],
    slope = self$slope[self$ordinate, self$abscissa],
    p.Slope = self$pSlope[self$ordinate, self$abscissa])%>%
    t() %>%
    as.data.frame() %>%
    tibble::rownames_to_column("regression parameter") %>%
    tibble::as_tibble() %>%
    dplyr::rename(value = "V1")
  return(df)
})

pgu.regressor$set("public", "printInterceptTbl", function(){
  self$intercept %>%
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

pgu.regressor$set("public", "printPValueTbl", function(){
  self$pValue %>%
    tibble::as_tibble() %>%
    dplyr::mutate(features = self$featureNames) %>%
    dplyr::select(features, dplyr::everything()) %>%
    return()
})


################
# plot functions
################
pgu.regressor$set("private", "plotRegression", function(){
  p <- ggplot2::ggplot(data = self$model$model,
                       aes_string(x=names(self$model$model)[2],
                                  y=names(self$model$model)[1]),
                       na.rm = TRUE)+
    ggplot2::geom_point()+
    ggplot2::stat_smooth(method = "lm")
  return(p)
})

pgu.regressor$set("private", "plotResidualDist", function(){
  p <- tibble::enframe(self$model$residuals, name=c("index")) %>%
    ggplot2::ggplot(mapping=ggplot2::aes_string(x="value"), na.rm=TRUE)+
    ggplot2::geom_histogram()
  return(p)
})

pgu.regressor$set("private", "plotResidualBox", function(){
  p <- tibble::enframe(self$model$residuals, name=c("index")) %>%
    ggplot2::ggplot(mapping=ggplot2::aes_string(y="value"), na.rm=TRUE)+
    ggplot2::geom_boxplot()+
    ggplot2::theme(axis.title.x = element_blank(),
                   axis.text.x = element_blank(),
                   axis.ticks.x = element_blank(),
                   axis.title.y = element_blank())
  return(p)
})

pgu.regressor$set("public", "plotResult", function(){
  p1 <- private$plotRegression()
  p2 <- private$plotResidualBox()
  p3 <- private$plotResidualDist()
  p <- gridExtra::grid.arrange(p1,p2,p3, layout_matrix = rbind(c(1,1,1,2),c(1,1,1,3)))
  return(p)
})