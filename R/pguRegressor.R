library("R6")
library("tidyverse")
library("stats")
library("DT")
library("gridExtra")

pgu.regressor <- R6::R6Class("pgu.regressor",
                              ####################
                              # instance variables
                              ####################
                               private = list(
                                 .featureNames = "character",
                                 .intercept = "matrix",
                                 .slope = "matrix",
                                 .pValue = "matrix",
                                 .abscissa = "character",
                                 .ordinate = "character",
                                 .model = "lm" 
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
                                   private$.slope <- self$resetMatrix(value = 0)
                                   private$.pValue <- self$resetMatrix(value = 1)
                                 },
                                 intercept = function(){
                                   return(private$.intercept)
                                 },
                                 slope = function(){
                                   return(private$.slope)
                                 },
                                 pValue = function(){
                                   return(private$.pValue)
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
                                 model = function(){
                                   return(private$.model)
                                 }
                                 ),
                              ###################
                              # memory management
                              ###################
                               public = list(
                                 initialize = function(data = "tbl_df"){
                                   private$.featureNames <- data %>%
                                     dplyr::select_if(is.numeric) %>%
                                     colnames()
                                   private$.intercept <- self$resetMatrix(value = 0)
                                   private$.slope <- self$resetMatrix(value = 0)
                                   private$.pValue <- self$resetMatrix(value = 1)
                                   self$createRegressionMatrix(data)
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
               dimnames = list(self$featureNames, self$featureNames)) %>%
    self$resetDiagonal()
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
  #n = length(self$featureNames)
  for (abs in self$featureNames){
    for (ord in self$featureNames){
      # if((abs != ord) & (obj$featureIsValid(feature = abs)) & (obj$featureIsValid(feature = ord))){
      if(abs != ord){
        private$.abscissa <- abs
        private$.ordinate <- ord
        self$createModel(data)
        private$.intercept[ord, abs] <- as.numeric(c(summary(self$model)$coefficients[1,1]))
        private$.slope[ord, abs] <- as.numeric(c(summary(self$model)$coefficients[2,1]))
        private$.pValue[ord, abs] <-as.numeric(c(summary(self$model)$coefficients[2,4]))
      }
    }
  }
})