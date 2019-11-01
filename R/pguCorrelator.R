library("R6")
library("tidyverse")
library("stats")
library("DT")

pgu.correlator <- R6::R6Class("pgu.correlator",
                              ####################
                              # instance variables
                              ####################
                               private = list(
                                 .featureNames = "character",
                                 .method = "character",
                                 .coefficient = "matrix",
                                 .pValue = "matrix",
                                 .test = "htest"
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
                                   private$.coefficient <- self$resetMatrix(value = 0)
                                   private$.pValue <- self$resetMatrix(value = 1)
                                 },
                                 method = function(){
                                   return(private$.method)
                                 },
                                 coefficient = function(){
                                   return(private$.coefficient)
                                 },
                                 pValue = function(){
                                   return(private$.pValue)
                                 },
                                 test = function(){
                                   return(private$.test)
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
                                   self$resetCorrelator(data)
                                 },
                                 finalize = function(){
                                   print("Instance of pgu.correlator removed from heap")
                                 },
                                 ##########################
                                 # print instance variables
                                 ##########################
                                 print = function(){
                                   rString <- sprintf("\npgu.correlator\n")
                                   cat(rString)
                                   print(self$featureNames)
                                   mString <- sprintf("\nmethod: %s\n", self$method)
                                   cat(mString)
                                   coefString <- sprintf("\ncorrelation coefficient matrix\n")
                                   cat(coefString)
                                   print(self$coefficient)
                                   pString <- sprintf("\np-Value matrix\n")
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
pgu.correlator$set("public", "resetCorrelator", function(data = "tbl_df"){
  private$.featureNames <- data %>%
    dplyr::select_if(is.numeric) %>%
    colnames()
  private$.method <- "spearman"
  private$.coefficient <- self$resetMatrix(value = 0)
  private$.pValue <- self$resetMatrix(value = 0)
  self$createCorrelationMatrix(data)
})

pgu.correlator$set("public", "resetMatrix", function(value = "numeric"){
  n = length(self$featureNames)
  df <- matrix(data = value,
               nrow = n,
               ncol = n,
               dimnames = list(self$featureNames, self$featureNames))
  return(df)
})
##################
# helper functions
##################
pgu.correlator$set("public", "featureIdx", function(feature = "character"){
  idx <- match(feature, self$featureNames)
  if(is.na(idx)){
    rString <- sprintf("\nWarning in pgu.optimizer: feature %s is not known\n",
                       feature)
    cat(rString)
  }
  return(idx)
})

#######################
# correlation functions
#######################
pgu.correlator$set("public", "calcCorrelationNumeric", function(abscissa = "numeric", ordinate = "numeric"){
  private$.test <- stats::cor.test(x = abscissa,
                                   y = ordinate,
                                   alternative = "two.sided",
                                   exact = FALSE,
                                   method = self$method)
})

pgu.correlator$set("public", "createCorrelationMatrix", function(data = "tbl_df"){
  n = length(self$featureNames)
  for (abs in self$featureNames){
    for (ord in self$featureNames){
      self$calcCorrelationNumeric(abscissa = data %>%
                                    dplyr::pull(abs),
                                  ordinate = data %>%
                                    dplyr::pull(ord))
      private$.coefficient[ord,abs] <-self$test$estimate[[1]]
      private$.pValue[ord, abs] <- self$test$p.value
    }
  }
})

#################
# print functions
#################
pgu.correlator$set("public", "printModel", function(abscissa = "character", ordinate = "character"){
  df <- data.frame(
    abscissa = c(abscissa),
    ordinate = c(ordinate),
    rho = self$coefficient[ordinate, abscissa],
    p.correlation = self$pValue[ordinate, abscissa]) %>%
    t() %>%
    as.data.frame() %>%
    tibble::rownames_to_column("correlation parameter") %>%
    tibble::as_tibble() %>%
    dplyr::rename(value = "V1")
  return(df)
})

pgu.correlator$set("public", "printCoefficientTbl", function(){
  self$coefficient %>%
    tibble::as_tibble() %>%
    dplyr::mutate(features = self$featureNames) %>%
    dplyr::select(features, dplyr::everything()) %>%
    return()
})

pgu.correlator$set("public", "printPValueTbl", function(){
  self$pValue %>%
    tibble::as_tibble() %>%
    dplyr::mutate(features = self$featureNames) %>%
    dplyr::select(features, dplyr::everything()) %>%
    return()
})