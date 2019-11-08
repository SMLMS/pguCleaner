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
                                 .r = "matrix",
                                 .pPearson = "matrix",
                                 .tau = "matrix",
                                 .pKendall = "matrix",
                                 .rho = "matrix",
                                 .pSpearman = "matrix",
                                 .abscissa = "character",
                                 .ordinate = "character",
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
                                   private$.r <- self$resetMatrix(value = 0)
                                   private$.pPearson <- self$resetMatrix(value = 1)
                                   private$.tau <- self$resetMatrix(value = 0)
                                   private$.pKendall <- self$resetMatrix(value = 1)
                                   private$.rho <- self$resetMatrix(value = 0)
                                   private$.pSpearman <- self$resetMatrix(value = 1)
                                 },
                                 method = function(){
                                   return(private$.method)
                                 },
                                 r = function(){
                                   return(private$.r)
                                 },
                                 pPearson = function(){
                                   return(private$.pPearson)
                                 },
                                 tau = function(){
                                   return(private$.tau)
                                 },
                                 pKendall = function(){
                                   return(private$.pKendall)
                                 },
                                 rho = function(){
                                   return(private$.rho)
                                 },
                                 pSpearman = function(){
                                   return(private$.pSpearman)
                                 },
                                 abscissa = function(){
                                   return(private$.abscissa)
                                 },
                                 setAbscissa = function(value = "character"){
                                   private$.abscissa <- value 
                                 },
                                 ordinate = function(){
                                   return(private$.ordinate)
                                 },
                                 setOrdinate = function(value = "character"){
                                   private$.ordinate <- value
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
pgu.correlator$set("public", "resetCorrelator", function(data = "tbl_df", progress = "Progress"){
  private$.featureNames <- data %>%
    dplyr::select_if(is.numeric) %>%
    colnames()
  private$.method <- c("pearson", "kendall","spearman")
  private$.r <- self$resetMatrix(value = 0)
  private$.pPearson <- self$resetMatrix(value = 1)
  private$.tau <- self$resetMatrix(value = 0)
  private$.pKendall <- self$resetMatrix(value = 1)
  private$.rho <- self$resetMatrix(value = 0)
  private$.pSpearman <- self$resetMatrix(value = 1)
  self$correlate(data, progress)
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
pgu.correlator$set("public", "calcCorrelationNumeric", function(abscissa = "numeric", ordinate = "numeric", method = "character"){
  private$.test <- stats::cor.test(x = abscissa,
                                   y = ordinate,
                                   alternative = "two.sided",
                                   exact = FALSE,
                                   method = method)
})

pgu.correlator$set("public", "createCorrelationMatrixPearson", function(data = "tbl_df", progress = "Progress"){
  for (abs in self$featureNames){
    for (ord in self$featureNames){
      if(("shiny" %in% (.packages())) & (class(progress)[1] == "Progress")){
        progress$inc(1)
      }
      self$calcCorrelationNumeric(abscissa = data %>%
                                    dplyr::pull(abs),
                                  ordinate = data %>%
                                    dplyr::pull(ord),
                                  method = self$method[1])
      private$.r[ord,abs] <-self$test$estimate[[1]]
      private$.pPearson[ord, abs] <- self$test$p.value
    }
  }
})

pgu.correlator$set("public", "createCorrelationMatrixKendall", function(data = "tbl_df", progress = "Progress"){
  for (abs in self$featureNames){
    for (ord in self$featureNames){
      if(("shiny" %in% (.packages())) & (class(progress)[1] == "Progress")){
        progress$inc(1)
      }
      self$calcCorrelationNumeric(abscissa = data %>%
                                    dplyr::pull(abs),
                                  ordinate = data %>%
                                    dplyr::pull(ord),
                                  method = self$method[2])
      private$.tau[ord,abs] <-self$test$estimate[[1]]
      private$.pKendall[ord, abs] <- self$test$p.value
    }
  }
})

pgu.correlator$set("public", "createCorrelationMatrixSpearman", function(data = "tbl_df", progress = "Progress"){
  for (abs in self$featureNames){
    for (ord in self$featureNames){
      if(("shiny" %in% (.packages())) & (class(progress)[1] == "Progress")){
        progress$inc(1)
      }
      self$calcCorrelationNumeric(abscissa = data %>%
                                    dplyr::pull(abs),
                                  ordinate = data %>%
                                    dplyr::pull(ord),
                                  method = self$method[3])
      private$.rho[ord,abs] <-self$test$estimate[[1]]
      private$.pSpearman[ord, abs] <- self$test$p.value
    }
  }
})

pgu.correlator$set("public", "correlate", function(data = "tbl_df", progress = "Progress"){
  self$createCorrelationMatrixPearson(data, progress)
  self$createCorrelationMatrixKendall(data, progress)
  self$createCorrelationMatrixSpearman(data, progress)
})

#################
# print functions
#################
pgu.correlator$set("public", "printFeature", function(){
  df <- data.frame(
    abscissa = self$abscissa,
    ordinate = self$ordinate,
    r = self$r[self$ordinate, self$abscissa],
    p.Pearson = self$pPearson[self$ordinate, self$abscissa],
    tau = self$tau[self$ordinate, self$abscissa],
    p.Kendall = self$pPearson[self$ordinate, self$abscissa],
    rho = self$rho[self$ordinate, self$abscissa],
    p.Spearman = self$pSpearman[self$ordinate, self$abscissa]) %>%
    t() %>%
    as.data.frame() %>%
    tibble::rownames_to_column("correlation parameter") %>%
    tibble::as_tibble() %>%
    dplyr::rename(value = "V1")
  return(df)
})

pgu.correlator$set("public", "printRTbl", function(){
  self$r %>%
    tibble::as_tibble() %>%
    dplyr::mutate(features = self$featureNames) %>%
    dplyr::select(features, dplyr::everything()) %>%
    return()
})

pgu.correlator$set("public", "printPPearsonTbl", function(){
  self$pPearson %>%
    tibble::as_tibble() %>%
    dplyr::mutate(features = self$featureNames) %>%
    dplyr::select(features, dplyr::everything()) %>%
    return()
})

pgu.correlator$set("public", "printTauTbl", function(){
  self$tau %>%
    tibble::as_tibble() %>%
    dplyr::mutate(features = self$featureNames) %>%
    dplyr::select(features, dplyr::everything()) %>%
    return()
})

pgu.correlator$set("public", "printPKendallTbl", function(){
  self$pKendall %>%
    tibble::as_tibble() %>%
    dplyr::mutate(features = self$featureNames) %>%
    dplyr::select(features, dplyr::everything()) %>%
    return()
})

pgu.correlator$set("public", "printRhoTbl", function(){
  self$rho %>%
    tibble::as_tibble() %>%
    dplyr::mutate(features = self$featureNames) %>%
    dplyr::select(features, dplyr::everything()) %>%
    return()
})

pgu.correlator$set("public", "printPSpearmanTbl", function(){
  self$pSpearman %>%
    tibble::as_tibble() %>%
    dplyr::mutate(features = self$featureNames) %>%
    dplyr::select(features, dplyr::everything()) %>%
    return()
})