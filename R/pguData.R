library("R6")
library("tidyverse")

pgu.data <- R6::R6Class("pgu.data",
                        ####################
                        # instance variables
                        ####################
                        private = list(
                          .rawData = "tbl_df",
                          .featureNames = "character",
                          .numericFeatureNames = "character",
                          .nonNumericFeatureNames = "character",
                          .activeFeatureNames = "character",
                          .abscissa = "character",
                          .ordinate = "character"
                        ),
                        ##################
                        # accessor methods
                        ##################
                        active = list(
                          rawData = function(){
                            return(private$.rawData)
                          },
                          setRawData = function(data = "tbl_df"){
                            private$.rawData <- data
                            private$.featureNames <- colnames(data)
                            private$.numericFeatureNames <- data %>%
                              dplyr::select_if(is.numeric) %>%
                              colnames()
                            private$.nonNumericFeatureNames <- data %>%
                              dplyr::select_if(purrr::negate(is.numeric)) %>%
                              colnames()
                            private$.abscissa <- self$featureNames[1]
                            private$.ordinate <- self$featureNames[1]
                            self$resetActiveFeatureNames()
                          },
                          featureNames = function(){
                            return(private$.featureNames)
                          },
                          numericFeatureNames = function(){
                            return(private$.numericFeatureNames)
                          },
                          nonNumericFeatureNames = function(){
                            return(private$.nonNumericFeatureNames)
                          },
                          activeFeatureNames = function(){
                            return(private$.activeFeatureNames)
                          },
                          setActiveFeatureNames = function(val = "character"){
                            idx = 0
                            for(feature in val){
                              idx <- idx + self$featureIdx(feature)
                            }
                            if(is.na(idx)){
                              self$resetActiveFeatureNames()
                            }
                            else{
                              private$.activeFeatureNames <- val 
                            }
                          },
                          abscissa = function(){
                            return(private$.abscissa)
                          },
                          setAbscissa = function(feature = "character"){
                            idx <- self$featureIdx(feature)
                            if(!is.na(idx)){
                              private$.abscissa <- feature
                            }
                            else{
                              private$.abscissa <- self$featureNames[1]
                            }
                          },
                          ordinate = function(){
                            return(private$.ordinate)
                          },
                          setOrdinate = function(feature = "character"){
                            idx <- self$featureIdx(feature)
                            if(!is.na(idx)){
                              private$.ordinate <- feature
                            }
                            else{
                              private$.ordinate<- self$featureNames[1]
                            }
                          }
                        ),
                        ###################
                        # memory management
                        ###################
                        public = list(
                          initialize = function(data = "tbl_df") {
                            if(class(data) != "tbl_df"){
                              data <- tibble::tibble(names <- "none",
                                                     values <- c(NA))
                            }
                            self$setRawData <- data
                          },
                          finalize = function() {
                            print("Instance of pgu.data removed from heap")
                          },
                          ##########################
                          # print instance variables
                          ##########################
                          print = function() {
                            rString <- sprintf("\npgu.data\n")
                            cat(rString)
                            print(head(self$rawData))
                            fString <- sprintf("\nfeatureNames:\n")
                            cat(fString)
                            print(self$featureNames)
                            nString <- sprintf("\nnumeric featureNames:\n")
                            cat(nString)
                            print(self$numericFeatureNames)
                            noString <- sprintf("\nnon numeric featureNames:\n")
                            cat(noString)
                            print(self$nonNumericFeatureNames)
                            aString <- sprintf("\nactive featureNames:\n")
                            cat(aString)
                            print(self$activeFeatureNames)
                            cat("\n\n")
                            invisible(self)
                          }
                        )
)
####################
# public functions
####################
pgu.data$set("public", "resetActiveFeatureNames", function(){
  private$.activeFeatureNames <- self$featureNames
})

##################
# helper functions
##################
pgu.data$set("public", "featureIdx", function(feature = "character"){
  idx <- match(feature, self$featureNames)
  if(is.na(idx)){
    rString <- sprintf("\nWarning in pgu.data: feature %s is not known\n",
                       feature)
    cat(rString)
  }
  return(idx)
})

pgu.data$set("public", "numericData", function(){
  self$rawData %>%
    dplyr::select_if(is.numeric) %>%
    return()
})
##################
# data information
##################
pgu.data$set("public", "dataInformation", function(){
  self$rawData %>%
    dplyr::summarise_all(class) %>%
    tidyr::gather(variable, class) %>%
    return()
})

###########################
# data statistics functions
###########################
pgu.data$set("public", "summarizeNumeric",function(val = "numeric"){
  if(!any(is.na(val))){
    res <- c(summary(val),"NA's"=0)
  } else{
    res <- summary(val)
  }
  return(res)
})

pgu.data$set("public", "dataStatistics", function(){
  self$numericData() %>%
    apply(MARGIN=2, FUN=self$summarizeNumeric) %>%
    t() %>%
    as.data.frame() %>%
    tibble::rownames_to_column("Value") %>%
    tibble::as_tibble() %>%
    return()
})

pgu.data$set("public", "reducedDataStatistics", function(){
  self$dataStatistics() %>%
    dplyr::select(c("Value", "Mean")) %>%
    return()
})

pgu.data$set("public", "missings", function(){
  len <- nrow(self$rawData)
  self$dataStatistics() %>%
    dplyr::select(c("Value", "NA's")) %>%
    dplyr::rename(absolute = !!("NA's")) %>%
    dplyr::mutate(fraction = absolute / len) %>%
    return()
})