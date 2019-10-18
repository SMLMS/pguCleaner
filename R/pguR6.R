library("R6")
library("tools")
#library(tidyverse)

pgu.fileDesign <- R6::R6Class("pgu.fileDesign",
                              ####################
                              # instance variables
                              ####################
                               private = list(
                                 
                               ),
                              ##################
                              # accessor methods
                              ##################
                               active = list(
                                 
                                 ),
                              ###################
                              # memory management
                              ###################
                               public = list(
                                 initialize = function(){
                                   
                                 },
                                 finalize = function(){
                                   print("Instance of xxx removed from heap")
                                 },
                                 ##########################
                                 # print instance variables
                                 ##########################
                                 print = function(){
                                   rString <- sprintf("\nclassName\n")
                                   cat(rString)
                                   invisible(self)
                                 }
                                 )
)
####################
# public functions
####################
pgu.optimizer$set("public", "resetOptParameter", function(data = "tbl_df"){
  
})
##################
# helper functions
##################
pgu.model$set("public", "featureIdx", function(feature = "character"){
  idx <- match(feature, self$modelParameter[["features"]])
  if(is.na(idx)){
    rString <- sprintf("\nWarning in pgu.optimizer: feature %s is not known\n",
                       feature)
    cat(rString)
  }
  return(idx)