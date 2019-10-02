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
# pgu.fileDesign$set("public", "splitFileName", function(){
#   self$setBaseName <- tools::file_path_sans_ext(basename(self$fileName))
#   self$setFolderName <- dirname(self$fileName)
#   self$setSuffix <- tools::file_ext(self$fileName)
# })