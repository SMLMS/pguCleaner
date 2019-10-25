library("R6")
library("tidyverse")

pgu.data <- R6::R6Class("pgu.data",
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
                          initialize = function(data = "tbl_df") {
                            
                          },
                          finalize = function() {
                            print("Instance of className removed from heap")
                          },
                          ##########################
                          # print instance variables
                          ##########################
                          print = function() {
                            rString <- sprintf("\nclassName\n")
                            cat(rString)
                            cat("\n\n")
                            invisible(self)
                          }
                        )
)
####################
# public functions
####################
pgu.data$set("public", "resetActiveFeatureNames", function(){
  private$activeFeatureNames <- self$featureNames
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