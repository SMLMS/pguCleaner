library("R6")
library("tidyverse")

pgu.filter <- R6::R6Class("pgu.filter",
                          ####################
                          # instance variables
                          ####################
                          private = list(
                            .colIdx = "numeric",
                            .rowIdx = "numeric"
                          ),
                          ##################
                          # accessor methods
                          ##################
                          active = list(
                            colIdx = function(){
                              return(private$.colIdx)
                            },
                            setColIdx = function(idx = "numeric"){
                              private$.colIdx <- idx
                            },
                            rowIdx = function(){
                              return(private$.rowIdx)
                            },
                            setRowIdx = function(idx = "numeric"){
                              private$.rowIdx = idx
                            }
                          ),
                          ###################
                          # memory management
                          ###################
                          public = list(
                            initialize = function(data = "tbl_df"){
                              if(class(data) != "tbl_df"){
                                data <- tibble::tibble(names <- "none",
                                                       values <- c(NA))
                              }
                              data %>%
                                self$resetFilter()
                            },
                            finalize = function(){
                              print("Instance of pgu.filter removed from heap")
                            },
                            ##########################
                            # print instance variables
                            ##########################
                            print = function(){
                              rString <- sprintf("\npgu.filter\n")
                              cat(rString)
                              cat("colIdx:\n")
                              cat(private$.colIdx)
                              cat("\nrawIdx:\n")
                              cat(private$.rowIdx)
                              cat("\n\n")
                              invisible(self)
                            }
                          )
)
                          
                              
#################
# reset functions
#################
pgu.filter$set("public", "resetColIdx", function(data = "tbl_df"){
  self$setColIdx <- seq(1,ncol(data), 1)
})

pgu.filter$set("public", "resetRowIdx", function(data = "tbl_df"){
  self$setRowIdx <- seq(1,nrow(data), 1)
})

pgu.filter$set("public", "resetFilter", function(data = "tbl_df"){
  self$resetColIdx(data)
  self$resetRowIdx(data)
})
##################
# filter functions
##################
pgu.filter$set("public", "filter", function(data = "tbl_df"){
  data %>%
    dplyr::select(self$colIdx) %>%
    dplyr::slice(self$rowIdx) %>%
    return()
})

pgu.filter$set("public", "filterRows", function(data = "tbl_df"){
  data %>%
    dplyr::slice(self$rowIdx) %>%
    return()
})