library("R6")
library("tidyverse")
library("writexl")
library("tools")

pgu.exporter <- R6::R6Class("pgu.exporter",
                            ####################
                            # instance variables
                            ####################
                            private = list(
                              .fileName = "character",
                              .suffix = "factor",
                              .suffixAlphabet  = "character",
                              .naChar = "character"
                            ),
                            ##################
                            # accessor methods
                            ##################
                            active = list(
                              fileName = function(){
                              return(private$.fileName)
                            },
                            setFileName = function(name = "character"){
                              private$.fileName <- name
                              self$extractSuffix()
                            },
                            suffix = function(){
                              return(private$.suffix)
                            },
                            naChar = function() {
                              return(private$.naChar)
                            },
                            setNaChar = function(name = "character"){
                              private$.naChar <- name
                            }
                            ),
                            ###################
                            # memory management
                            ###################
                            public = list(
                              initialize = function() {
                                private$.suffixAlphabet <- c("xlsx")
                                private$.naChar <- "NA"
                              },
                              finalize = function() {
                                print("Instance of pgu.exporter removed from heap")
                              },
                              ##########################
                              # print instance variables
                              ##########################
                              print = function() {
                                rString <- sprintf("\npgu.exporter\n")
                                cat(rString)
                                eString <- sprintf("\noutFileName: %s\nsuffix: %s\nnaChar: %s\n",
                                                   self$fileName,
                                                   self$suffix,
                                                   self$naChar)
                                cat(eString)
                                cat("\n\n")
                                invisible(self)
                              }
                            )
)

pgu.exporter$set("public", "extractSuffix", function(){
  private$.suffix <- tools::file_ext(self$fileName)
})

pgu.exporter$set("public", "writeDataToExcel", function(obj = "tbl_df"){
  writexl::write_xlsx(obj,
                      path = self$fileName,
                      col_names = TRUE,
                      format_headers = TRUE
                      )
})