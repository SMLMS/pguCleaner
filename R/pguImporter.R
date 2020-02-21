library("R6")
#library("hdf5r")
library("tools")
library("tidyverse")
source(file = "../R/pguFile.R", local=TRUE)

pgu.importer <- R6::R6Class("pgu.importer",
                                ####################
                                # instance variables
                                ####################
                                private = list(
                                  .suffixes = "character"
                                ),
                                ##################
                                # accessor methods
                                ##################
                                active = list(
                                  suffixes = function(){
                                    return(private$.suffixes)
                                  } 
                                ),
                                ###################
                                # memory management
                                ###################
                                public = list(
                                  initialize = function(){
                                    private$.suffixes <- c("xls", "xlsx")
                                  },
                                  finalize = function(){
                                    print("Instance of pgu.importer removed from heap")
                                  },
                                  ##########################
                                  # print instance variables
                                  ##########################
                                  print = function(){
                                    rString <- sprintf("\npgu.importer\nsuffixes:\n")
                                    for (suffix in self$suffixes){
                                      rString <- sprintf("%s%s\n", rString, suffix)
                                    }
                                    cat(rString)
                                    invisible(self)
                                  }
                                )
)
####################
# public functions
####################
pgu.importer$set("public", "suffixIsKnown", function(obj = "pgu.file"){
  if(sum(obj$suffix == self$suffixes)>0){
    return(TRUE)
  }
  else{
    return(FALSE)
  }
})

# pgu.importer$set("public", "importXls", function(obj = "pgu.file"){
#   dataTbl <- readxl::read_xls(path = obj$uploadFileName,
#                               sheet = obj$dataSheet,
#                               col_names = obj$header,
#                               skip = obj$skipRows,
#                               na = obj$naChar)
#   return(dataTbl)
# })
# 
# pgu.importer$set("public", "importXlsx", function(obj = "pgu.file"){
#   dataTbl <- readxl::read_xlsx(path = obj$uploadFileName,
#                                sheet = obj$dataSheet,
#                                col_names = obj$header,
#                                skip = obj$skipRows,
#                                na = obj$naChar)
#   
#   return(dataTbl)
# })
# 
# pgu.importer$set("public", "import", function(obj = "pgu.file"){
#   if(self$suffixIsKnown(obj)){
#     switch(obj$suffix,
#            xls = {return(self$importXls(obj))},
#            xlsx = {return(self$importXlsx(obj))})
#   }else{
#     return(NULL)
#   }
# })

pgu.importer$set("public", "importData", function(obj = "pgu.file"){
  if(self$suffixIsKnown(obj)){
    switch(obj$suffix,
           xls = {return(readxl::read_xlsx(path = obj$uploadFileName,
                                           sheet = obj$dataSheet,
                                           col_names = obj$header,
                                           skip = obj$skipRows,
                                           na = obj$naChar))},
           xlsx = {return(readxl::read_xlsx(path = obj$uploadFileName,
                                            sheet = obj$dataSheet,
                                            col_names = obj$header,
                                            skip = obj$skipRows,
                                            na = obj$naChar))})
  }else{
    return(NULL)
  }
})

pgu.importer$set("public", "importLoq", function(obj = "pgu.file"){
  if(self$suffixIsKnown(obj)){
    switch(obj$suffix,
           xls = {return(readxl::read_xlsx(path = obj$uploadFileName,
                                           sheet = obj$loqSheet,
                                           col_names = obj$header,
                                           skip = obj$skipRows,
                                           na = obj$naChar))},
           xlsx = {return(readxl::read_xlsx(path = obj$uploadFileName,
                                            sheet = obj$loqSheet,
                                            col_names = obj$header,
                                            skip = obj$skipRows,
                                            na = obj$naChar))})
  }else{
    return(NULL)
  }
})

pgu.importer$set("public", "importMetadata", function(obj = "pgu.file"){
  if(self$suffixIsKnown(obj)){
    switch(obj$suffix,
           xls = {return(readxl::read_xlsx(path = obj$uploadFileName,
                                           sheet = obj$metadataSheet,
                                           col_names = obj$header,
                                           skip = obj$skipRows,
                                           na = obj$naChar))},
           xlsx = {return(readxl::read_xlsx(path = obj$uploadFileName,
                                            sheet = obj$metadataSheet,
                                            col_names = obj$header,
                                            skip = obj$skipRows,
                                            na = obj$naChar))})
  }else{
    return(NULL)
  }
})