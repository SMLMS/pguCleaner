library("R6")
library("hdf5r")
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
                                    private$.suffixes <- c("csv", "txt", "h5", "xls", "xlsx")
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

pgu.importer$set("public", "importCsv", function(obj = "pgu.file"){
  dataTbl <- readr::read_csv(file = obj$uploadFileName,
                             skip = obj$skipRows,
                             na = obj$naChar,
                             col_names = obj$header)
  return(dataTbl)
})

pgu.importer$set("public", "importXls", function(obj = "pgu.file"){
  dataTbl <- readxl::read_xls(path = obj$uploadFileName,
                              sheet = obj$sheetIndex,
                              col_names = obj$header,
                              skip = obj$skipRows,
                              na = obj$naChar)
  return(dataTbl)
})

pgu.importer$set("public", "importXlsx", function(obj = "pgu.file"){
  dataTbl <- readxl::read_xlsx(path = obj$uploadFileName,
                               sheet = obj$sheetIndex,
                               col_names = obj$header,
                               skip = obj$skipRows,
                               na = obj$naChar)
  
  return(dataTbl)
})

pgu.importer$set("public", "importHdf5", function(obj = "pgu.file"){
  file.h5 <- hdf5r::h5file(obj$uploadFileName, mode="r")
  dataTbl <- file.h5$open("data/raw")$read() %>%
    tibble::as_tibble()
  file.h5$close_all()
  return(dataTbl)
})

pgu.importer$set("public", "import", function(obj = "pgu.file"){
  if(self$suffixIsKnown(obj)){
    switch(obj$suffix,
           csv = {return(self$importCsv(obj))},
           #txt = {return(self$importCsv(obj))},
           h5 = {return(self$importHdf5(obj))},
           xls = {return(self$importXls(obj))},
           xlsx = {return(self$importXlsx(obj))})
  }else{
    return(NULL)
  }
})