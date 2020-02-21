library("R6")

pgu.status <- R6::R6Class("pgu.status",
                        ####################
                        # instance variables
                        ####################
                        private = list(
                          .processAlphabet = "character",
                          .processStatus = "logical"
                        ),
                        ##################
                        # accessor methods
                        ##################
                        active = list(
                          processAlphabet = function(){
                            return(private$.processAlphabet)
                          },
                          processStatus = function(){
                            return(private$.processStatus)
                          }
                        ),
                        ###################
                        # memory management
                        ###################
                        public = list(
                          initialize = function(data = "tbl_df") {
                            print("Instance of pgu.status allocated")
                            self$reset()
                          },
                          finalize = function() {
                            print("Instance of pgu.status removed from heap")
                          },
                          ##########################
                          # print instance variables
                          ##########################
                          print = function() {
                            message <- sprintf("\npgu.status\n\n")
                            for (i in (1:length(self$processAlphabet))){
                              message <- sprintf("%s%s:\t%s\n", message, self$processAlphabet[i], self$processStatus[i])
                            }
                            cat(message)
                            
                            invisible(self)
                          }
                        )
)
####################
# public functions
####################
pgu.status$set("public", "reset", function(){
  private$.processAlphabet <- c("dataUploaded", "dataImported", "loqImported", "metadataImported", "dataFiltered",
                                "loqDetected", "loqMutated", "modelOptimized", "modelDefined", "naDetected",
                                "naMutated", "outlierDetected", "outlierMutated", "correlated", "regression")
  private$.processStatus <- c(rep(FALSE, length(self$processAlphabet)))
})


pgu.status$set("public", "update", function(processName = "character", value = "logical"){
  processFactor <- factor(processName, levels = self$processAlphabet)
  idx <- as.integer(processFactor)
  private$.processStatus[1:idx] <- TRUE
  private$.processStatus[idx:length(self$processAlphabet)] <- FALSE
  private$.processStatus[idx] <- value
  
})

pgu.status$set("public", "query", function(processName = "character"){
  processFactor <- factor(processName, levels = self$processAlphabet)
  idx <- as.integer(processFactor)
  return(self$processStatus[idx])
})

main <- function(){
  status <- pgu.status$new()
  status$update(processName = "correlated", value = TRUE)
  print(status)
  status$query(processName = "correlated") %>%
    print()
}

main()
