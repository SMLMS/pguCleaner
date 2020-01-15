library('R6')
library('tidyverse')
library('pracma')


pgu.limitsOfQuantification <- R6::R6Class(
  'pgu.limitsOfQuantification',
  ####################
  # instance variables
  ####################
  private = list(
    .loq = "tbl_df",
    .validity = "logical",
    .substituteAlphabet = "character",
    .substituteAgent = "factor",
    .nanHandlingAlphabet = "character",
    .nanHandlingAgent = "factor"
  ),
  ##################
  # accessor methods
  ##################
  active = list(
    loq = function(){
      return(private$.loq)
    },
    setLoq = function(obj = 'tbl_df'){
      private$.loq <- obj
    },
    validity = function(){
      return(private$.validity)
    },
    substituteAlphabet = function(){
      return(private$.substituteAlphabet)
    },
    substituteAgent = function(){
      return(as.character(private$.substituteAgent))
    },
    setSubstituteAgent = function(agent = "character"){
      private$.substituteAgent <- factor(agent, levels = self$substituteAlphabet)
    },
    nanHandlingAlphabet = function(){
      return(private$.nanHandlingAlphabet)
    },
    nanHandlingAgent = function(){
      return(as.character(private$.nanHandlingAgent))
    },
    setNanHandlingAgent = function(agent = "character"){
      private$.nanHandlingAgent <- factor(agent, levels = self$nanHandlingAgent)
    }
  ),
  ###################
  # memory management
  ###################
  public = list(
    initialize = function(obj = 'tbl_df'){
      print("Created  instance of pgu.limitsOfQuantification in heap")
      private$.validity = FALSE
      private$.substituteAlphabet <- c("LOQ", "NaN")
      self$setSubstituteAgent <- self$substituteAlphabet[1]
      private$.nanHandlingAlphabet <- c("keep", "LLOQ", "ULOQ")
      self$setNanHandlingAgent <- self$nanHandlingAlphabet[1]
      if(class(obj)[1] != "tbl_df"){
        obj <- tibble::tibble(names <- "none",
                               values <- c(NA))
      }
      self$resetLoq(obj)
      
    },
    finalize = function(){
      print("Instance of pgu.limitsOfQuantification removed from heap")
    },
    ##########################
    # print instance variables
    ##########################
    print = function() {
      rString <- sprintf("\npgu.limitsOfQuantification\n")
      cat(rString)
      ustring <- sprintf("\nsubstituteAgent: %s\nnanHandlingAgent: %s\n", self$substituteAgent, self$nanHandlingAgent)
      cat(ustring)
      cat("\n\n")
      invisible(self)
    }
  )
)
####################
# public functions
####################
pgu.limitsOfQuantification$set("public", "resetLoq", function(obj = "tbl_df"){
  print("reset")
})

pgu.limitsOfQuantification$set("public", "checkValidity", function(numericFeatureNames = "character"){
  print("strcmp")
  print(numericFeatureNames[1])
  print(is.element(numericFeatureNames[1], colnames(self$loq)))
  print("lapply")
  print(lapply(numericFeatureNames, is.element, set = colnames(self$loq)))
  print("select")
  tryCatch({
    self$loq %>%
      dplyr::select(numericFeatureNames)
    private$.validity = TRUE
  },
  error = function(e) {
    private$.validity = FALSE
    print("error")
    print(e)
  },
  warning = function(w) {
    private$.validity = FALSE
    print("warning")
    print(e)
  }
  )
})

pgu.limitsOfQuantification$set("public", "applyLoqInformation", function(obj = 'tbl_df'){
  for (i in seq_along(colnames(self$loq))){
    colname <- colnames(self$loq)[i]
    if((colname %in% colnames(obj)) & (is.numeric(self$loq[[colname]]))){
      minValue <-  self$loq %>%
        dplyr::filter(LOQ == 'LLOQ') %>%
        dplyr::select(colname) %>%
        as.numeric()
      maxValue <- self$loq %>%
        dplyr::filter(LOQ == 'ULOQ') %>%
        dplyr::select(colname) %>%
        as.numeric()
      obj[colname] <- pmax(minValue, obj[[colname]], na.rm = TRUE)
      obj[colname] <- pmin(maxValue, obj[[colname]], na.rm = TRUE)
    }
  }
  return(obj)
})

main = function(){
  data <- readxl::read_xlsx(path = "/home/malkusch/PowerFolders/pharmacology/Daten/Gurke/Lisa/loq-test.xlsx",sheet = "data")
  loqData <- readxl::read_xlsx(path = "/home/malkusch/PowerFolders/pharmacology/Daten/Gurke/Lisa/loq-test.xlsx",sheet = "LOQ")
  loq <- pgu.limitsOfQuantification$new(obj = loqData)
  data <- loq$applyLoqInformation(obj = data)
  print(head(data['1-AG']))
}

main()
