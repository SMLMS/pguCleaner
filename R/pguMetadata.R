library('R6')
library('tidyverse')

pgu.metadata <- R6::R6Class(
  'pgu.metadata',
  ####################
  # instance variables
  ####################
  private = list(
    .features = "tbl_df"
  ),
  ##################
  # accessor methods
  ##################
  active = list(
    features = function(){
      return(private$.features)
    }
  ),
  ###################
  # memory management
  ###################
  public = list(
    initialize = function(lst = 'character'){
      print("Created  instance of pgu.metadata in heap")
      self$resetMetadata(lst)
    },
    finalize = function(){
      print("Instance of pgu.metadata removed from heap")
    },
    ##########################
    # print instance variables
    ##########################
    print = function() {
      rString <- sprintf("\npgu.metadata\n")
      cat(rString)
      print(head(self$features))
      cat("\n\n")
      invisible(self)
    }
  )
)

####################
# public functions
####################
pgu.metadata$set("public", "resetMetadata", function(lst = "character"){
  features <- lst
  type <- c(rep("data", length(lst)))
  private$.features <- tibble::tibble(features, type)
})

pgu.metadata$set("public", "defineData", function(lst = "character"){
  idx <- sapply(dplyr::select(self$features, features), function(x) x %in% lst)
  private$.features <- private$.features %>%
    dplyr::mutate(type = ifelse(as.vector(idx), "data", "metadata"))
})

pgu.metadata$set("public", "defineMetadata", function(lst = "character"){
  idx <- sapply(dplyr::select(self$features, features), function(x) x %in% lst)
  private$.features <-private$.features %>%
    dplyr::mutate(type = ifelse(as.vector(idx), "metadata", "data"))
})

pgu.metadata$set("public", "dataFeatures", function(){
  private$.features %>%
    dplyr::filter(!grepl("meta", type)) %>%
    dplyr::pull(features) %>%
    return()
})

pgu.metadata$set("public", "metadataFeatures", function(){
  private$.features %>%
    dplyr::filter(grepl("meta", type)) %>%
    dplyr::pull(features) %>%
    return()
})

main = function(){
  features <- c("Name", "sex", "age", "lip-1", "lip-2")
  dataLst <- c("lip-1", "lip-2", "lip-3", "lip-12")
  metadataLst <- c("Name", "sex", "age","stuff")
  metadata <- pgu.metadata$new(lst = features)
  metadata$defineData(lst = dataLst)
  print("data")
  print(metadata$dataFeatures())
  print("metadata")
  print(metadata$metadataFeatures())
}

main()
