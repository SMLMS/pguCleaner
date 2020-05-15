library("R6")
library("tidyverse")
library("mice")
library("VIM")
library("DMwR")
library("Amelia")

pgu.imputation <- R6::R6Class("pgu.imputation",
                              ####################
                              # instance variables
                              ####################
                               private = list(
                                 .imputationParameter = "tbl_df",
                                 .imputationSites = "tbl_df",
                                 .imputationAgentAlphabet = "character",
                                 .imputationAgent = "factor",
                                 .seed = "numeric",
                                 .iterations = "numeric",
                                 .amv = "ANY"
                               ),
                              ##################
                              # accessor methods
                              ##################
                               active = list(
                                 imputationParameter = function(){
                                   return(private$.imputationParameter)
                                 },
                                 imputationSites = function(){
                                   return(private$.imputationSites)
                                 },
                                 imputationAgentAlphabet = function(){
                                   return(private$.imputationAgentAlphabet)
                                 },
                                 imputationAgent = function(){
                                   return(as.character(private$.imputationAgent))
                                 },
                                 setImputationAgent = function(agent = "character") {
                                   private$.imputationAgent <- factor(agent, levels = self$imputationAgentAlphabet)
                                 },
                                 seed = function(){
                                   return(private$.seed)
                                 },
                                 setSeed = function(value = "numeric"){
                                   private$.seed <- value
                                 },
                                 iterations = function(){
                                   return(private$.iterations)
                                 },
                                 setIterations = function(value = "numeric"){
                                   private$.iterations <- value
                                 },
                                 amv = function(){
                                   return(private$.amv)
                                 }
                                 ),
                              ###################
                              # memory management
                              ###################
                               public = list(
                                 initialize = function(data = "tbl_df"){
                                   private$.imputationAgentAlphabet <- c("none", "median", "mean", "mu", "mc", "knn", "pmm", "cart", "rf", "M5P", "amelia", "amelia_bound")
                                   self$setSeed <- 42
                                   self$setIterations <- 4
                                   self$setImputationAgent <- self$imputationAgentAlphabet[1]
                                   if(class(data)[1] != "tbl_df"){
                                     data <- tibble::tibble(names <- "none",
                                                            values <- c(NA))
                                   }
                                   self$resetImputationParameter(data)
                                 },
                                 finalize = function(){
                                   print("Instance of pgu.imputation removed from heap")
                                 },
                                 ##########################
                                 # print instance variables
                                 ##########################
                                 print = function(){
                                   rString <- sprintf("\npgu.imputation\n")
                                   cat(rString)
                                   uString <- sprintf("\nseed: %i\niterations: %i\nimputationAgent: %s\nimputationSites:\n", self$seed, self$iterations, as.character(self$imputationAgent))
                                   cat(uString)
                                   print(self$imputationParameter)
                                   print(self$imputationSites)
                                   cat("\n\n")
                                   invisible(self)
                                 }
                                 )
)
####################
# public functions
####################
pgu.imputation$set("public", "resetImputationParameter", function(data = "tbl_df"){
  numericData <- data %>%
    dplyr::select_if(is.numeric)
  features <- numericData %>%
    colnames()
  measurements <- c(rep(0.0, length(features)))
  existings <- c(rep(0.0, length(features)))
  missings <- c(rep(0.0, length(features)))
  fractionOfMissings <- c(rep(0.0, length(features)))
  private$.imputationParameter <- tibble::tibble(features, measurements, existings, missings, fractionOfMissings)
  private$.amv <- VIM::aggr(numericData, plot=FALSE)
  self$gatherImputationStatistics(data)
  self$detectImputationSites(data)
})
##################
# helper functions
##################
pgu.imputation$set("public", "featureIdx", function(feature = "character"){
  idx <- match(feature, self$imputationParameter[["features"]])
  if(is.na(idx)){
    rString <- sprintf("\nWarning in pgu.imputation: feature %s is not known\n",
                       feature)
    cat(rString)
  }
  return(idx)
})

pgu.imputation$set("public", "filterFeatures", function(data = "tbl_df"){
  data %>%
    dplyr::select(private$.imputationParameter[["features"]]) %>%
    return()
})

pgu.imputation$set("public", "imputationSiteIdxByFeature", function(featureName = "character"){
  self$imputationSites %>%
    dplyr::filter(features == featureName) %>%
    dplyr::pull(row) %>%
    as.integer() %>%
    return()
})

pgu.imputation$set("public", "nanFeatureList", function(data = "tbl_df"){
  nanFeature <- c(rep("complete", nrow(data)))
  if (nrow(self$imputationSites) > 0) {
    for(i in seq(from = 1,to = nrow(self$imputationSites), by =1)){
     if (grepl("complete", nanFeature[self$imputationSites[[i,"row"]]])){
        nanFeature[self$imputationSites[[i,"row"]]] <- self$imputationSites[[i, "features"]]
      }
      else{
        nanFeature[self$imputationSites[[i,"row"]]] <- "multiple"
     }
    }
  }
  return(nanFeature)
})

#####################
# missings statistics
#####################
pgu.imputation$set("public", "gatherMeasurements", function(value = "numeric"){
  return(length(value))
})

pgu.imputation$set("public", "gatherMissings", function(value = "numeric"){
  y <- sum(is.na(value))
  return(y)
})

pgu.imputation$set("public", "gatherExistings", function(value = "numeric"){
  y <- sum(!is.na(value))
  return(y)
})

pgu.imputation$set("public", "gatherFractionOfMissings",  function(value = "numeric"){
  y <- 100.0*sum(is.na(value))/length(value)
  return(y)
})

pgu.imputation$set("public", "gatherImputationStatistics", function(data = "tbl_df"){
  filteredData <- data %>%
    self$filterFeatures()
  private$.imputationParameter["measurements"] <- filteredData %>%
    apply(MARGIN=2, FUN=self$gatherMeasurements)
  private$.imputationParameter["existings"] <- filteredData %>%
    apply(MARGIN=2, FUN=self$gatherExistings)
  private$.imputationParameter["missings"] <- filteredData %>%
    apply(MARGIN=2, FUN=self$gatherMissings)
  private$.imputationParameter["fractionOfMissings"] <- filteredData %>%
    apply(MARGIN=2, FUN=self$gatherFractionOfMissings)
})

#########################
# detect imputation sites
#########################
pgu.imputation$set("public", "detectImputationSites", function(data = "tbl_df"){
  private$.imputationSites <- data %>%
    self$filterFeatures() %>%
    is.na() %>%
    which(arr.ind=TRUE) %>%
    tibble::as_tibble() %>%
    dplyr::mutate(features = self$imputationParameter[["features"]][col])
})

#########################
# handle imputation sites
#########################
pgu.imputation$set("public", "handleImputationSites", function(data = "tbl_df", progress = "Progress"){
  if(is.na(self$imputationAgent)){
    print("Warning: Error in pgu.imputation imputationAgent is not valid. Will be set to none.")
    self$setimputationAgent <- "none"
  }
  cleanedData <- switch((self$imputationAgent),
                       "none" = data,
                       "median" = self$imputeByMedian(data, progress),
                       "mean" = self$imputeByMean(data, progress),
                       "mu" = self$imputeByExpectationValue(data, progress),
                       "mc" = self$imputeByMC(data, progress),
                       "knn" = self$imputeByKnn(data, progress),
                       "pmm" = self$imputeByMice(data,"pmm", progress),
                       "cart" = self$imputeByMice(data, "cart", progress),
                       "rf" = self$imputeByMice(data, "rf", progress),
                       "M5P" = self$imputeByM5P(data, progress),
                       "amelia" = self$imputeByAmelia(data, progress),
                       "amelia_bound" = self$imputeByAmeliaBound(data, progress)
  )
  return(cleanedData)
})

pgu.imputation$set("public", "imputeByMedian", function(data = "tbl_df", progress = "Progress"){
  for (feature in self$imputationParameter[["features"]]){
    if(("shiny" %in% (.packages())) & (class(progress)[1] == "Progress")){
      progress$inc(1)
    }
    indices <- self$imputationSiteIdxByFeature(feature)
    data <- data %>%
      dplyr::mutate(!!feature := replace(!!as.name(feature),
                                         indices,
                                         stats::median(!!as.name(feature), na.rm = TRUE)))
  }
  return(data)
})

pgu.imputation$set("public", "imputeByMean", function(data = "tbl_df", progress = "Progress"){
  for (feature in self$imputationParameter[["features"]]){
    if(("shiny" %in% (.packages())) & (class(progress)[1] == "Progress")){
      progress$inc(1)
    }
    indices <- self$imputationSiteIdxByFeature(feature)
    data <- data %>%
      dplyr::mutate(!!feature := replace(!!as.name(feature),
                                         indices,
                                         mean(!!as.name(feature), na.rm = TRUE)))
  }
  return(data)
})

pgu.imputation$set("public", "imputeByExpectationValue", function(data = "tbl_df", progress = "Progress"){
  for (feature in self$imputationParameter[["features"]]){
    if(("shiny" %in% (.packages())) & (class(progress)[1] == "Progress")){
      progress$inc(1)
    }
    indices <- self$imputationSiteIdxByFeature(feature)
    data <- data %>%
      dplyr::mutate(!!feature := replace(!!as.name(feature),
                                         indices,
                                         0.0))
  }
  return(data)
})

pgu.imputation$set("public", "imputeByMC", function(data = "tbl_df", progress = "Progress"){
  imputed_df <- data
  for (feature in self$imputationParameter[["features"]]){
    if(("shiny" %in% (.packages())) & (class(progress)[1] == "Progress")){
      progress$inc(1)
    }
    stats0 <- data %>%
      dplyr::select(feature) %>%
      unlist() %>%
      psych::describe()
    stats <- matrix(NA, ncol= self$iterations, nrow = 13)
    for (j in 1:self$iterations) {
      set.seed(self$seed + j - 1)
      indices <- self$imputationSiteIdxByFeature(feature)
      mcVal <- stats::rnorm(n = length(indices),
                            mean = 0.0,
                            sd = 1.0)
      complete_Data <- data %>%
        dplyr::mutate(!!feature := replace(!!as.name(feature),
                                           indices,
                                           mcVal))
      stats[,j] <-complete_Data %>%
        dplyr::select(feature) %>%
        unlist() %>%
        psych::describe() %>%
        t()%>%
        unlist()
    }
    diffMat <- stats %>%
      sweep(MARGIN = 1, STATS = unlist(stats0), FUN = "-") %>%
      abs()
    ranks <- apply(X = diffMat, MARGIN = 1, FUN = function(x)rank(x, ties.method = "max"))
    set.seed(self$seed+which.min(rowSums(ranks[,3:13]))-1)
    indices <- self$imputationSiteIdxByFeature(feature)
    mcVal <- stats::rnorm(n = length(indices),
                          mean = 0.0,
                          sd = 1.0)
    complete_Data <- data %>%
      dplyr::mutate(!!feature := replace(!!as.name(feature),
                                         indices,
                                         mcVal))

    imputed_df <- imputed_df %>%
      dplyr::mutate(!!feature := complete_Data %>%
                      dplyr::select(feature) %>%
                      unlist())
  }
  return(imputed_df)
})

pgu.imputation$set("public", "imputeByKnn", function(data = "tbl_df", progress = "Progress"){
  if(("shiny" %in% (.packages())) & (class(progress)[1] == "Progress")){
    progress$inc(length(self$imputationParameter[["features"]]))
  }
  if (ncol(data) < 4){
    return(data)
  }
  else{
    data %>%
      as.data.frame() %>%
      DMwR::knnImputation(k=3,
                          scale = TRUE,
                          meth = "weighAvg",
                          distData = NULL) %>%
      tibble::as_tibble() %>%
      return()
  }
})

pgu.imputation$set("public", "imputeByMice", function(data, method = "character", progress = "Progress") {
  if(ncol(data) < 2){
    return(data)
  }
  else{
    data_col_names <- colnames(data)
    colnames(data) <- paste0("F", seq(1:ncol(data))) %>%
      as.character()
    imputed_df <- data
    for (col_name in colnames(data)) {
      if(("shiny" %in% (.packages())) & (class(progress)[1] == "Progress")){
        progress$inc(1)
      }
      stats0 <- data %>%
        dplyr::select(col_name) %>%
        unlist() %>%
        psych::describe()
      stats <- matrix(NA, ncol= self$iterations, nrow = 13)
      for (j in 1:self$iterations) {
        imputed_Data <- data %>%
          mice::mice(method = method, seed = self$seed+j-1, printFlag = FALSE)
        complete_Data <- mice::complete(imputed_Data,1)
        stats[,j] <-complete_Data %>%
          dplyr::select(col_name) %>%
          unlist() %>%
          psych::describe() %>%
          t()%>%
          unlist()
      }
      diffMat <- stats %>%
        sweep(MARGIN = 1, STATS = unlist(stats0), FUN = "-") %>%
        abs()
      ranks <- apply(X = diffMat, MARGIN = 1, FUN = function(x)rank(x, ties.method = "max"))
      imputed_Data <- data %>%
        mice::mice(method = method,
                   seed = self$seed+which.min(rowSums(ranks[,3:13]))-1,
                   printFlag = FALSE)
      complete_Data <- mice::complete(imputed_Data,1)
      imputed_df <- imputed_df %>%
        dplyr::mutate(!!col_name := complete_Data %>%
                        dplyr::select(col_name) %>%
                        unlist())
    }
    colnames(imputed_df) <- data_col_names
    return(imputed_df)
  }
})

pgu.imputation$set("public", "imputeByM5P", function(data = "tl_df", progress = "Progress"){
  if(ncol(data) < 2){
    return(data)
  }
  else{
    data_col_names <- colnames(data)
    colnames(data) <- paste0("F", seq(1:ncol(data))) %>%
      as.character()
    imputed_df <- data
    for (i in 1:length(colnames(data))) {
      if(("shiny" %in% (.packages())) & (class(progress)[1] == "Progress")){
        progress$inc(1)
      }

      na_idx <- self$imputationSiteIdxByFeature(featureName = data_col_names[i])

      if((length(na_idx)<1) | length(na_idx) == nrow(data)){
        next
      }
      train_df <- data %>%
        dplyr::slice(-na_idx)
      
      na_df <- data %>%
        dplyr::slice(na_idx)
      
      m5 <- colnames(data)[i] %>%
        paste("~.") %>%
        as.formula() %>%
        RWeka::M5P(data = train_df)
      
      na_values <- predict(m5, newdata = na_df)
      
      for (j in 1:length(na_idx)){
        imputed_df[[na_idx[j], colnames(data)[i]]] <- na_values[j]
      }
    }
    colnames(imputed_df) <- data_col_names
    return(imputed_df)
  }
})

pgu.imputation$set("public", "imputeByAmelia", function(data = "tbl_df", progress = "Progress"){
  if(("shiny" %in% (.packages())) & (class(progress)[1] == "Progress")){
    progress$inc(length(self$imputationParameter[["features"]]))
  }
  ameliaOutput <- data %>%
    Amelia::amelia()#(m = 1, parallel = "multicore", ncpus = 8)
  ameliaOutput$imputations[[1]] %>%
    tibble::as_tibble() %>%
    return()
})

pgu.imputation$set("public", "imputeByAmeliaBound", function(data = "tbl_df", progress = "Progress"){
  if(("shiny" %in% (.packages())) & (class(progress)[1] == "Progress")){
    progress$inc(length(self$imputationParameter[["features"]]))
  }
  tblBounds = data.frame(column=c(1:ncol(data)),
                         lower=c(0),
                         upper=sapply(data,stats::quantile,probs = c(0.25),names=FALSE, na.rm = TRUE)) %>%
    as.matrix()
  ameliaOutput <- data %>%
    Amelia::amelia()#(m = 1,bounds=tblBounds, parallel = "multicore", ncpus = 8)
  ameliaOutput$imputations[[1]] %>%
    tibble::as_tibble() %>%
    return()
})

########
# output
########
pgu.imputation$set("public", "imputationSiteDistribution", function(data = "tbl_df"){
  d <- data %>%
    self$filterFeatures() %>%
    as.data.frame() %>%
    mice::md.pattern(plot=FALSE)
  colnames(d)[-1] <- "Sites"
  rownames(d)[length(rownames(d))] <- "Sum"
  return(d)
})

pgu.imputation$set("public", "mergeImputationSiteData", function(dfData = "tbl_df", dfMetadata = "tbl_df"){
  dfMerge <- dfData
  if(nrow(dfData) == nrow(dfMetadata)){
    dfMerge <- dplyr::bind_cols(dfMetadata, dfData)
  }
  dfMerge %>%
    dplyr::filter_all(dplyr::any_vars(is.na(.))) %>%
    return()
})

################
# plot functions
################
pgu.imputation$set("public", "imputationSiteHeatMap", function(){
  p <- plot(self$amv,
            col=c('navyblue','red'),
            numbers=TRUE,
            sortVars=TRUE,
            labels=self$imputationParameter[["features"]],
            cex.axis=.7,
            gap=3,
            ylab=c("Histogram of imputation sites","Pattern"))
  return(p)
})

pgu.imputation$set("public", "featureBarPlot", function(data = "tbl_df", feature = "character"){
  feature <- sym(feature)
  p <- data %>%
    ggplot2::ggplot(mapping = ggplot2::aes_string(x=feature), na.rm=TRUE) +
    ggplot2::geom_bar(stat = "bin")
  return(p)
})

pgu.imputation$set("public", "featureBoxPlotWithSubset", function(data = "tbl_df", feature = "character"){
  nanFeature <- self$nanFeatureList(data)
  p <- data %>%
    dplyr::select(feature) %>%
    dplyr::mutate(nanFeature = nanFeature) %>%
    tidyr::gather_(key="feature", value="measurement", feature) %>%
    ggplot2::ggplot(mapping=ggplot2::aes_string(x="feature",y="measurement"), na.rm=TRUE)+
    ggplot2::geom_boxplot(na.rm=TRUE)+
    ggplot2::geom_jitter(ggplot2::aes(colour=nanFeature), na.rm=TRUE)
  return(p)
})

pgu.imputation$set("public", "featurePlot", function(data = "tbl_df", feature = "character"){
  p1 <- self$featureBoxPlotWithSubset(data, feature) +
    ggplot2::theme(legend.position = c(0.9, 0.9),
                   legend.key = ggplot2::element_blank(),
                   legend.background = ggplot2::element_blank())
  
  limits1 <- layer_scales(p1)$y$range$range
  
  p2 <- self$featureBarPlot(data, feature)
  
  limits2 <- ggplot2::layer_scales(p2)$x$range$range
  
  limits <- c(min(c(limits1[1], limits2[1])),
              max(c(limits1[2], limits2[2]))
  )
  
  p1 <- p1 +
    ggplot2::scale_y_continuous(limits=limits)
  
  p2 <- p2 +
    ggplot2::scale_x_continuous(position = "top", limits=limits) +
    ggplot2::coord_flip()
  
  # p <- gridExtra::grid.arrange(p1,p2, layout_matrix = rbind(c(1,2),c(1,2)))
  
  p <- gridExtra::grid.arrange(p1,p2, layout_matrix = rbind(c(1,1,2),c(1,1,2)))
  return(p)
})