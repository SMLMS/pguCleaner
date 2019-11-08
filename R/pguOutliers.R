library("R6")
library("tidyverse")
library("outliers")
library("grDevices")
library("gplots")
library("DT")

pgu.outliers <- R6::R6Class("pgu.outliers",
                              ####################
                              # instance variables
                              ####################
                               private = list(
                                 .outliersParameter = "tbl_df",
                                 .outliers = "tbl_df",
                                 .cleaningAgentAlphabet = "character",
                                 .cleaningAgent = "factor",
                                 .seed = "numeric",
                                 .featureData = "numeric",
                                 .alpha = "numeric",
                                 .minSamples = "numeric",
                                 .outliersStatistics = "tbl_df"
                               ),
                              ##################
                              # accessor methods
                              ##################
                               active = list(
                                 outliersParameter = function(){
                                   return(private$.outliersParameter)
                                 },
                                 outliers = function(){
                                   return(private$.outliers)
                                 },
                                 cleaningAgentAlphabet = function(){
                                   return(private$.cleaningAgentAlphabet)
                                 },
                                 cleaningAgent = function(){
                                   return(as.character(private$.cleaningAgent))
                                 },
                                 setCleaningAgent = function(agent = "character"){
                                   private$.cleaningAgent <- factor(agent, levels = self$cleaningAgentAlphabet)
                                 },
                                 seed = function(){
                                   return(private$.seed)
                                 },
                                 setSeed = function(value = "numeric"){
                                   private$.seed <- value
                                 },
                                 featureData = function(){
                                   return(private$.featureData)
                                 },
                                 alpha = function(){
                                   return(private$.alpha)
                                 },
                                 setAlpha = function(value = "numeric"){
                                   private$.alpha <- value
                                 },
                                 minSamples = function(){
                                   return(private$.minSamples)
                                 },
                                 outliersStatistics = function(){
                                   return(private$.outliersStatistics)
                                 }
                                 ),
                              ###################
                              # memory management
                              ###################
                               public = list(
                                 initialize = function(data = "tbl_df"){
                                   self$setAlpha <- 0.05
                                   private$.minSamples <- 6
                                   private$.cleaningAgentAlphabet <- c("none", "median", "mean", "mu", "mc", "knn", "pmm", "cart", "rf", "amelia")
                                   self$setSeed <- 42.0
                                   self$setCleaningAgent <- self$cleaningAgentAlphabet[1]
                                   if(class(data)[1] != "tbl_df"){
                                     data <- tibble::tibble(names <- "none",
                                                            values <- c(NA))
                                   }
                                   self$resetOutliersParameter(data)
                                 },
                                 finalize = function(){
                                   print("Instance of pgu.outliers removed from heap")
                                 },
                                 ##########################
                                 # print instance variables
                                 ##########################
                                 print = function(){
                                   rString <- sprintf("\npgu.outliers\n")
                                   cat(rString)
                                   uString <- sprintf("\nseed: %.3e\ncleaningAgent: %s\noutliers:\n", self$seed, self$cleaningAgent)
                                   cat(uString)
                                   print(self$outliers)
                                   print(self$outliersParameter)
                                   print(self$outliersStatistics)
                                   cat("\n\n")
                                   invisible(self)
                                 }
                                 )
)
####################
# public functions
####################
pgu.outliers$set("public", "resetOutliersParameter", function(data = "tbl_df", progress = "Progress"){
  numericData <- data %>%
    dplyr::select_if(is.numeric)
  features <- numericData %>%
    colnames()
  measurements <- c(rep(0.0, length(features)))
  existings <- c(rep(0.0, length(features)))
  outliers <- c(rep(0.0, length(features)))
  fractionOfOutliers <- c(rep(0.0, length(features)))
  private$.outliersParameter <- tibble::tibble(features, measurements, existings, outliers, fractionOfOutliers)
  private$.outliers <- tibble::tibble(measurement = numeric(0),
                                     feature = character(0),
                                     values = numeric(0),
                                     type = character(0),
                                     color = character(0)) %>%
    dplyr::mutate_if(is.numeric, round, 8)
  self$findOutliers(data, progress)
})
##################
# helper functions
##################
pgu.outliers$set("public", "featureIdx", function(feature = "character"){
  idx <- match(feature, self$modelParameter[["features"]])
  if(is.na(idx)){
    rString <- sprintf("\nWarning in pgu.outliers: feature %s is not known\n",
                       feature)
    cat(rString)
  }
  return(idx)
})

pgu.outliers$set("public", "filterFeatures", function(data = "tbl_df"){
  data %>%
    dplyr::select(private$.outliersParameter[["features"]]) %>%
    return()
})

pgu.outliers$set("public", "outliersIdxByFeature", function(featureName = "character"){
  self$outliers %>%
    dplyr::filter(feature == featureName) %>%
    dplyr::pull(measurement) %>%
    as.integer() %>%
    return()
})

pgu.outliers$set("public", "outliersFeatureList", function(data = "tbl_df"){
  outFeature <- c(rep("complete", nrow(data)))
  if (nrow(self$outliers) > 0) {
    for(i in seq(from = 1,to = nrow(self$outliers), by =1)){
      if (grepl("complete", outFeature[self$outliers[[i,"measurement"]]])){
        outFeature[self$outliers[[i,"measurement"]]] <- self$outliers[[i, "feature"]]
      }
      else{
        outFeature[self$outliers[[i,"measurement"]]] <- "multiple"
      }
    }
  }
  return(outFeature)
})
#################
# detect outliers
#################
pgu.outliers$set("public", "runGrubbs", function(data = "tbl_df", feature = "character"){
  result <- outliers::grubbs.test(self$featureData, opposite = FALSE)
  if(result$p.value < self$alpha){
    if (grepl("lowest", result$alternative)){
      col <- "blue"
      t <- "min"
      idx <- which.min(self$featureData)
    }
    else{
      col <- "firebrick"
      t <- "max"
      idx <- which.max(self$featureData)
    }
    value <- data[[idx, feature]]
    private$.outliers <- tibble::add_row(self$outliers,
                                        measurement = as.numeric(idx),
                                        feature = as.character(feature),
                                        values = as.numeric(value),
                                        type = as.character(t),
                                        color = as.character(col))

    private$.featureData[idx] <- NA
    return (TRUE)
  }
  else{
    return (FALSE)
  }
})

pgu.outliers$set("public", "findOutliers", function(data = "tbl_df", progress = "Progress"){
  for (feature in self$outliersParameter[["features"]]){
    if(("shiny" %in% (.packages())) & (class(progress)[1] == "Progress")){
      progress$inc(1)
    }
    private$.featureData <- data[[feature]]
    foundOutlier <- TRUE
    while (foundOutlier) {
      foundOutlier <- self$runGrubbs(data, feature)
    }
  }
  self$outlierStatistics(data)
})

####################
# outlier statistics
####################
pgu.outliers$set("public", "outlierStatistics", function(data = "tbl_df"){
  absCount <- data %>%
    self$filterFeatures() %>%
    dplyr::summarise_all(~sum(!is.na(.)))
  private$.outliersStatistics <- tibble::tibble(features = colnames(absCount),
                                              absCount = absCount %>%
                                                unlist() %>%
                                                as.numeric())
  outlierCount <- c(rep(0.0, length(self$outliersParameter[["features"]])))
  minCount <- c(rep(0.0, length(self$outliersParameter[["features"]])))
  maxCount <- c(rep(0.0, length(self$outliersParameter[["features"]])))
  if(nrow(self$outliers) > 0){
    for (i in seq(from = 1,to = length(self$outliersParameter[["features"]]), by =1)) {
      outlierCount[i] <- sum(self$outliers["feature"] == self$outliersParameter[[i, "features"]])
      minCount[i] <- self$outliers %>%
        dplyr::filter(type == "min" & feature == self$outliersParameter[[i, "features"]]) %>%
        dplyr::select("feature") %>%
        dplyr::count() %>%
        unlist() %>%
        as.numeric()
      maxCount[i] <- self$outliers %>%
        dplyr::filter(type == "max" & feature == self$outliersParameter[[i, "features"]]) %>%
        dplyr::select("feature") %>%
        dplyr::count() %>%
        unlist() %>%
        as.numeric()
      
    }
  }
  private$.outliersStatistics <- self$outliersStatistics %>%
    dplyr::mutate(outlierCount = outlierCount,
                  low = minCount,
                  high = maxCount,
                  outlierFraction = 100* outlierCount/absCount) %>%
    dplyr::mutate(minFraction = 100 * low/absCount,
                  maxFraction = 100 * high/absCount)
})

#################
# handle outliers
#################
pgu.outliers$set("public", "insertImputationSites", function(data = "tbl_df"){
  for(feature in self$outliersParameter[["features"]]){
    indices <- self$outliers %>%
      dplyr::filter(!!feature == feature) %>%
      dplyr::pull(measurement) %>%
      as.integer()
    data <- data %>%
      dplyr::mutate(!!feature := replace(x = !!as.name(feature),
                                         list = indices,
                                         values = NA))
  }
  return(data)
})

pgu.outliers$set("public", "handleOutliers", function(data = "tbl_df", progress = "Progress"){
  cleanedData <- switch(self$cleaningAgent,
                       "none" = {data},
                       "median" = {data %>%
                         self$filterFeatures() %>%
                         self$insertImputationSites() %>%
                         self$cleanByMedian(progress = progress)},
                       "mean" = {data %>%
                         self$filterFeatures() %>%
                         self$insertImputationSites() %>%
                         self$cleanByMean(progress = progress)},
                       "mu" = {data %>%
                         self$filterFeatures() %>%
                         self$insertImputationSites() %>%
                         self$cleanByExpectationValue(progress = progress)},
                       "mc" = {data %>%
                         self$filterFeatures() %>%
                         self$insertImputationSites() %>%
                         self$cleanByMC(progress = progress)},
                       "knn" = {data %>%
                         self$filterFeatures() %>%
                         self$insertImputationSites() %>%
                         self$cleanByKnn(progress = progress)},
                       "pmm" = {data %>%
                         self$filterFeatures() %>%
                         self$insertImputationSites() %>%
                         self$cleanByPmm(progress = progress)},
                       "cart" = {data %>%
                         self$filterFeatures() %>%
                         self$insertImputationSites() %>%
                         self$cleanByCart(progress = progress)},
                       "rf" = {data %>%
                         self$filterFeatures() %>%
                         self$insertImputationSites() %>%
                         self$cleanByRf(progress = progress)},
                       "amelia" = {data %>%
                         self$filterFeatures() %>%
                         self$insertImputationSites() %>%
                         self$cleanByAmelia(progress = progress)}
  )
  data %>%
    dplyr::select(-dplyr::one_of(self$outliersParameter[["features"]])) %>%
    dplyr::bind_cols(cleanedData) %>%
    dplyr::select(colnames(data)) %>%
    return()
})

pgu.outliers$set("public", "cleanByMedian", function(data = "tbl_df", progress = "Progress"){
  for (feature in self$outliersParameter[["features"]]){
    if(("shiny" %in% (.packages())) & (class(progress)[1] == "Progress")){
      progress$inc(1)
    }
    indices <- self$outliersIdxByFeature(feature)
    data <- data %>%
      dplyr::mutate(!!feature := replace(!!as.name(feature),
                                         indices,
                                         stats::median(!!as.name(feature), na.rm = TRUE)))
  }
  return(data)
})

pgu.outliers$set("public", "cleanByMean", function(data = "tbl_df", progress = "Progress"){
  for (feature in self$outliersParameter[["features"]]){
    if(("shiny" %in% (.packages())) & (class(progress)[1] == "Progress")){
      progress$inc(1)
    }
    indices <- self$outliersIdxByFeature(feature)
    data <- data %>%
      dplyr::mutate(!!feature := replace(!!as.name(feature),
                                         indices,
                                         mean(!!as.name(feature), na.rm = TRUE)))
  }
  return(data)
})

pgu.outliers$set("public", "cleanByExpectationValue", function(data = "tbl_df", progress = "Progress"){
  for (feature in self$outliersParameter[["features"]]){
    if(("shiny" %in% (.packages())) & (class(progress)[1] == "Progress")){
      progress$inc(1)
    }
    indices <- self$outliersIdxByFeature(feature)
    data <- data %>%
      dplyr::mutate(!!feature := replace(!!as.name(feature),
                                         indices,
                                         0.0))
  }
  return(data)
})

pgu.outliers$set("public", "cleanByMC", function(data = "tbl_df", progress = "Progress"){
  for (feature in self$outliersParameter[["features"]]){
    if(("shiny" %in% (.packages())) & (class(progress)[1] == "Progress")){
      progress$inc(1)
    }
    indices <- self$outliersIdxByFeature(feature)
    mcVal <- stats::rnorm(n = length(indices),
                          mean = 0.0,
                          sd = 1.0)
    data <- data %>%
      dplyr::mutate(!!feature := replace(!!as.name(feature),
                                         indices,
                                         mcVal))
  }
  return(data)
})


pgu.outliers$set("public", "cleanByKnn", function(data = "tbl_df", progress = "Progress"){
  cleanedData <- data %>%
    as.data.frame() %>%
    DMwR::knnImputation(k=3,
                        scale = TRUE,
                        meth = "weighAvg",
                        distData = NULL) %>%
      tibble::as_tibble()

    for (feature in self$outliersParameter[["features"]]){
      if(("shiny" %in% (.packages())) & (class(progress)[1] == "Progress")){
        progress$inc(1)
      }
      indices <- self$outliersIdxByFeature(feature)
      data %>%
        dplyr::slice(indices) %>%
        dplyr::pull(feature)

      
      data <- data %>%
        dplyr::mutate(!!feature := replace(!!as.name(feature),
                                           indices,
                                           cleanedData %>%
                                             dplyr::slice(indices) %>%
                                             dplyr::pull(feature)))
    }
    return(data)
})


pgu.outliers$set("public", "cleanByPmm", function(data = "tbl_df", progress = "Progress"){
  cleanedData <- data %>%
    as.data.frame() %>%
    mice::mice(m=5,
               maxit = 5,
               method = "pmm",
               seed = self$seed) %>%
    mice::complete(action = 1) %>%
    tibble::as_tibble()

  for (feature in self$outliersParameter[["features"]]){
    if(("shiny" %in% (.packages())) & (class(progress)[1] == "Progress")){
      progress$inc(1)
    }
    indices <- self$outliersIdxByFeature(feature)
    data %>%
      dplyr::slice(indices) %>%
      dplyr::pull(feature)
    
    data <- data %>%
      dplyr::mutate(!!feature := replace(!!as.name(feature),
                                         indices,
                                         cleanedData %>%
                                           dplyr::slice(indices) %>%
                                           dplyr::pull(feature)))
  }
  return(data)
})

pgu.outliers$set("public", "cleanByCart", function(data = "tbl_df", progress = "Progress"){
  cleanedData <- data %>%
    as.data.frame() %>%
    mice::mice(minbucket = 3,
               method = "cart",
               seed = self$seed) %>%
    mice::complete(action = 1) %>%
    tibble::as_tibble()

  for (feature in self$outliersParameter[["features"]]){
    if(("shiny" %in% (.packages())) & (class(progress)[1] == "Progress")){
      progress$inc(1)
    }
    indices <- self$outliersIdxByFeature(feature)
    data %>%
      dplyr::slice(indices) %>%
      dplyr::pull(feature)
    
    data <- data %>%
      dplyr::mutate(!!feature := replace(!!as.name(feature),
                                         indices,
                                         cleanedData %>%
                                           dplyr::slice(indices) %>%
                                           dplyr::pull(feature)))
  }
  return(data)
})

pgu.outliers$set("public", "cleanByRf", function(data = "tbl_df", progress = "Progress"){
  cleanedData <- data %>%
    as.data.frame() %>%
    mice::mice(ntree = 3,
               method = "rf",
               seed = self$seed) %>%
    mice::complete(action = 1) %>%
    tibble::as_tibble()

  for (feature in self$outliersParameter[["features"]]){
    if(("shiny" %in% (.packages())) & (class(progress)[1] == "Progress")){
      progress$inc(1)
    }
    indices <- self$outliersIdxByFeature(feature)
    data %>%
      dplyr::slice(indices) %>%
      dplyr::pull(feature)
    
    data <- data %>%
      dplyr::mutate(!!feature := replace(!!as.name(feature),
                                         indices,
                                         cleanedData %>%
                                           dplyr::slice(indices) %>%
                                           dplyr::pull(feature)))
  }
  return(data)
})

pgu.outliers$set("public", "cleanByAmelia", function(data = "tbl_df", progress = "Progress"){
  ameliaOutput <- data %>%
    Amelia::amelia(m = 1, parallel = "multicore", ncpus = 8)
  cleanedData <- ameliaOutput$imputations[[1]] %>%
    tibble::as_tibble()

  for (feature in self$outliersParameter[["features"]]){
    if(("shiny" %in% (.packages())) & (class(progress)[1] == "Progress")){
      progress$inc(1)
    }
    indices <- self$outliersIdxByFeature(feature)
    data %>%
      dplyr::slice(indices) %>%
      dplyr::pull(feature)
    
    data <- data %>%
      dplyr::mutate(!!feature := replace(!!as.name(feature),
                                         indices,
                                         cleanedData %>%
                                           dplyr::slice(indices) %>%
                                           dplyr::pull(feature)))
  }
  return(data)
})

# output function
pgu.outliers$set("public", "DataTable", function(data = "tbl_df"){
  t <- data %>%
    dplyr::mutate_if(is.numeric, round, 3) %>%
    DT::datatable(options = list(scrollX = TRUE,
                                scrollY = '350px',
                                paging = FALSE))
  for (featureName in self$outliersParameter[["features"]]){
    featureOutlier <- self$outliers %>%
      dplyr::filter(grepl(featureName, feature)) %>%
      dplyr::mutate_if(is.numeric, round, 3)
    if (nrow(featureOutlier)>0){
      t <- DT::formatStyle(t,
                           featureName,
                           backgroundColor = styleEqual(data %>%
                                                          dplyr::select(!!featureName) %>%
                                                          dplyr::slice(featureOutlier[["measurement"]]) %>%
                                                          unlist() %>%
                                                          as.numeric() %>%
                                                          round(digits = 3),
                                                        featureOutlier[["color"]])
      )
    }
  }
  return(t)
})

pgu.outliers$set("public", "outlierTable", function(data = "tbl_df"){
  idx <- self$outliers[["measurement"]][!duplicated(self$outliers[["measurement"]])]
  t <- data %>%
    dplyr::slice(idx) %>%
    dplyr::mutate_if(is.numeric, round, 3) %>%
    DT::datatable(
      options = list(
        scrollX = TRUE,
        scrollY = '350px',
        paging = FALSE)
    )
  for (featureName in self$outliersParameter[["features"]]){
    featureOutlier <- self$outliers %>%
      dplyr::filter(grepl(featureName, feature)) %>%
      dplyr::mutate_if(is.numeric, round, 3)
    if (nrow(featureOutlier)>0){
      t <- DT::formatStyle(t,
                           featureName,
                           backgroundColor = styleEqual(data %>%
                                                          dplyr::select(!!featureName) %>%
                                                          dplyr::slice(featureOutlier[["measurement"]]) %>%
                                                          unlist() %>%
                                                          as.numeric() %>%
                                                          round(digits = 3),
                                                        featureOutlier[["color"]])
      )
    }
  }
  return(t)
})

pgu.outliers$set("public", "plotOutliersDistribution", function(){
  p <- self$outliersStatistics %>%
    tidyr::gather('low', 'high', key = "type", value="typeCount") %>%
    dplyr::mutate(fraction = 100 * typeCount/absCount) %>%
    ggplot2::ggplot(mapping = ggplot2::aes_string(x = "features", y = "fraction", fill = "type"), na.rm=TRUE)+
    ggplot2::geom_col()+
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
  return(p)
})

pgu.outliers$set("public", "featureBarPlot", function(data = "tbl_df", feature = "character"){
  p <- data %>%
    ggplot2::ggplot(mapping = ggplot2::aes_string(x=feature), na.rm=TRUE) +
    ggplot2::geom_bar(stat = "bin")
  return(p)
})

pgu.outliers$set("public", "featureBoxPlotWithSubset", function(data = "tbl_df", feature = "character"){
  outFeature <- self$outliersFeatureList(data)
  p <- data %>%
    dplyr::select(feature) %>%
    dplyr::mutate(outFeature = outFeature) %>%
    tidyr::gather_(key="feature", value="measurement", feature) %>%
    ggplot2::ggplot(mapping=ggplot2::aes_string(x="feature",y="measurement"), na.rm=TRUE)+
    ggplot2::geom_boxplot(na.rm=TRUE)+
    ggplot2::geom_jitter(ggplot2::aes(colour=outFeature), na.rm=TRUE)
  return(p)
})

pgu.outliers$set("public", "featurePlot", function(data = "tbl_df", feature = "character"){
  p1 <- self$featureBoxPlotWithSubset(data, feature) +
    ggplot2::theme(legend.position = c(0.9, 0.9),
                   legend.key = ggplot2::element_blank(),
                   legend.background = ggplot2::element_blank())
  p2 <- self$featureBarPlot(data, feature) +
    ggplot2::scale_x_discrete(position = "top") +
    ggplot2::coord_flip()
  p <- gridExtra::grid.arrange(p1,p2, layout_matrix = rbind(c(1,1,2),c(1,1,2)))
  return(p)
})