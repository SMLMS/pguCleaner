library('R6')
library('tidyverse')
library('pracma')
library('DT')
library('grid')


pgu.limitsOfQuantification <- R6::R6Class(
  'pgu.limitsOfQuantification',
  ####################
  # instance variables
  ####################
  private = list(
    .loq = "tbl_df",
    .outliers = "tbl_df",
    .lloqSubstituteAlphabet = "character",
    .lloqSubstituteAgent = "factor",
    .uloqSubstituteAlphabet = "character",
    .uloqSubstituteAgent = "factor",
    .naHandlingAlphabet = "character",
    .naHandlingAgent = "factor",
    .loqStatistics = "tbl_df"
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
    outliers = function(){
      return(private$.outliers)
    },
    lloqSubstituteAlphabet = function(){
      return(private$.lloqSubstituteAlphabet)
    },
    lloqSubstituteAgent = function(){
      return(as.character(private$.lloqSubstituteAgent))
    },
    setLloqSubstituteAgent = function(agent = "character"){
      private$.lloqSubstituteAgent <- factor(agent, levels = self$lloqSubstituteAlphabet)
    },
    uloqSubstituteAlphabet = function(){
      return(private$.uloqSubstituteAlphabet)
    },
    uloqSubstituteAgent = function(){
      return(as.character(private$.uloqSubstituteAgent))
    },
    setUloqSubstituteAgent = function(agent = "character"){
      private$.uloqSubstituteAgent <- factor(agent, levels = self$uloqSubstituteAlphabet)
    },
    naHandlingAlphabet = function(){
      return(private$.naHandlingAlphabet)
    },
    naHandlingAgent = function(){
      return(as.character(private$.naHandlingAgent))
    },
    setNaHandlingAgent = function(agent = "character"){
      private$.naHandlingAgent <- factor(agent, levels = self$naHandlingAlphabet)
    },
    loqStatistics = function(){
      return(private$.loqStatistics)
    }
  ),
  ###################
  # memory management
  ###################
  public = list(
    initialize = function(obj = 'tbl_df'){
      print("Created  instance of pgu.limitsOfQuantification in heap")
      private$.lloqSubstituteAlphabet <- c("keep","NA", "LLOQ", "0.5 LLOQ")
      self$setLloqSubstituteAgent <- self$lloqSubstituteAlphabet[1]
      private$.uloqSubstituteAlphabet <- c("keep", "NA", "ULOQ")
      self$setUloqSubstituteAgent <- self$uloqSubstituteAlphabet[1]
      private$.naHandlingAlphabet <- c("keep", "<LLOQ", ">ULOQ")
      self$setNaHandlingAgent <- self$naHandlingAlphabet[1]
      if(class(obj)[1] != "tbl_df"){
        obj <- tibble::tibble(names <- "none",
                               values <- c(NA))
      }
      self$setLoq <- obj
      self$resetLoq()
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
      ustring <- sprintf("\nlloqSubstituteAgent: %s\nuloqSubstituteAgent: %s\nnaHandlingAgent: %s\n", self$lloqSubstituteAgent, self$uloqSubstituteAgent, self$naHandlingAgent)
      cat(ustring)
      cat("\n\n")
      print(self$loq)
      invisible(self)
    }
  )
)
####################
# public functions
####################
pgu.limitsOfQuantification$set("public", "checkValidity", function(features = "charater"){
  validity = FALSE
  tryCatch({
    self$loq %>%
      dplyr::select(features)
    validity = TRUE
  },
  error = function(e) {
    print("error")
    print(e)
  },
  warning = function(w) {
    print("warning")
    print(e)
  }
  )
  return(validity)
})

pgu.limitsOfQuantification$set("public", "resetLoqStatistics", function(obj = 'tbl_df'){
  featureNames <- colnames(obj)
  if(self$checkValidity(featureNames)){
    private$.loqStatistics <- self$loq %>%
      dplyr::select(featureNames) %>%
      t() %>%
      as.data.frame() %>%
      tibble::rownames_to_column() %>%
      tibble::as_tibble() %>%
      purrr::set_names(c("features", dplyr::pull(self$loq, c(1)))) %>%
      dplyr::mutate(quantitative = as.logical(quantitative),
                    measurements = c(rep(nrow(obj), length(featureNames))),
                    missings = as.integer(dplyr::summarise_all(obj, list(~sum(is.na(.))))),
                    belowLloq = as.integer(c(rep(0, length(featureNames)))),
                    aboveUloq = as.integer(c(rep(0, length(featureNames)))),
                    loqOutliers = as.integer(c(rep(0, length(featureNames)))),
                    fractionBelowLloq = as.integer(c(rep(0, length(featureNames)))),
                    fractionAboveUloq = as.integer(c(rep(0, length(featureNames)))),
                    fractionLoqOutliers = as.integer(c(rep(0, length(featureNames))))
      ) %>%
      dplyr::select(features, dplyr::everything())
  }
})

pgu.limitsOfQuantification$set("public", "resetLoq", function(){
  private$.outliers <- tibble::tibble(measurement = numeric(0),
                                      feature = character(0),
                                      value = numeric(0),
                                      type = character(0),
                                      color = character(0)) %>%
    dplyr::mutate_if(is.numeric, round, 8)
})

##################
# helper functions
##################
pgu.limitsOfQuantification$set("public", "featureLloq", function(feature = "character"){
  limit <- NA
  if(self$checkValidity(features = feature)){
    limit <-  self$loq %>%
      dplyr::filter(LOQ == 'LLOQ') %>%
      dplyr::select(feature) %>%
      as.numeric()
  }
  return(limit)
})

pgu.limitsOfQuantification$set("public", "featureUloq", function(feature = "character"){
  limit <- NA
  if(self$checkValidity(features = feature)){
    limit <-  self$loq %>%
      dplyr::filter(LOQ == 'ULOQ') %>%
      dplyr::select(feature) %>%
      as.numeric()
  }
  return(limit)
})

pgu.limitsOfQuantification$set("public", "appendOutlier", function(feature = "character", value = "numeric", idx = "numeric", lloq = "numeric", uloq = "numeric"){
  isOutlier <- FALSE
  if(is.na(value)){
    switch(self$naHandlingAgent,
           "keep" = {isOutlier <- FALSE},
           "<LLOQ" = {
             isOutlier <- TRUE
             outlierType <- "LLOQ"
             outlierColor <- "blue"
           },
           ">ULOQ" = {
             isOutlier <- TRUE
             outlierType <- "ULOQ"
             outlierColor <- "firebrick"
             }
           )
  }
  else if (value < lloq){
    isOutlier <- TRUE
    outlierType <- "LLOQ"
    outlierColor <- "blue"
  }
  else if (value > uloq){
    isOutlier <- TRUE
    outlierType <- "ULOQ"
    outlierColor <- "firebrick"
  }
  if(isOutlier){
    private$.outliers <- tibble::add_row(self$outliers,
                                         measurement = idx,
                                         feature = feature,
                                         value = value,
                                         type = as.character(outlierType),
                                         color = as.character(outlierColor))
  }
})

pgu.limitsOfQuantification$set("public", "findOutliers", function(obj = "tbl_df"){
  self$resetLoq()
  features <- colnames(obj)
  if(self$checkValidity(features)){
    for (feature in features){
      lloq <-  self$featureLloq(feature)
      uloq <- self$featureUloq(feature)
      featureData <- obj %>%
        dplyr::pull(feature) %>%
        as.numeric()
      for (i in seq_along(featureData)){
        self$appendOutlier(feature = feature, value = featureData[i], idx = i, lloq = lloq, uloq = uloq)
      }
    }
    return(TRUE)
  }
  else{
    return(FALSE)
  }
})

pgu.limitsOfQuantification$set("public", "collectStatistics", function(obj = "tbl_df"){
  self$resetLoqStatistics(obj)
  featureNames <- colnames(obj)
  if(self$checkValidity(featureNames)){
    lowerOutlier <- as.integer(c(rep(0, length(featureNames))))
    upperOutlier <- as.integer(c(rep(0, length(featureNames))))
    for (i in seq_along(colnames(obj))){
      feature <- featureNames[i]
      lloq <-  self$featureLloq(feature)
      uloq <- self$featureUloq(feature)
      lowerOutlier[i] <- obj %>%
        dplyr::mutate(below = as.numeric(!!dplyr::sym(feature) < lloq)) %>%
        dplyr::select(below) %>%
        sum(na.rm = TRUE)
      upperOutlier[i] <- obj %>%
        dplyr::mutate(above = as.numeric(!!dplyr::sym(feature) > uloq)) %>%
        dplyr::select(above) %>%
        sum(na.rm = TRUE)
    }
    private$.loqStatistics <- switch(self$naHandlingAgent,
                                     "keep" = {
                                       private$.loqStatistics %>%
                                         dplyr::mutate(belowLloq = lowerOutlier,
                                                       aboveUloq = upperOutlier)
                                       },
                                     "<LLOQ" = {
                                       private$.loqStatistics %>%
                                         dplyr::mutate(belowLloq = lowerOutlier + missings,
                                                       aboveUloq = upperOutlier)
                                       },
                                     ">ULOQ" = {
                                       private$.loqStatistics %>%
                                         dplyr::mutate(belowLloq = lowerOutlier,
                                                       aboveUloq = upperOutlier + missings)
                                     }
    )
    
    private$.loqStatistics <- private$.loqStatistics %>%
      dplyr::mutate(loqOutliers = belowLloq + aboveUloq,
                    fractionBelowLloq = belowLloq / measurements,
                    fractionAboveUloq = aboveUloq / measurements,
                    fractionLoqOutliers = loqOutliers / measurements)
  }
})

pgu.limitsOfQuantification$set("public", "mutateLoqOutliers", function(obj = "tbl_df"){
  if(self$checkValidity(colnames(obj))){
  obj %>%
    self$mutateLloqOutliers() %>%
    self$mutateUloqOutliers() %>%
    return()
  }
  else{
    print("error")
    return(obj)
  }
})

pgu.limitsOfQuantification$set("public", "mutateLloqOutliers", function(obj = "tbl_df"){
  featureNames <- colnames(obj)
  cleanObj <- obj
  if(self$checkValidity(featureNames)){
    value = NA
    for (feature in featureNames){
      switch(
        self$lloqSubstituteAgent,
        "keep" = {value <- value},
        "NA" = {value <- NA},
        "LLOQ" = {value <- self$featureLloq(feature)},
        "0.5 LLOQ" = {value <- 0.5 * self$featureLloq(feature)}
        )
      idx <- self$outliers %>%
        dplyr::filter(feature == !!feature) %>%
        dplyr::filter(type == "LLOQ") %>%
        dplyr::pull(measurement) %>%
        as.integer()
      cleanObj <- cleanObj %>%
        dplyr::mutate(!!as.symbol(feature) := replace(x = !!as.symbol(feature), list = idx, values = value))
    }
  }
  return(cleanObj)
})

pgu.limitsOfQuantification$set("public", "mutateUloqOutliers", function(obj = "tbl_df"){
  featureNames <- colnames(obj)
  cleanObj <- obj
  if(self$checkValidity(featureNames)){
    value = NA
    for (feature in featureNames){
      switch(
        self$uloqSubstituteAgent,
        "keep" = {value <- value},
        "NA" = {value <- NA},
        "ULOQ" = {value <- self$featureUloq(feature)}
      )
      idx <- self$outliers %>%
        dplyr::filter(feature == !!feature) %>%
        dplyr::filter(type == "ULOQ") %>%
        dplyr::pull(measurement) %>%
        as.integer()
      cleanObj <- cleanObj %>%
        dplyr::mutate(!!as.symbol(feature) := replace(x = !!as.symbol(feature), list = idx, values = value))
    }
  }
  return(cleanObj)
})

pgu.limitsOfQuantification$set("public", "featureOutlier", function(feature = "character"){
  t <- NULL
  if(self$checkValidity(feature)){
    t <- featureOutlier <- self$outliers %>%
      dplyr::filter(feature == !!feature)
  }
  return(t)
})

##################
# data information
##################
pgu.limitsOfQuantification$set("public", "dataInformation", function(){
  self$loq %>%
    dplyr::summarise_all(class) %>%
    tidyr::gather(variable, class) %>%
    return()
})

##################
# output functions
##################
pgu.limitsOfQuantification$set("public", "loqDataTable", function(dfData = "tbl_df", dfMetadata = "tbl_df"){
  options(htmlwidgets.TOJSON_ARGS = list(na = 'string'))
  t <- NULL
  featureNames <- colnames(dfData)
  tryCatch(
    dfMerge <- dplyr::bind_cols(dfMetadata, dfData),
    error = function(e){
      print("error")
      print(e)
      dfMerge <- dfData
    }
  )
  if(self$checkValidity(featureNames)){
    t <- dfMerge %>%
      dplyr::mutate_if(is.numeric, round, 3) %>%
      DT::datatable(options = list(scrollX = TRUE,
                                   scrollY = '350px',
                                   paging = FALSE))
    for (featureName in featureNames){
      featureOutlier <- self$outliers %>%
        dplyr::filter(feature == featureName) %>%
        dplyr::mutate_if(is.numeric, round, 3)
      if (nrow(featureOutlier) > 0){
        t <- DT::formatStyle(t,
                             featureName,
                             backgroundColor = DT::styleEqual(dfMerge %>%
                                                                dplyr::select(!!featureName) %>%
                                                                dplyr::slice(featureOutlier[["measurement"]]) %>%
                                                                unlist() %>%
                                                                round(digits = 3),
                                                              featureOutlier[["color"]]))
      }
    }
  }
  return(t)
})

pgu.limitsOfQuantification$set("public", "loqFeatureTable", function(obj = "tbl_df", feature = "character"){
  options(htmlwidgets.TOJSON_ARGS = list(na = 'string'))
  t <- NULL
  if(self$checkValidity(feature)){
    featureOutlier <- self$outliers %>%
      dplyr::filter(feature == !!feature) %>%
      dplyr::mutate_if(is.numeric, round, 3)
    
    dfFeature <- obj %>%
      # dplyr::select(!!feature) %>%
      dplyr::mutate_if(is.numeric, round, 3)
    
    print(dfFeature)
    
    t <- dfFeature %>%
      DT::datatable(options = list(scrollX = TRUE,
                                   scrollY = '350px',
                                   paging = FALSE))
    if (nrow(featureOutlier) > 0){
      t <- DT::formatStyle(
        t,
        feature,
        backgroundColor = DT::styleEqual(dfFeature %>%
                                           dplyr::select(!!feature) %>%
                                           dplyr::slice(featureOutlier[["measurement"]]) %>%
                                           unlist() %>%
                                           round(digits = 3),
                                         featureOutlier[["color"]]))
    }
  }
  return(t)
})

pgu.limitsOfQuantification$set("public", "plotLoqDistribution", function(){
  p <- self$loqStatistics %>%
    # tidyr::gather('low', 'high', key = "type", value="typeCount") %>%
    tidyr::gather('belowLloq', 'aboveUloq', key = "type", value="typeCount") %>%
    dplyr::mutate(fraction = 100 * typeCount/measurements) %>%
    ggplot2::ggplot(mapping = ggplot2::aes_string(x = "features", y = "fraction", fill = "type"), na.rm=TRUE)+
    ggplot2::geom_col()+
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
  return(p)
})

pgu.limitsOfQuantification$set("public", "featureBarPlot", function(obj = "tbl_df", feature = "character"){
  p <- NULL
  if(self$checkValidity(feature)){
    lloq <-  self$featureLloq(feature)
    uloq <- self$featureUloq(feature)
    p <- obj %>%
      # dplyr::select(feature) %>%
      ggplot2::ggplot(mapping = ggplot2::aes_string(x = as.name(feature)), na.rm=TRUE) +
      ggplot2::geom_bar(stat = "bin") +
      ggplot2::geom_vline(xintercept=lloq, linetype="dashed") +
      ggplot2::geom_vline(xintercept=uloq, linetype="dashed")
}
  return(p)
})

pgu.limitsOfQuantification$set("public", "featureBoxPlotWithSubset", function(obj = "tbl_df", feature = "character"){
  p <- NULL
  if(self$checkValidity(feature)){
    lloq <-  self$featureLloq(feature)
    uloq <- self$featureUloq(feature)
    p <- obj %>%
      dplyr::select(feature) %>%
      dplyr::mutate(LOQ = dplyr::if_else(condition = !!dplyr::sym(feature) < lloq, true = "< LLOQ", dplyr::if_else(condition = !!dplyr::sym(feature) > uloq, true = "> ULOQ", false = "quantitative", missing = "quantitative"), missing = "quantitative")) %>%
      tidyr::gather_(key="feature", value="measurement", feature) %>%
      ggplot2::ggplot(mapping=ggplot2::aes_string(x="feature",y="measurement"), na.rm=TRUE)+
      ggplot2::geom_boxplot(na.rm=TRUE)+
      ggplot2::geom_jitter(ggplot2::aes(colour=LOQ), na.rm=TRUE) +
      ggplot2::geom_hline(yintercept=lloq, linetype="dashed") +
      ggplot2::geom_hline(yintercept=uloq, linetype="dashed")
  }
  return(p)
})

pgu.limitsOfQuantification$set("public", "featurePlot", function(obj = "tbl_df", feature = "character"){
  p1 <- self$featureBoxPlotWithSubset(obj, feature) +
    ggplot2::theme(legend.position = c(0.9, 0.9),
                   legend.key = ggplot2::element_blank(),
                   legend.background = ggplot2::element_blank())
  
  limits1 <- ggplot2::layer_scales(p1)$y$range$range
  
  p2 <- self$featureBarPlot(obj, feature)
  limits2 <- ggplot2::layer_scales(p2)$x$range$range
  
  limits <- c(min(c(limits1[1], limits2[1])),
              max(c(limits1[2], limits2[2]))
  )
  
  p1 <- p1 +
    ggplot2::scale_y_continuous(limits=limits)
  
  p2 <- p2 +
    ggplot2::scale_x_continuous(position = "top", limits=limits) +
    ggplot2::coord_flip()
  
  p <- gridExtra::grid.arrange(p1,p2, layout_matrix = rbind(c(1,2),c(1,2)))
  return(p)
})