library("R6")
library("tidyverse")
library('grid')

pgu.explorer <- R6::R6Class("pgu.explorer",
                        ####################
                        # instance variables
                        ####################
                        private = list(
                          .rawData = "tbl_df",
                          .abscissa = "character",
                          .ordinate = "character"
                        ),
                        ##################
                        # accessor methods
                        ##################
                        active = list(
                          rawData = function(){
                            return(private$.rawData)
                          },
                          setRawData = function(obj = "tbl_df"){
                            private$.rawData <- obj
                          },
                          abscissa = function(){
                            return(private$.abscissa)
                          },
                          setAbscissa = function(value = "character"){
                            private$.abscissa <- value
                          },
                          ordinate = function(){
                            return(private$.ordinate)
                          },
                          setOrdinate = function(value = "character"){
                            private$.ordinate <- value
                          }
                        ),
                        ###################
                        # memory management
                        ###################
                        public = list(
                          initialize = function(obj = "tbl_df") {
                            if(class(obj) != "tbl_df"){
                              obj <- tibble::tibble(!!("Sample Name") <- c("none"),
                                                     values <- c(NA)
                                                     )
                            }
                            self$setRawData <- obj
                            self$setAbscissa <- "Sample Name"
                            self$setOrdinate <- "Sample Name"
                          },
                          finalize = function() {
                            print("Instance of pgu.explorer removed from heap")
                          },
                          ##########################
                          # print instance variables
                          ##########################
                          print = function() {
                            rString <- sprintf("\npgu.explorer\n")
                            cat(rString)
                            aString <- sprintf("\nabscissa: %s\nordindate: %s\n", self$abscissa, self$ordinate)
                            cat(aString)
                            print(self$rawData)
                            cat("\n\n")
                            invisible(self)
                          }
                        )
)
####################
# public functions
####################
pgu.explorer$set("public", "reset", function(obj = "tbl_df", abs = "character", ord = "character"){
  self$setRawData <- obj
  self$setAbscissa <- abs
  self$setOrdinate <- ord
})

pgu.explorer$set("public", "abscissaIsNumeric", function(){
  self$rawData %>%
    dplyr::select(self$abscissa) %>%
    purrr::map(is.numeric) %>%
    unlist() %>%
    return()
})

pgu.explorer$set("public", "ordinateIsNumeric", function(){
  self$rawData %>%
    dplyr::select(self$ordinate) %>%
    purrr::map(is.numeric) %>%
    unlist() %>%
    return()
})

##################
# graphical output
##################
pgu.explorer$set("public", "scatterPlot", function(){
  p <- self$rawData %>%
    ggplot2::ggplot(mapping = ggplot2::aes_string(x = as.name(self$abscissa), y = as.name(self$ordinate)), na.rm = TRUE) +
    ggplot2::geom_point()
  return(p)
})

pgu.explorer$set("public", "abscissaBarPlot", function(){
  p <- NULL
  if(self$abscissaIsNumeric()){
    p <- self$rawData %>%
      ggplot2::ggplot(mapping = ggplot2::aes_string(x = as.name(self$abscissa)), na.rm=TRUE) +
      ggplot2::geom_bar(stat = "bin")
  }
  return(p)
})

pgu.explorer$set("public", "abscissaBoxPlot", function(){
  p <- NULL
  if(self$abscissaIsNumeric()){
    p <- self$rawData %>%
      dplyr::select(self$abscissa) %>%
      tidyr::gather_(key="feature", value="measurement", as.name(self$abscissa)) %>%
      ggplot2::ggplot(mapping=ggplot2::aes_string(x="feature",y="measurement"), na.rm=TRUE)+
      ggplot2::geom_boxplot(na.rm=TRUE)+
      ggplot2::geom_jitter()
  }
  return(p)
})

pgu.explorer$set("public", "abscissaPlot", function(){
  p <- NULL
  if(self$abscissaIsNumeric()){
    p1 <- self$abscissaBoxPlot()
    limits <- layer_scales(p1)$y$range$range
    p2 <- self$abscissaBarPlot() +
      ggplot2::scale_x_continuous(position = "top", limits=limits) +
      ggplot2::coord_flip()
    p <- gridExtra::grid.arrange(p1,p2, layout_matrix = rbind(c(1,2),c(1,2)))
  }
  return(p)
})

pgu.explorer$set("public", "ordinateBarPlot", function(){
  p <- NULL
  if(self$ordinateIsNumeric()){
    p <- self$rawData %>%
      ggplot2::ggplot(mapping = ggplot2::aes_string(x = as.name(self$ordinate)), na.rm=TRUE) +
      ggplot2::geom_bar(stat = "bin")
  }
  return(p)
})

pgu.explorer$set("public", "ordinateBoxPlot", function(){
  p <- NULL
  if(self$ordinateIsNumeric()){
    p <- self$rawData %>%
      dplyr::select(self$ordinate) %>%
      tidyr::gather_(key="feature", value="measurement", as.name(self$ordinate)) %>%
      ggplot2::ggplot(mapping=ggplot2::aes_string(x="feature",y="measurement"), na.rm=TRUE)+
      ggplot2::geom_boxplot(na.rm=TRUE)+
      ggplot2::geom_jitter()
  }
  return(p)
})

pgu.explorer$set("public", "ordinatePlot", function(){
  p <- NULL
  if(self$ordinateIsNumeric()){
    p1 <- self$ordinateBoxPlot()
    limits <- layer_scales(p1)$y$range$range
    p2 <- self$ordinateBarPlot() +
      ggplot2::scale_x_continuous(position = "top", limits=limits) +
      ggplot2::coord_flip()
    p <- gridExtra::grid.arrange(p1,p2, layout_matrix = rbind(c(1,2),c(1,2)))
  }
  return(p)
})

################
# numeric output
################
pgu.explorer$set("public", "summarizeNumeric", function(val = "numeric"){
  if(!any(is.na(val))){
    res <- c(summary(val),"NA's"=0)
  } else{
    res <- summary(val)
  }
  return(res)
})

pgu.explorer$set("public", "abscissaStatistic", function(){
  t <- NULL
  if(self$abscissaIsNumeric()){
    t <- self$rawData %>%
      dplyr::select(self$abscissa) %>%
      apply(MARGIN=2, FUN=self$summarizeNumeric) %>%
      as.data.frame() %>%
      tibble::rownames_to_column("Value") %>%
      tibble::as_tibble()
  }
  return(t)
})

pgu.explorer$set("public", "ordinateStatistic", function(){
  t <- NULL
  if(self$abscissaIsNumeric()){
    t <- self$rawData %>%
      dplyr::select(self$ordinate) %>%
      apply(MARGIN=2, FUN=self$summarizeNumeric) %>%
      as.data.frame() %>%
      tibble::rownames_to_column("Value") %>%
      tibble::as_tibble()
  }
  return(t)
})