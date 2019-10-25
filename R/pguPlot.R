library("R6")
library("tidyverse")

pgu.plot <- R6::R6Class("pgu.plot",
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
                          setRawData = function(data = "tbl_df"){
                            private$.rawData <- data
                          },
                          abscissa = function(){
                            return(private$.abscissa)
                          },
                          ordinate = function(){
                            return(private$.ordinate)
                          }
                        ),
                        ###################
                        # memory management
                        ###################
                        public = list(
                          initialize = function(data = "tbl_df") {
                            if(class(data) != "tbl_df"){
                              data <- tibble::tibble(abscissa <- c(NA),
                                                     ordinate <- c(NA))
                            }
                            self$setRawData <- data
                          },
                          finalize = function() {
                            print("Instance of pgu.plot removed from heap")
                          },
                          ##########################
                          # print instance variables
                          ##########################
                          print = function() {
                            rString <- sprintf("\npgu.plot\n")
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
pgu.plot$set("public", "resetRawData", function(data = "tbl_df", abs = "character", ord = "character"){
  self$setRawData <- tibble::tibble(abscissa = data %>%
                                      dplyr::pull(!!(abs)),
                                    ordinate = data %>%
                                      dplyr::pull(!!(ord))
  )
  private$.abscissa <- abs
  private$.ordinate <- ord
})

##################
# graphical output
##################
pgu.plot$set("public", "scatterPlot", function(data = "tbl_df", abs = "character", ord = "character"){
  self$resetRawData(data, abs, ord)
  p <- ggplot2::ggplot(
    data = self$rawData,
    mapping = ggplot2::aes_string(x = "abscissa", y = "ordinate"),
    na.rm = TRUE
  ) +
    ggplot2::geom_point() +
    ggplot2::labs(title = "Feature Scatter Plot",
                  x = self$abscissa,
                  y = self$ordinate)
  return(p)
})