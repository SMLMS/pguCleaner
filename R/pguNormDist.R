library("R6")
library("bbmle")
library("tidyverse")
library("stats")
library("nortest")
source(file = "R/pguGlobals.R", local=TRUE)

pgu.normDist <- R6::R6Class("pgu.normDist",
                              ####################
                              # instance variables
                              ####################
                               private = list(
                                 .featureName = "character",
                                 .rawData = "tbl_df",
                                 .histogram = "tbl_df",
                                 .expMu = "numeric",
                                 .expSigma = "numeric",
                                 .dataPoints = "numeric",
                                 .logLikelihood = "numeric",
                                 .degOfFreedom = "numeric",
                                 .bic = "numeric",
                                 .aic = "numeric",
                                 .aicc = "numeric",
                                 .rmse = "numeric",
                                 # Test results
                                 .fitSuccess = "logical",
                                 .testNames = "character",
                                 .testParameterNames = "character",
                                 .alpha = "numeric",
                                 # Shapiro Wilk test
                                 .w.shapiro = "numeric",
                                 .p.shapiro = "numeric",
                                 # Kolmogorow Smirnow test
                                 .d.kolmogorow = "numeric",
                                 .p.kolmogorow = "numeric",
                                 # Anderson Darling test
                                 .a.anderson = "numeric",
                                 .p.anderson = "numeric"
                               ),
                              ##################
                              # accessor methods
                              ##################
                               active = list(
                                 featureName = function(){
                                   return(private$.featureName)
                                 },
                                 rawData = function(){
                                   return(private$.rawData)
                                 },
                                 setRawData = function(data = "tbl_df"){
                                   self$resetNormDist(data)
                                 },
                                 histogram = function(){
                                   return(private$.histogram)
                                 },
                                 expMu = function(){
                                   return(private$.expMu)
                                 },
                                 expSigma = function(){
                                   return(private$.expSigma)
                                 },
                                 dataPoints = function(){
                                   return(private$.dataPoints)
                                 },
                                 logLikelihood = function(){
                                   return(private$.logLikelihood)
                                 },
                                 degOfFreedom = function(){
                                   return(private$.degOfFreedom)
                                 },
                                 bic = function(){
                                   return(private$.bic)
                                 },
                                 aic = function(){
                                   return(private$.aic)
                                 },
                                 aicc = function(){
                                   return(private$.aicc)
                                 },
                                 rmse = function(){
                                   return(private$.rmse)
                                 },
                                 fitSuccess = function(){
                                   return(private$.fitSuccess)
                                 },
                                 testNames = function(){
                                   return(private$.testNames)
                                 },
                                 testParameterNames = function(){
                                   return(private$.testParameterNames)
                                 },
                                 alpha = function(){
                                   return(private$.alpha)
                                 },
                                 w.shapiro = function(){
                                   return(private$.w.shapiro)
                                 },
                                 p.shapiro = function(){
                                   return(private$.p.shapiro)
                                 },
                                 d.kolmogorow = function(){
                                   return(private$.d.kolmogorow)
                                 },
                                 p.kolmogorow = function(){
                                   return(private$.p.kolmogorow)
                                 },
                                 a.anderson = function(){
                                   return(private$.a.anderson)
                                 },
                                 p.anderson = function(){
                                   return(private$.p.anderson)
                                 }
                                 ),
                              ###################
                              # memory management
                              ###################
                               public = list(
                                 initialize = function(data = "tbl_df"){
                                   self$resetNormDist(data)
                                 },
                                 finalize = function(){
                                   print("Instance of pgu.normDist removed from heap")
                                 },
                                 ##########################
                                 # print instance variables
                                 ##########################
                                 print = function(){
                                   rString <- sprintf("\npgu.normDist:\n%s\n", self$featureName)
                                   cat(rString)
                                   cat(self$fitResult())
                                   cat(self$testResultCompendium())
                                   cat("\n\n")
                                   invisible(self)
                                 }
                                 )
)
####################
# public functions
####################
pgu.normDist$set("public", "resetNormDist", function(data = "tbl_df"){
  if ((ncol(data) ==1) & (is.numeric(data[[1]]))){
    private$.featureName <- colnames(data)
  }
  else{
    rString <- sprintf("\nWarning in pgu.normDist: rawData is not numeric\n")
    cat(rString)
  }
  colnames(data) <- c("x")
  private$.rawData  <- stats::na.omit(data)
  private$.testNames <- c("Shapiro-Wilk", "Kolmogorow-Smirnow", "Anderson-Darling")
  private$.testParameterNames <- c("W", "D", "A")
  names(private$.testParameterNames) <- self$testNames
  private$.alpha <- 0.05
  private$.dataPoints <- length(self$rawData$x)
  private$.expMu <- 0
  private$.expSigma <- 0
  private$.logLikelihood <- 0
  private$.degOfFreedom <- 0
  private$.bic <- 0
  private$.aic <-0
  private$.aicc <- 0
  private$.rmse <- 0
  private$.fitSuccess <- FALSE
  private$.w.shapiro <- 0
  private$.p.shapiro <- 0
  private$.d.kolmogorow <- 0
  private$.p.kolmogorow <- 0
  private$.a.anderson <- 0
  private$.p.anderson <- 0
})

pgu.normDist$set("public", "resetFail", function(){
  private$.expMu <- NA
  private$.expSigma <- NA
  private$.logLikelihood <- NA
  private$.degOfFreedom <- NA
  private$.bic <- NA
  private$.aic <-NA
  private$.aicc <- NA
  private$.rmse <- NA
  private$.fitSuccess <- FALSE
  private$.w.shapiro <- NA
  private$.p.shapiro <- NA
  private$.d.kolmogorow <- NA
  private$.p.kolmogorow <- NA
  private$.a.anderson <- NA
  private$.p.anderson <- NA
})

###################
# fitting functions
###################
pgu.normDist$set("public", "optimize", function(){
  estMu <- mean(self$rawData[["x"]], na.rm=TRUE)
  estSigma <- sd(self$rawData[["x"]], na.rm=TRUE)
  fit<-tryCatch({
    bbmle::mle2(x ~ dLogLikelihood(mu=mu, sigma=sigma), start = list(mu=estMu, sigma=estSigma), data=self$rawData)
  },
  error = function(cond) {
    self$resetFail()
    warning("Warning in pgu.normDist during maximum likelihood optimization: %s.", cond)
    return(NA)
  })
  if(isS4(fit)){
    private$.rawData["residuals"] <- residuals(fit)
    private$.expMu <- fit@coef[1]
    private$.expSigma <- fit@coef[2]
    ll <- bbmle::logLik(fit)
    private$.logLikelihood <- ll[1]
    private$.degOfFreedom <- attr(ll, "df")
    private$.bic <- BIC(fit)
    private$.aic <- bbmle::AIC(fit)
    private$.aicc <- bbmle::AICc(fit)
    private$.fitSuccess <- TRUE
  }
})

pgu.normDist$set("public", "createHistogram", function(){
  rawHist <- ggplot_build(ggplot2::ggplot(data = self$rawData, mapping = ggplot2::aes_string(x="x"))+
                            ggplot2::geom_histogram(aes(y=..density..))
  )
  d <- tibble::tibble(x = rawHist$data[[1]]$x,
                      y_data =rawHist$data[[1]]$y)
  d["y_fit"] <- normalDistribution(x=d["x"], mu=self$expMu, sigma=self$expSigma)
  d <- dplyr::mutate(d, res =y_data - y_fit)
  private$.rmse <- sqrt(mean(d[["res"]]^2))
  private$.histogram <- d
})

pgu.normDist$set("public", "normalQQData", function(){
  p <- ggplot2::ggplot(data=self$rawData, mapping=ggplot2::aes_string(sample="x"))+
    ggplot2::stat_qq()+
    ggplot2::stat_qq_line(color="blue")
  d <- ggplot2::ggplot_build(p)
  private$.rawData["sample_quantile"] <- d$data[[1]]$sample
  private$.rawData["theoretical_quantile"] <- d$data[[1]]$theoretical
})

##############################
# test functions for normality
##############################
pgu.normDist$set("public", "test.shapiro", function(){
  test <- stats::shapiro.test(self$rawData$x)
  private$.w.shapiro <- test$statistic
  private$.p.shapiro <- test$p.value
})

pgu.normDist$set("public", "test.kolmogorow", function(){
  test <- stats::ks.test(self$rawData$x, "pnorm", mean=self$expMu, sd=self$expSigma)
  private$.d.kolmogorow <- test$statistic
  private$.p.kolmogorow <- test$p.value
})

pgu.normDist$set("public", "test.anderson", function(){
  test <- nortest::ad.test(self$rawData$x)
  private$.a.anderson <- test$statistic
  private$.p.anderson <- test$p.value
})


################
# export results
################
pgu.normDist$set("public", "fitResult", function(){
  s <- sprintf("MLE Fit Result:\nmu = %.5f\nsigma = %.5f\nlogLikelihood = %.5f\nnumber of data points = %i\ndegrees of freedom = %i\nBIC = %.5f\nAIC = %.5f\nAICc = %.5f\nRMSE = %.5f",
               self$expMu, self$expSigma, self$logLikelihood, self$dataPoints, self$degOfFreedom, self$bic, self$aic, self$aicc, self$rmse)
  return(s)
})

pgu.normDist$set("public", "testResult", function(testName = "character"){
  switch (testName,
          "Shapiro-Wilk"={
            name <- self$testNames[1]
            parameter <- self$testParameterNames[1]
            s <- self$w.shapiro
            p <- self$p.shapiro
          },
          "Kolmogorow-Smirnow"={
            name <- self$testNames[2]
            parameter <- self$testParameterNames[2]
            s <- self$d.kolmogorow
            p <- self$p.kolmogorow
          },
          "Anderson-Darling"={
            name <- self$testNames[3]
            parameter <- self$testParameterNames[3]
            s <- self$a.anderson
            p <- self$p.anderson
          }
  )
  s <- sprintf("%s test for normality:\n%s = %.5f\np = %.5f", name, parameter, s, p)
  return(s)
})

pgu.normDist$set("public", "testResultCompendium", function(){
  s <- "\nStatistical test results:"
  for (name in self$testNames) {
    s <- paste(s, self$testResult(name), sep="\n")
  }
  return(s)
})

pgu.normDist$set("public", "plotHistogram", function(){
  p <- ggplot2::ggplot(data=self$histogram, mapping = ggplot2::aes_string(x="x", y="y_data"))+
    ggplot2::geom_col()+
    ggplot2::geom_line(ggplot2::aes_string(x="x", y="y_fit"), color="blue")
  return(p)
})

pgu.normDist$set("public", "plotResiduals", function(){
  p <- ggplot2::ggplot(data=self$histogram, mapping=ggplot2::aes_string(x="x", y="res"))+
    ggplot2::geom_point()+
    ggplot2::geom_smooth()
  return(p)
})

pgu.normDist$set("public", "plotResidualDist", function(){
  p <- ggplot2::ggplot(data=self$histogram, mapping=ggplot2::aes_string(x="res"))+
    ggplot2::geom_histogram()
  return(p)
})

pgu.normDist$set("public", "plotRawResidualDist", function(){
  p <- ggplot2::ggplot(data=self$rawData, mapping=ggplot2::aes_string(x="residuals"))+
    ggplot2::geom_histogram()
  return(p)
})

pgu.normDist$set("public", "plotRawDataDist", function(){
  p <- ggplot2::ggplot(data=self$rawData, mapping=ggplot2::aes_string(x="x"))+
    ggplot2::geom_histogram()
  return(p)
})

pgu.normDist$set("public", "plotNnormalQQ", function(){
  p <- ggplot2::ggplot(data=self$rawData, mapping=ggplot2::aes_string(sample="x"))+
    ggplot2::stat_qq()+
    ggplot2::stat_qq_line(color="blue")
  return(p)
})

####################
# compound functions
####################
pgu.normDist$set("public", "fit", function(){
  self$optimize()
  if(self$fitSuccess){
    self$createHistogram()
    self$normalQQData()
    self$test.shapiro()
    self$test.kolmogorow()
    self$test.anderson()
  }
})