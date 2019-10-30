library("tidyverse")
##################
# Global functions
##################
normalDistribution = function(x="numeric", mu="numeric",sigma="numeric"){
  y <- 1.0/(sigma*sqrt(2.0*pi)) *exp(-1.0 * ((x-mu)^2)/(2.0*sigma^2))
  return(y)
}

dLogLikelihood = function(x="numeric", mu="numeric",sigma="numeric", log=TRUE){
  y <- dnorm(x,mu,sigma)
  return(sum(log(y)))
}

sLogLikelihood = function(mu="numeric", sigma="numeric"){
  bbmle::snorm(mean=mu, sd=sigma)
}

transformTibble = function(obj = "tbl_df"){
  vNames <- colnames(obj)[-1]
  cNames <- obj[[1,]]
  obj[-1] %>%
    as.data.frame() %>%
    t() %>%
    tibble::tibble() %>%
    dplyr::rename_all(~ c(cNames)) %>%
    dplyr::mutate(parameter = vNames) %>%
    dplyr::select(parameter, dplyr::everything()) %>%
    return()
}