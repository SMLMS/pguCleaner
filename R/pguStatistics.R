pgu.missings$set("public", "summarize", function(data = "tbl_df"){
  if(!any(is.na(data))){
    res <- c(summary(data),"NA's"=0)
  } else{
    res <- summary(data)
  }
  return(res)
})

pgu.missings$set("public", "dataStatistics", function(data = "tbl_df"){
  d <- do.call(cbind, data %>%
                 self$filterFeatures() %>%
                 purrr::map(self$summarize)) %>%
    t() %>%
    as.data.frame() %>%
    tibble::rownames_to_column("Value") %>%
    tibble::as_tibble()
  return(d)
})