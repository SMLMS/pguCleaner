library("tidyverse")
library("psych")
library("mice")
library("RWeka")

ImputeData <- function(Data, method = "rf") {
  data_df <- as.data.frame(Data)
  if(ncol(data_df) == 1) stop("data_df must include at least two columns!")
  MatrixImputed <- matrix(NA, nrow = nrow(data_df), ncol = ncol(data_df))
  for (i in 1:ncol(data_df)) {
    stats0 <- psych::describe(data_df[,i])
    stats <- matrix(NA, ncol= 0, nrow = 13)
    for (j in 1:2) {
      imputed_Data <- mice::mice(as.matrix(data_df), method = method, seed = 42+j, printFlag = FALSE)
      complete_Data <- mice::complete(imputed_Data,1)
      stats <- rbind(stats,psych::describe(complete_Data[,i]))
    }
    diffMat <- abs(sweep(x = stats, MARGIN = 2, STATS = unlist(stats0), FUN = "-"))
    ranks <- apply(X = diffMat, MARGIN = 2, FUN = function(x)rank(x, ties.method = "max"))
    imputed_Data <- mice::mice(as.matrix(data_df), method = method, seed = 42+which.min(rowSums(ranks[,3:13])), printFlag = FALSE)
    complete_Data <- mice::complete(imputed_Data,1)
    MatrixImputed[,i] <- complete_Data[,i]
  }
  imputed_df <- tibble::as.tibble(MatrixImputed)
  colnames(imputed_df) <- colnames(data_df)
  return(imputed_df)    
}

ImputeTidyverseData <- function(data_df, method = "rf") {
  if(ncol(data_df) == 1) stop("data_df must include at least two columns!")
  data_col_names <- colnames(data_df)
  colnames(data_df) <- paste0("F", seq(1:ncol(data_df))) %>%
    as.character()
  print(data_df)
  imputed_df <- data_df
  n <- 2
  for (col_name in colnames(data_df)) {
    stats0 <- data_df %>%
      dplyr::select(col_name) %>%
      unlist() %>%
      psych::describe()
    stats <- matrix(NA, ncol= n, nrow = 13)
    for (j in 1:n) {
      imputed_Data <- data_df %>%
        mice::mice(method = method, seed = 42+j, printFlag = FALSE)
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
      # abs(sweep(x = stats, MARGIN = 1, STATS = unlist(stats0), FUN = "-"))
    ranks <- apply(X = diffMat, MARGIN = 1, FUN = function(x)rank(x, ties.method = "max"))
    imputed_Data <- mice::mice(data_df, method = method, seed = 42+which.min(rowSums(ranks[,3:13])), printFlag = FALSE)
    complete_Data <- mice::complete(imputed_Data,1)
    imputed_df <- imputed_df %>%
      dplyr::mutate(!!col_name := complete_Data %>%
                      dplyr::select(col_name) %>%
                      unlist())
  }
  colnames(imputed_df) <- data_col_names
  return(imputed_df)    
}

imputeDataM5 <- function(data_df){
  if(ncol(data_df) < 2){
    return(data_df)
  }
  else{
    data_col_names <- colnames(data_df)
    colnames(data_df) <- paste0("F", seq(1:ncol(data_df))) %>%
      as.character()
    impute_df <- data_df
    for (col_name in colnames(data_df)) {
      print(col_name)
      na_idx <- which(is.na(data_df[[col_name]]))
      if((length(na_idx)<1) | length(na_idx) == nrow(data_df)){
        next
      }
      train_df <- data_df %>%
        dplyr::slice(-na_idx)
      
      na_df <- data_df %>%
        dplyr::slice(na_idx)
      
      print(nrow(train_df))
      print(nrow(na_df))
      
      m5 <- col_name %>%
        paste("~.") %>%
        as.formula() %>%
        RWeka::M5P(data = train_df)
      
      na_values <- predict(m5, newdata = na_df)
      
      for (i in 1:length(na_idx)){
        impute_df[[na_idx[i], col_name]] <- na_values[i]
      }
    }
    colnames(impute_df) <- data_col_names
    return(impute_df)
  }
}

# data_df <- utils::read.csv(file = "data/DatensatzDifferences.csv")
data_df <- readxl::read_xlsx(path = "~/Documents/Code/R/pguCleaner/data/testDataFormat_200213.xlsx") %>%
  dplyr::select(c("AEA", "PEA", "1-AG", "2-AG"))

# colnames(data_df) <- c("a","b", "c", "d")
# 
# start_time <- Sys.time()
# DataImputed <- ImputeData(Data = data_df)
# stop_time <- Sys.time()
# 
# print(DataImputed)
# print(stop_time - start_time)

# start_time <- Sys.time()
# DataImputed <- ImputeTidyverseData(data_df)
# stop_time <- Sys.time()
# 
# print(DataImputed)
# print(stop_time - start_time)

impute_df <- imputeDataM5(data_df)
print(impute_df)
