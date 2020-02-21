library("tidyverse")
library("stats")

main <- function(){
  df_hsan <- readr::read_csv(file='./results/HSAN_variants_eigen_scores.csv', col_names = TRUE) %>%
    dplyr::select(c('chr', 'position', 'ref', 'alt', 'coding', 'pain_relevant', 'Eigen'))
  
  df_random <- readr::read_csv(file='./data/random_variants_eigen.csv', col_names = TRUE)
  df_random <- df_random %>%
    dplyr::mutate(Eigen = eigen,
                  pain_relevant = c(rep(FALSE, nrow(df_random)))) %>%
    dplyr::select(c('chr', 'position', 'ref', 'alt', 'coding', 'pain_relevant', 'Eigen'))
  
  df_control <- dplyr::bind_rows(df_hsan, df_random)
  df_values <- readr::read_csv(file='./results/variants_eigen_dataframe_200205.csv', col_names = TRUE) %>%
    dplyr::filter(pain_relevant == TRUE)
  
  ###################
  # create dataframes
  ###################
  df_controlPainIrrelevant <- df_control %>%
    dplyr::filter(pain_relevant == FALSE)
  
  df_controlPainRelevant <- df_control %>%
    dplyr::filter(pain_relevant == TRUE)
  
  df_dataPainRelevant <- df_values %>%
    dplyr::filter(pain_relevant == TRUE & ai_relevant == FALSE)
  
  df_dataAiRelevant <- df_values %>%
    dplyr::filter(ai_relevant == TRUE)
  
  ###################
  # shapiro Wilk Test
  ###################
  swTestControlPainIrrelevant <- stats::shapiro.test(x = df_controlPainIrrelevant$Eigen)
  swTestControlPainRelevant <- stats::shapiro.test(x = df_controlPainRelevant$Eigen)
  swTestDataPainRelevant <- stats::shapiro.test(x = df_dataPainRelevant$Eigen)
  swTestDataAiRelevant <- stats::shapiro.test(x = df_dataAiRelevant$Eigen)

  swTest <- tibble::tibble(species = c("controlPainIrrelevant","controlPainRelevant","dataPainRelevant","dataAiRelevant"),
                           w = c(swTestControlPainIrrelevant$statistic,
                                 swTestControlPainRelevant$statistic,
                                 swTestDataPainRelevant$statistic,
                                 swTestDataAiRelevant$statistic),
                           p = c(swTestControlPainIrrelevant$p.value,
                                 swTestControlPainRelevant$p.value,
                                 swTestDataPainRelevant$p.value,
                                 swTestDataAiRelevant$p.value),
                           mean = c(mean(df_controlPainIrrelevant$Eigen),
                                    mean(df_controlPainRelevant$Eigen),
                                    mean(df_dataPainRelevant$Eigen),
                                    mean(df_dataAiRelevant$Eigen)),
                           std = c(sd(df_controlPainIrrelevant$Eigen),
                                   sd(df_controlPainRelevant$Eigen),
                                   sd(df_dataPainRelevant$Eigen),
                                   sd(df_dataAiRelevant$Eigen))
                           )
  print(swTest)
  
  #######
  # ttest
  #######
  ttest01 <-stats::t.test(x = df_controlPainIrrelevant$Eigen, y = df_controlPainRelevant$Eigen, paired = FALSE)
  ttest02 <- stats::t.test(x = df_controlPainIrrelevant$Eigen, y = df_dataPainRelevant$Eigen, paired = FALSE)
  ttest03 <- stats::t.test(x = df_controlPainIrrelevant$Eigen, y = df_dataAiRelevant$Eigen, paired = FALSE)
  ttest04 <- stats::t.test(x = df_controlPainRelevant$Eigen, y = df_dataPainRelevant$Eigen, paired = FALSE)
  ttest05 <- stats::t.test(x = df_controlPainRelevant$Eigen, y = df_dataAiRelevant$Eigen, paired = FALSE)
  ttest06 <- stats::t.test(x = df_dataPainRelevant$Eigen, y = df_dataAiRelevant$Eigen, paired = FALSE)
  
  #####################
  # Mann Whitney U Test
  #####################
  mtest01 <- stats::wilcox.test(x = df_controlPainIrrelevant$Eigen, y = df_controlPainRelevant$Eigen)
  mtest02 <- stats::wilcox.test(x = df_controlPainIrrelevant$Eigen, y = df_dataPainRelevant$Eigen)
  mtest03 <- stats::wilcox.test(x = df_controlPainIrrelevant$Eigen, y = df_dataAiRelevant$Eigen)
  mtest04 <- stats::wilcox.test(x = df_controlPainRelevant$Eigen, y = df_dataPainRelevant$Eigen)
  mtest05 <- stats::wilcox.test(x = df_controlPainRelevant$Eigen, y = df_dataAiRelevant$Eigen)
  mtest06 <- stats::wilcox.test(x = df_dataPainRelevant$Eigen, y = df_dataAiRelevant$Eigen)
  
  p_value_ttest <- matrix(1:16, nrow=4, ncol = 4,
                         dimnames = list(c("controlPainIrrelevant","controlPainRelevant","dataPainRelevant","dataAiRelevant"),
                                         c("controlPainIrrelevant","controlPainRelevant","dataPainRelevant","dataAiRelevant")))
  
  ###################
  # result extraction
  ###################
  print(ttest01)
  
  
  
  for(i in 1:4){
    p_value_ttest[i,i] <- NA
  }
  p_value_ttest[1,2] <- ttest01$p.value
  p_value_ttest[1,3] <- ttest02$p.value
  p_value_ttest[1,4] <- ttest03$p.value
  
  p_value_ttest[2,1] <- ttest01$p.value
  p_value_ttest[2,3] <- ttest04$p.value
  p_value_ttest[2,4] <- ttest05$p.value
  
  p_value_ttest[3,1] <- ttest02$p.value
  p_value_ttest[3,2] <- ttest04$p.value
  p_value_ttest[3,4] <- ttest06$p.value
  
  p_value_ttest[4,1] <- ttest03$p.value
  p_value_ttest[4,2] <- ttest05$p.value
  p_value_ttest[4,3] <- ttest06$p.value
  
  statistic_ttest <- matrix(1:16, nrow=4, ncol = 4,
                            dimnames = list(c("controlPainIrrelevant","controlPainRelevant","dataPainRelevant","dataAiRelevant"),
                                            c("controlPainIrrelevant","controlPainRelevant","dataPainRelevant","dataAiRelevant")))
  
  for(i in 1:4){
    statistic_ttest[i,i] <- NA
  }
  statistic_ttest[1,2] <- ttest01$statistic
  statistic_ttest[1,3] <- ttest02$statistic
  statistic_ttest[1,4] <- ttest03$statistic
  
  statistic_ttest[2,1] <- ttest01$statistic
  statistic_ttest[2,3] <- ttest04$statistic
  statistic_ttest[2,4] <- ttest05$statistic
  
  statistic_ttest[3,1] <- ttest02$statistic
  statistic_ttest[3,2] <- ttest04$statistic
  statistic_ttest[3,4] <- ttest06$statistic
  
  statistic_ttest[4,1] <- ttest03$statistic
  statistic_ttest[4,2] <- ttest05$statistic
  statistic_ttest[4,3] <- ttest06$statistic
  
  p_value_mtest <- matrix(1:16, nrow=4, ncol = 4,
                          dimnames = list(c("controlPainIrrelevant","controlPainRelevant","dataPainRelevant","dataAiRelevant"),
                                          c("controlPainIrrelevant","controlPainRelevant","dataPainRelevant","dataAiRelevant")))
  
  for(i in 1:4){
    p_value_mtest[i,i] <- NA
  }
  
  p_value_mtest[1,2] <- mtest01$p.value
  p_value_mtest[1,3] <- mtest02$p.value
  p_value_mtest[1,4] <- mtest03$p.value
  
  p_value_mtest[2,1] <- mtest01$p.value
  p_value_mtest[2,3] <- mtest04$p.value
  p_value_mtest[2,4] <- mtest05$p.value
  
  p_value_mtest[3,1] <- mtest02$p.value
  p_value_mtest[3,2] <- mtest04$p.value
  p_value_mtest[3,4] <- mtest06$p.value
  
  p_value_mtest[4,1] <- mtest03$p.value
  p_value_mtest[4,2] <- mtest05$p.value
  p_value_mtest[4,3] <- mtest06$p.value
  
  statistic_mtest <- matrix(1:16, nrow=4, ncol = 4,
                            dimnames = list(c("controlPainIrrelevant","controlPainRelevant","dataPainRelevant","dataAiRelevant"),
                                            c("controlPainIrrelevant","controlPainRelevant","dataPainRelevant","dataAiRelevant")))
  
  for(i in 1:4){
    statistic_mtest[i,i] <- NA
  }
  
  statistic_mtest[1,2] <- mtest01$statistic
  statistic_mtest[1,3] <- mtest02$statistic
  statistic_mtest[1,4] <- mtest03$statistic
  
  statistic_mtest[2,1] <- mtest01$statistic
  statistic_mtest[2,3] <- mtest04$statistic
  statistic_mtest[2,4] <- mtest05$statistic
  
  statistic_mtest[3,1] <- mtest02$statistic
  statistic_mtest[3,2] <- mtest04$statistic
  statistic_mtest[3,4] <- mtest06$statistic
  
  statistic_mtest[4,1] <- mtest03$statistic
  statistic_mtest[4,2] <- mtest05$statistic
  statistic_mtest[4,3] <- mtest06$statistic
  
  
  print("T-test p Values")
  print(p_value_ttest)
  
  print("M-test p Values")
  print(p_value_mtest)
  print(mtest01)
    
  ##################
  # securing results
  ##################
  
  swTest %>%
    readr::write_csv(path = "./results/shapiroWilk.csv")
  
  p_value_ttest %>%
    tibble::as_tibble(rownames = "id") %>%
    readr::write_csv(path = "./results/ttest_pValue.csv")
  
  statistic_ttest %>%
    tibble::as_tibble(rownames = "id") %>%
    readr::write_csv(path = "./results/ttest_statistic.csv")
  
  p_value_mtest %>%
    tibble::as_tibble(rownames = "id") %>%
    readr::write_csv(path = "./results/mtest_pValue.csv")
  
  statistic_mtest %>%
    tibble::as_tibble(rownames = "id") %>%
    readr::write_csv(path = "./results/mtest_statistic.csv")
    
}

main()