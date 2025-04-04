c50Rsl_bagging <- function(
  trainD, testD, trainD.proportion = 1, bagTimes = 10, seed = 7074, cutPt = .5
){
  # 預測機率表
  testD_nrow <- nrow(testD)
  df_pred_c50 <- data.frame(matrix(nrow = testD_nrow, ncol = bagTimes))
  names(df_pred_c50) <- paste0("c50_", 1:bagTimes)
  
  # bagging
  set.seed(seed)
  for (.i in 1:bagTimes) {
    
    # bag
    trainD_sn <- sample.int(n = nrow(trainD), size = 1*nrow(trainD), replace = T)
    trainD_bag <- trainD[trainD_sn, ]
    rm(trainD_sn)
    
    # mdl
    mdl_c50 <- C5.0(Y~., data = trainD_bag)
    
    # predict
    predProb_c50 <- predict(mdl_c50, newdata = testD, type = "prob") %>% 
      {.[, "1"]} %>%    # 以欄位名稱取機率
      as.vector()
    
    # record in df
    df_pred_c50[[.i]] <- predProb_c50
  }
  
  pred_c50 <- df_pred_c50 %>% 
    mutate(
      c50_avg = rowMeans(df_pred_c50)
    ) %>%    # calculate prob. mean
    {.$c50_avg} %>%    # get prob. mean
    {ifelse(.>cutPt, yes = 1, no = 0)}    # prob. to 0 or 1
  
  return(pred_c50)
}
