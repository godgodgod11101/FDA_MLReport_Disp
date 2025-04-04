rpartRsl_bagging <- function(
  trainD, testD, trainD.proportion = 1, bagTimes = 10, seed = 7074, cutPt = .5
){
  # 預測機率表
  testD_nrow <- nrow(testD)
  df_pred_rpart <- data.frame(matrix(nrow = testD_nrow, ncol = bagTimes))
  names(df_pred_rpart) <- paste0("rpart_", 1:bagTimes)
  
  # bagging
  set.seed(seed)
  for (.i in 1:bagTimes) {
    
    # bag
    trainD_sn <- sample.int(n = nrow(trainD), size = 1*nrow(trainD), replace = T)
    trainD_bag <- trainD[trainD_sn, ]
    rm(trainD_sn)
    
    # mdl
    mdl_rpart <- rpart(Y~., data = trainD_bag, method = "class")
    
    # predict
    predProb_rpart <- predict(mdl_rpart, newdata = testD, type = "prob") %>% 
      {.[, "1"]} %>%    # 以欄位名稱取機率
      as.vector()
    
    # record in df
    df_pred_rpart[[.i]] <- predProb_rpart
  }
  
  pred_rpart <- df_pred_rpart %>% 
    mutate(
      rpart_avg = rowMeans(df_pred_rpart)
    ) %>%    # calculate prob. mean
    {.$rpart_avg} %>%    # get prob. mean
    {ifelse(.>cutPt, yes = 1, no = 0)}    # prob. to 0 or 1
  
  return(pred_rpart)
}
