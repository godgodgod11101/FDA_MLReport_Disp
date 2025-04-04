nbRsl_bagging <- function(
  trainD, testD, trainD.proportion = 1, bagTimes = 10, seed = 7074, cutPt = .5
){
  # 預測機率表
  testD_nrow <- nrow(testD)
  df_pred_nb <- data.frame(matrix(nrow = testD_nrow, ncol = bagTimes))
  names(df_pred_nb) <- paste0("nb_", 1:bagTimes)
  
  # bagging
  set.seed(seed)
  for (.i in 1:bagTimes) {
    
    # bag
    trainD_sn <- sample.int(n = nrow(trainD), size = 1*nrow(trainD), replace = T)
    trainD_bag <- trainD[trainD_sn, ]
    rm(trainD_sn)
    
    # mdl
    mdl_nb <- naiveBayes(formula = Y~., data = trainD_bag)
    
    # predict
    predProb_nb <- predict(
      mdl_nb, newdata = testD, 
      type = "raw"    # type = "class" for category
    ) %>% 
      {.[, "1"]}    # 以欄位名稱取機率
    
    # record in df
    df_pred_nb[[.i]] <- predProb_nb
  }
  
  pred_nb <- df_pred_nb %>% 
    mutate(
      nb_avg = rowMeans(df_pred_nb)
    ) %>%    # calculate prob. mean
    {.$nb_avg} %>%    # get prob. mean
    {ifelse(.>cutPt, yes = 1, no = 0)}    # prob. to 0 or 1
  
  return(pred_nb)
}
