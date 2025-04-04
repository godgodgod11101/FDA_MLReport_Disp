lgsRsl_bagging <- function(
  trainD, testD, trainD.proportion = 1, bagTimes = 10, seed = 7074, cutPt = .5
){
  # 更改Y的level順序
  if(levels(trainD$Y)[[1]] == "1"){
    trainD$Y <- factor(trainD$Y, levels = c("0", "1"))
  }
  
  # 預測機率表
  testD_nrow <- nrow(testD)
  df_pred_lgs <- data.frame(matrix(nrow = testD_nrow, ncol = bagTimes))
  names(df_pred_lgs) <- paste0("lgs_", 1:bagTimes)
  
  # bagging
  set.seed(seed)
  for (.i in 1:bagTimes) {
    
    # bag
    trainD_sn <- sample.int(n = nrow(trainD), size = 1*nrow(trainD), replace = T)
    trainD_bag <- trainD[trainD_sn, ]
    rm(trainD_sn)
    
    # mdl
    mdl_lgs <- glm(
      Y~., data = trainD_bag, 
      family = "binomial"
    )
    
    # predict
    predProb_lgs <- predict(mdl_lgs, newdata = testD, type = "response") %>% 
      as.vector()
    
    # record in df
    df_pred_lgs[[.i]] <- predProb_lgs
  }
  
  pred_lgs <- df_pred_lgs %>% 
    mutate(
      lgs_avg = rowMeans(df_pred_lgs)
    ) %>%    # calculate prob. mean
    {.$lgs_avg} %>%    # get prob. mean
    {ifelse(.>cutPt, yes = 1, no = 0)}    # prob. to 0 or 1
  
  return(pred_lgs)
}
