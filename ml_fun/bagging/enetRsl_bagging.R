enetRsl_bagging <- function(
  trainD, testD, trainD.proportion = 1, bagTimes = 10, seed = 7074, cutPt = .5
){
  # 更改Y的level順序
  if(levels(trainD$Y)[[1]] == "1"){
    trainD$Y <- factor(trainD$Y, levels = c("0", "1"))
  }
  
  # 預測機率表
  testD_nrow <- nrow(testD)
  df_pred_enet <- data.frame(matrix(nrow = testD_nrow, ncol = bagTimes))
  names(df_pred_enet) <- paste0("enet_", 1:bagTimes)
  
  # dummy var. mx. (testD)
  testD_mx <- model.matrix(Y~., data = testD)[, -1]
  
  # bagging
  set.seed(seed)
  for (.i in 1:bagTimes) {
    
    # 套袋（抽後放回）
    trainD_sn <- sample.int(n = nrow(trainD), size = 1*nrow(trainD), replace = T)
    trainD_bag <- trainD[trainD_sn, ]
    rm(trainD_sn)
    
    # dummy var. mx. (trainD_bag)
    trainD_bag_mx <- model.matrix(Y~., data = trainD_bag)[, -1]
    
    # get lambda.1se (s = lambda)
    mdl_cvenet <- glmnet::cv.glmnet(
      x = trainD_bag_mx, y = trainD_bag$Y, 
      family = "binomial", 
      alpha = .5, 
      nfolds = 5
    )
    lam_1se <- mdl_cvenet$lambda.1se
    
    # mdl
    mdl_enet <- glmnet(
      x = trainD_bag_mx, y = trainD_bag$Y, 
      family = "binomial", 
      alpha = .5
    )
    
    # predict
    predProb_enet <- predict(mdl_enet, newx = testD_mx, type = "response", s = lam_1se) %>% 
      as.vector()
    
    # record in df
    df_pred_enet[[.i]] <- predProb_enet
  }
  
  pred_enet <- df_pred_enet %>% 
    mutate(
      enet_avg = rowMeans(df_pred_enet)
    ) %>%    # calculate prob. mean
    {.$enet_avg} %>%    # get prob. mean
    {ifelse(.>cutPt, yes = 1, no = 0)}    # prob. to 0 or 1
  
  return(pred_enet)
}

