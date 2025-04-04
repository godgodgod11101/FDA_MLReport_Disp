gbmRsl <- function(trainD, testD, cutPt = .5, seed = 7074){
  
  # gbm不用改應變數level順序
  
  # mdl
  set.seed(seed)    # for bag.fra, train.fra, cv.fold
  
  mdl_gbm <- gbm(
    # dep.var. must be character
    formula = as.character(Y)~., data = trainD, 
    distribution = "bernoulli",    # default
    n.trees = 1000,    # 迭代次數
    shrinkage = 0.1,    # default
    cv.fold = 5
  )
  
  # predict
  pred_gbm <- predict(    # 採用mdl_gbm中最佳迭代次數（n.trees）的模型
    mdl_gbm, newdata = testD, 
    type = "response"
  ) %>%
    {ifelse(.>cutPt, 1, 0)}
  
  return(pred_gbm)
}
