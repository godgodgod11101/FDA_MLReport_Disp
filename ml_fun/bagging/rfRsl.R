rfRsl <- function(trainD, testD, cutPt = .5, seed = 7074){
  
  # Random Forest含bagging概念！
  
  # 自變數個數
  n_features <- length(setdiff(names(trainD), "Y"))
  
  # mdl
  mdl_rf <- ranger(
    formula = Y~., data = trainD, 
    num.trees = 500,    # default
    mtry = floor(n_features/3),    # 每次用三分之一的自變數（隨機）建構一棵樹
    replace = TRUE,    # default 
    # sample.fraction = ifelse(replace, 1, 0.632)
    #   每次隨機抽取樣本（抽後放回、等量）建構一棵樹
    probability = T,    # probability forest
    seed = seed
  )
  
  # predict
  pred_rf <- predict(
    mdl_rf, data = testD, 
    type = "response"    # default
  ) %>% 
    {.$predictions} %>% 
    {.[, "1"]} %>%    # 以欄位名稱取機率
    {ifelse(.>=cutPt, 1, 0)}
  
  return(pred_rf)
}
