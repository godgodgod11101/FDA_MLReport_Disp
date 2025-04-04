calculateAUC <- function(pred, trueCon){
  
  pred <- ifelse(pred == "1", 1, 0)
  trueCon <- ifelse(trueCon == "1", 1, 0)
  
  predRsl <- prediction(predictions = pred, labels = trueCon)
  
  pred_auc <- performance(predRsl, measure = "auc")
  return(pred_auc@y.values[[1]])
}
