cfMxIndex <- function(..., simple = FALSE){
  
  list_cfMx <- list(...)    # 將input包裝成list
  
  # 產出評估指標表（依input順序）
  df_index <- data.frame()
  
  if(simple){
    for (i in seq_along(list_cfMx)) {
      
      df_index_temp <- data.frame(
        Sen. = list_cfMx[[i]]$byClass[["Sensitivity"]], 
        PPV = list_cfMx[[i]]$byClass[["Pos Pred Value"]], 
        F1 = list_cfMx[[i]]$byClass[["F1"]]
      )
      
      df_index <- rbind(df_index, df_index_temp)
    }
  } else{
    for (i in seq_along(list_cfMx)) {
      
      df_index_temp <- data.frame(
        Acc. = list_cfMx[[i]]$overall[["Accuracy"]], 
        Sen. = list_cfMx[[i]]$byClass[["Sensitivity"]], 
        Spe. = list_cfMx[[i]]$byClass[["Specificity"]], 
        PPV = list_cfMx[[i]]$byClass[["Pos Pred Value"]], 
        NPV = list_cfMx[[i]]$byClass[["Neg Pred Value"]], 
        F1 = list_cfMx[[i]]$byClass[["F1"]]
      )
      
      df_index <- rbind(df_index, df_index_temp)
    }
  }
  
  return(df_index)
}
