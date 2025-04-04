

nzvTbl <- function(df, excluCol, cutPt = 95/5) {
  
  rsl <- NULL
  
  for (i in seq_along(df)) {
    
    if (names(df)[[i]] %in% excluCol) {next}
    
    if (!is.numeric(df[[i]])) {
      df[[i]] <- df[[i]] %>% as.factor() %>% as.integer()
    }
    
    vName <- names(df)[[i]]
    
    # 零變異因子
    x <- nearZeroVar(df[[i]], saveMetrics = T)[["zeroVar"]]
    if (x) {
      rsl_r <- tibble_row(
        因子 = vName, 
        FrequencyRatio = Inf, 
        NearZeroVariance = T
      )
      
      rsl <- bind_rows(rsl, rsl_r)
      next
    }
    
    # 正常因子
    fr <- nearZeroVar(df[[i]], saveMetrics = T)[["freqRatio"]]
    nzv <- fr >= cutPt    # 閾值
    
    rsl_r <- tibble_row(
      因子 = vName, 
      FrequencyRatio = fr, 
      NearZeroVariance = nzv
    )
    
    rsl <- bind_rows(rsl, rsl_r)
  }
  
  return(rsl)
  
}
