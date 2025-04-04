varNameConverter <- function(data, dep, exclu){
  
  i = 0    # 自變數計數器
  
  for (.x in seq_along(data)) {
    
    if(names(data)[[.x]] %in% exclu){
      next
    } else if(names(data)[[.x]] == dep){
      names(data)[[.x]] <- "Y"
    } else{
      i = i+1
      if(i < 10){
        names(data)[[.x]] <- paste0("X0", i)
      } else{
        names(data)[[.x]] <- paste0("X", i)
      }
    }
  }
  
  return(data)
}
