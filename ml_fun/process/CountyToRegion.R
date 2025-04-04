

county2region_man <- function(county) {
  
  if (
    county %in% c("基隆市", "台北市", "新北市", "桃園市", "宜蘭縣", "花蓮縣", "台東縣")
  ) {return("北區")} 
  else if (
    county %in% c("新竹市", "新竹縣", "苗栗縣", "連江縣", "台中市", "彰化縣", "南投縣", "雲林縣")
  ) {return("中區")}
  else if (
    county %in% c("高雄市", "台南市", "嘉義縣", "嘉義市", "屏東縣", "澎湖縣", "金門縣")
  ) {return("南區")} 
  else {return(NULL)}
  
}

county2region <- function(county) {
  
  if (
    county %in% c("基隆市", "台北市", "新北市", "桃園市", "宜蘭縣", "新竹市", "新竹縣")
  ) {return("北部")} 
  else if (
    county %in% c("苗栗縣", "台中市", "彰化縣", "南投縣", "雲林縣")
  ) {return("中部")}
  else if (
    county %in% c("嘉義市", "嘉義縣", "台南市", "高雄市", "屏東縣", "澎湖縣")
  ) {return("南部")}
  else if (
    county %in% c("花蓮縣", "台東縣")
  ) {return("東部")}
  else if (
    county %in% c("金門縣", "連江縣")
  ) {return("離島")} 
  else {return(NULL)}
  
}
