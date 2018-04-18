Quantile <- function(x, p){
  if(!require(lubridate)) {install.packages("lubridate"); library(lubridate)}
  if(lubridate::is.Date(x)){
    result <- as.Date(quantile(as.numeric(x), p, na.rm = T),
                      origin = "1970-01-01")
  } else {
    result <- quantile(x,p,na.rm = T)
  }
  return(result)
}
