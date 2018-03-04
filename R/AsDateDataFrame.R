AsDateDataFrame <- function(data, variable){
  if(!require(dplyr)){install.packages("dplyr");library(dplyr)}
  idx <- which(names(data) == variable)
  data2change <- data[idx]
  data2stay <- data[-idx]
  invisible(tbl_df(sapply(data2change, as.Date)))
}

