AsDateDataFrame <- function(data, variable){
  if(!require(dplyr)){install.packages("dplyr");library(dplyr)}
  vorder <- names(data)
  idx <- which(names(data) == variable)
  data2change <- data[idx]
  data2stay <- data[-idx]
  dataachange <- tbl_df(sapply(data2change, as.Date))
  data <- cbind(dataachange, data2stay)
  data <- data[vorder]
  invisible(data)
}

