some2some <- function(data, before, after){
  for(i in 1:nrow(data)){
    data[[i]] <- ifelse(data[[i]]==before, after, data[[i]])
  }
  invisible(data)
}
