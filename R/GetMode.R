GetMode <- function(x, order=1, type='value'){
  # mode count
  count_tab <- sort(table(x), decreasing = T)
  mode_count <- count_tab[order]
  # mode value
  value_tab <- ifelse(is.null(value_tab), NA, names(count_tab))
  mode_value <- value_tab[order]
  if(type=='value') {
    result <- mode_value
  } else if(type=='count'){
    result <- mode_count
  }
  names(result) <- NULL
  return(result)
}

