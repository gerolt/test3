#' Mode, orderly
#'
#' The most frequent value of a random variable. You can see the number or value of mode.
#'
#' @param x object to be ordered.
#' @param order Order of modes.
#' @param type Type of returned value. Count, Value.
#' @return The most frequent value or number of a random variable.
#' @examples
#' GetMode(iris$Species, order=2, type='count') # The number of Second mode
#' GetMode(iris$Species, order=1, type='value') # The Value of first mode
#' @export
#' @importFrom dplyr
GetMode <- function(x, order=1, type='value'){
  # mode count
  count_tab <- sort(table(x), decreasing = T)
  mode_count <- count_tab[order]
  # mode value
  value_tab <- names(count_tab)
  mode_value <- value_tab[order]
  if(type=='value') {
    result <- mode_value
  } else if(type=='count'){
    result <- mode_count
  }
  names(result) <- NULL
  return(result)
}
