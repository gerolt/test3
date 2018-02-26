#' Numeric, Integer Vectors
#'
#' Creates or coerces objects of type "numeric" or "integer".
#'
#' @param x object to be coerced or tested.
#' @return TRUE / FALSE
#' @examples
#' is.number(iris$Sepal.Length)
#' is.number(iris$Species)
#' @export
#' @importFrom dplyr

install.packages('dplyr');library(dplyr)
is.number <- function(x) {
  try(if(class(x) %in% c('numeric','integer')) return(TRUE) else return(FALSE),silent = T)
}
