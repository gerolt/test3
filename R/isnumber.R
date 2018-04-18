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


is.number <- function(x){
  crit <- class(x)
  if(length(crit)>=2){
    crit <- paste(crit, collapse = ' ')
  }
  try(if( crit %in% c('numeric','integer', 'Date') )
    return(TRUE) else return(FALSE),silent = T)
}
