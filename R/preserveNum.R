#' 
#' @title ensures values are kept in the right format
#' @description This is an internal function.
#' @details we do not want some argument given as matrix to be turned 
#' into vector so we ensure the right format is passed on to the server 
#' side function.
#' @param input a matrix or a numeric vector
#' @keywords internal
#' @return an string character, an expression to be included in a call expression
#'
preserveNum <- function (x) 
{
  if (is.matrix(x)) {
    return(paste0("matrix(c(", paste(as.numeric(x), collapse=","), "),", dim(x)[1], ",", dim(x)[2], ")"))
  }
  else {
    return(x)
  }
}