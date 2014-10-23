#' 
#' @title Changes an expression to include only characters allowed by opal
#' @description This is an internal function.
#' @details For security reasons some symbols cannot pass the opal parser and are hence should be replaced
#' by string characters.
#' @param input a string character
#' @keywords internal
#' @return an string character with the the 'OpemMx' symbols
#'
construct <- function(input){
  symbols <- c("^", "%*%", "*", "%x%", "%&%", "/", "+", "-", ":", "%^%")
  characters <- c("HAT", "PERCASTERIX", "ASTERIX", "PERCX", "PERCADD", "SLASH", "PLUS", "MINUS", "COLON", "PERCHAT")
  output <- input
  for(i in 1:length(symbols)){
    output <- gsub(symbols[i], characters[i], output, fixed=TRUE)
  }
  return(output)
}