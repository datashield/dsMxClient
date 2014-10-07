#' 
#' @title deals with expressions with that include symbols 
#' @description This is an internal function.
#' @details Not required
#' @param input an expression for 'ds.mxAlgebra'
#' @param objnames an optional character string indicating the name of the object.
#' @param attnames a list, the dimnames attribute for the algebra
#' @keywords internal
#' @return an object of type call
#'
getCall <- function(input,objnames,attnames){
  # check if the input expression contains one of the symbols of 'omxSymbolTable'
  splitters <- c("\\b\\^\\b", "\\%\\*\\%", "\\*", "\\%x\\%", "\\%\\&\\%", "/", "\\+", "\\-", ":", "\\%\\^\\%")
  signs <- c("^", "%*%", "*", "%x%", "%&%", "/", "+", "-", ":", "%^%")
  for(i in 1:length(signs)){
    qq <- unlist(strsplit(input, split=splitters[i]))
    if(length(qq) > 1){ 
      symbol <- signs[i]
      if(length(symbol) > 0) {break} 
    }
  }
  
  # if a symbol if found create the call accordingly and if not form the call using the input object
  if(length(qq) > 1){
    if(symbol=="^"){sign <- "hat"}
    if(symbol=="%*%"){sign <- "percAsterix"}
    if(symbol=="*"){sign <- "asterix"}
    if(symbol=="%x%"){sign <- "percX"}
    if(symbol=="%&%"){sign <- "percAdd"}
    if(symbol=="/"){sign <- "slash"}
    if(symbol=="+"){sign <- "plus"}
    if(symbol=="%^%"){sign <- "percHat"}
    args <- list(qq, sign)
    myCall <- call("mxAlgebraDS", args, objnames, attnames)
  }else{
    qq <- unlist(strsplit(input, split="\\("))
    symbol <- qq[1]
    e1 <- unlist(strsplit(qq[2], split="\\)"))
    vars <- unlist(strsplit(e1, split=","))
    args <- list(vars,symbol)
    myCall <- call("mxAlgebraDS", args, objnames, attnames)
  }
  
  # throw a message and stop if the symbol in the input expression is not one those in the 'omxSymbolTable'
  mxSymbols <- c("solve", "t", "^", "%*%", "*", "%x%", "%&%", "/", "+", "-", "-", "cbind", "rbind", "det", 
                 "tr", "sum", "prod", "max", "min", "abs", "cos", "cosh", "sin", "sinh", "tan", "tanh", "exp", 
                 "log", "sqrt", "vech", "vechs", "diag2vec", "vec2diag", "omxMnor", "omxAllInt", ":", "%^%", 
                 "rvectorize", "cvectorize", "eigenvec", "eigenval", "ieigenvec", "ieigenval", "omxNot", 
                 "omxSelectRows", "omxSelectCols", "omxSelectRowsAndCols", "mean", "omxGreaterThan", "omxLessThan", 
                 "omxAnd", "omxOr", "omxApproxEquals", "omxExponential", "omxExponential", "chol", "cov2cor")
  
  if(!(symbol %in% mxSymbols)){
    stop("Please provide a valid operator: any symbol from the 'omxSymbolTable' but '['!", call.=FALSE)
  }
      
  return(myCall)
}