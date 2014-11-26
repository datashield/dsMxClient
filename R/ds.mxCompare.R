#' 
#' @title Assign Model Parameters
#' @description This function is similar to OpenMx function \code{mxCompare}. 
#' @details See details of the OpenMx function 'mxCompare' in the package \code{OpenMx}.
#' @param base a MxModel object or list of MxModel objects
#' @param comparison a MxModel object or list of MxModel objects
#' @param all a boolean value on whether to compare all bases with all comparisons. Default to FALSE
#' @return a table of comparison containing the name of the base model,
#' the name of the comparison model, number of estimated parameters in the model,
#' minus 2*log-likelihood of the comparison model, degrees of freedom of the comparison model,
#' Akaike's Information Criterion for the comparison model, Difference in minus 2*log-likelihoods 
#' of the base and comparison models, Difference in degrees of freedoms of the base and comparison 
#' models and P-value for likelihood ratio test based on diffLL and diffdf values
#' @author Gaye, A., Kutschke, J.
#' @export
#' @examples {
#'
#' }
#' @references   Steven M. Boker, Michael C. Neale, Hermine H. Maes, Michael J. Wilde, Michael Spiegel, Timothy R. Brick, 
#' Jeffrey Spies, Ryne Estabrook, Sarah Kenny, Timothy C., Bates, Paras Mehta, and John Fox. (2011) 
#' OpenMx: An Open Source Extended Structural Equation Modeling Framework. Psychometrika. 
#' Steven M. Boker, Michael C. Neale, Hermine H. Maes, Michael J. Wilde, Michael Spiegel, Timothy R. Brick, Ryne Estabrook, 
#' Timothy C. Bates, Paras Mehta, Timo von Oertzen, Ross J. Gore, Michael D. Hunter, Daniel C. Hackett, Julian Karch and 
#' Andreas M. Brandmaier. (2012) OpenMx 1.3 User Guide.
#' 
ds.mxCompare <- function(base, comparison, all=FALSE, datasources=NULL) {
  
  # if no opal login details were provided look for 'opal' objects in the environment
  if(is.null(datasources)){
    findLogin <- getOpals()
    if(findLogin$flag == 1){
      datasources <- findLogin$opals
    }else{
      if(findLogin$flag == 0){
        stop(" Are yout logged in to any server? Please provide a valid opal login object! ", call.=FALSE)
      }else{
        message(paste0("More than one list of opal login object were found: '", paste(findLogin$opals,collapse="', '"), "'!"))
        userInput <- readline("Please enter the name of the login object you want to use: ")
        datasources <- eval(parse(text=userInput))
        if(class(datasources[[1]]) != 'opal'){
          stop("End of process: you failed to enter a valid login object", call.=FALSE)
        }
      }
    }
  }
  
#   # Throw an error message if required arguments are not set
#   if(missing(base)){
#     stop("'base' argument be a MxModel object or list of MxModel objects", call.=FALSE)
#   }
  
  
  # call the server side function that does the job
#   if (!missing(base)) {base = paste0("c(", paste(base, collapse=", "), ")")}
#   if (!missing(comparison)) {comparison = paste0("c(", paste(comparison, collapse=", "), ")")}

  
#   cally <- paste0("mxCompareDS(base=", base, ", comparison=", comparison, ", all=", all, ")")
  cally = call('mxCompareDS', base, comparison, all)
  output = datashield.aggregate(datasources, cally)
  
  return(output)
  
}
