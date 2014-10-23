#' 
#' @title Evaluates values in an mxModel
#' @description This function is similar to OpenMx function \code{mxEval}. 
#' @details See details of the OpenMx function 'mxModel' in the package \code{OpenMx}.
#' @param expression a string character an arbitrary R expression.
#' @param model  a string character, the name of the model in which to evaluate the expression.
#' @param compute	a boolean, if TRUE then compute the value of algebra expressions.
#' @param show	if TRUE then print the translated expression.
#' @param defvar.row an integer, the row number for definition variables when compute=TRUE.
#' @param cacheBack a boolean, if TRUE then return the list pair (value, cache).
#' @param datasources a list of opal object(s) obtained after login in to opal servers;
#' these objects hold also the data assign to R, as \code{dataframe}, from opal datasources.
#' By default an internal function looks for 'opal' objects in the environment and sets this parameter. 
#' @return the evaluation of an expression.
#' @author Gaye, A.
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
ds.mxEval <- function(expression=NULL, model=NULL, compute=FALSE, show=FALSE, defvar.row=1, cacheBack=FALSE, datasources=NULL){
  
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
  
  # Throw an error message if required arguments are not set
  if(is.null(expression)){
    stop(" Please provie the expression to evaluate!", call.=FALSE)
  }  
  if(is.null(model)){
    stop(" Please provie the the name of the model in which to evaluate the expression!", call.=FALSE)
  }  
  
  # contruct an expression that is acceptable for the opal parser
  express <- construct(expression)
  
  # call the server side function that does the job
  cally <- call("mxEvalDS", eval(express), eval(model), compute, show, defvar.row, cacheBack)
  
  output <- datashield.aggregate(datasources, cally)
  
  return(output)
}
