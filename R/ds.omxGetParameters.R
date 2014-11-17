#' 
#' @title Return a vector of the free parameters in the model.
#' @description This function is similar to OpenMx function \code{omxGetParameters}. 
#' @details See details of the OpenMx function 'omxGetParameters' in the package \code{OpenMx}.
#' @param model a MxModel object
#' @param indep  fetch parameters from independent submodels
#' @param free fetch either free parameters (TRUE), or fixed parameters or both types. Default value is TRUE.
#' @param fetch which attribute of the parameters to fetch. Default choice is ‘values’
#' @return a vector of free (by default) parameters in the model
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
ds.omxGetParameters <- function(model, indep=FALSE, free=c(TRUE, FALSE, NA), fetch=c('values', 'free', 'lbound', 'ubound', 'all'), datasources=NULL) {
  
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
  if(is.null(model)){
    stop(" Please provide the name of a model! ", call.=FALSE)
  }
  
  
  # call the server side function that does the job
  if(!is.na(fetch[1])){ fetch <- paste0("c('", paste(fetch, collapse="','"), "')")}
  cally <- paste0("omxGetParameters(", model, ", '", indep, "', ", free, ", ", fetch, ")")
  # cally = call("omxGetParameters", model, indep, eval(free), eval(fetch))
  output = datashield.aggregate(datasources, cally)
  
  return(output)
  
}
