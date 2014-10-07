#' 
#' @title Performs the optimization of a model to estimate parameters
#' @description This function is similar to OpenMx function \code{mxRun}. 
#' @details See details of the OpenMx function 'mxRun' in the package \code{OpenMx}.
#' @param model an mxModel object to be optimized.
#' @param intervals a boolean indicating whether to compute the specified confidence intervals.
#' @param silent a boolean indicating whether to print status to terminal.
#' @param suppressWarnings a boolean indicating whether to suppress warnings.
#' @param unsafe a boolean indicating whether to ignore errors.
#' @param checkpoint a boolean indicating whether to periodically write parameter values to a file.
#' @param useSocket a boolean indicating whether to periodically write parameter values to a socket.
#' @param onlyFrontend	a boolean indicating whether to run only front-end model transformations.
#' @param useOptimizer a boolean indicating whether to run only the log-likelihood of the current 
#' free parameter values but not move any of the free parameters.
#' @param newobj the name of the new object. By default the the name of the new object is "mxRun_output".
#' @param datasources a list of opal object(s) obtained after login in to opal servers;
#' these objects hold also the data assign to R, as \code{dataframe}, from opal datasources.
#' By default an internal function looks for 'opal' objects in the environment and sets this parameter. 
#' @return an mxModel object with free parameters updated to their final values. The return value contains 
#' an "output" slot with the results of optimization.
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
ds.mxRun <- function(model, intervals=FALSE, silent=FALSE, suppressWarnings=FALSE, unsafe=FALSE, checkpoint=FALSE, 
                     useSocket=FALSE, onlyFrontend=FALSE, useOptimizer=TRUE, newobj=NULL, datasources=NULL){
  
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
    stop(" Please provide an mxModel object to optimize! ", call.=FALSE)
  }
  
  # create a name by default if user did not provide a name for the new object
  if(is.null(newobj)){
    newobj <- "mxRun_output"
  }
  
  # call the server side function that does the job
  cally <- call("mxRunDS", model, intervals, silent, suppressWarnings, 
                unsafe, checkpoint, useSocket, onlyFrontend, useOptimizer)
  datashield.assign(datasources, newobj, cally)
  
  # check that the new object has been created and display a message accordingly
  finalcheck <- isAssigned(datasources, newobj)
  
}
