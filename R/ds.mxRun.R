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
#' @param datasources a list of \code{\link{DSConnection-class}} objects obtained after login.
#' 
#'  
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
ds.mxRun <- function(model=NULL, intervals=FALSE, silent=FALSE, suppressWarnings=FALSE, unsafe=FALSE, checkpoint=FALSE, 
                     useSocket=FALSE, onlyFrontend=FALSE, useOptimizer=TRUE, newobj='mxRun_output', datasources=NULL){
  
  # look for DS connections
  if(is.null(datasources)){
    datasources <- datashield.connections_find()
  }
  
  # Throw an error message if required arguments are not set
  if(is.null(model)){
    stop(" Please provide an mxModel object to optimize! ", call.=FALSE)
  }
  
  # call the server side function that does the job
  cally <- call("mxRunDS", model, intervals, silent, suppressWarnings, 
                unsafe, checkpoint, useSocket, onlyFrontend, useOptimizer)
  datashield.assign(datasources, newobj, cally)
  
  # check that the new object has been created and display a message accordingly
  finalcheck <- isAssigned(datasources, newobj)
  
}
