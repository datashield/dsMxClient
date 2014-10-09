#' 
#' @title Creates a new mxFIMLObjective object
#' @description This function is similar to OpenMx function \code{mxFIMLObjective}. 
#' @details See details of the OpenMx function 'mxFIMLObjective' in the package \code{OpenMx}.
#' @param covariance a character string indicating the name of the expected covariance algebra.
#' @param means	a character string indicating the name of the expected means algebra.
#' @param dimnames An optional character vector to be assigned to the dimnames of the covariance and means algebras.
#' @param thresholds An optional character string indicating the name of the thresholds matrix.
#' @param vector a logical value indicating whether the objective function result is the likelihood vector.
#' @param threshnames An optional character vector to be assigned to the column names of the thresholds matrix.
#' @param newobj the name of the new object. By default the the name of the new object is "new_mxFIMLObjective".
#' @param datasources a list of opal object(s) obtained after login in to opal servers;
#' these objects hold also the data assign to R, as \code{dataframe}, from opal datasources.
#' By default an internal function looks for 'opal' objects in the environment and sets this parameter. 
#' @return a new MxFIMLObjective object. MxFIMLObjective objects should be included with models with referenced MxAlgebra, 
#' MxData and MxMatrix objects
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
ds.mxFIMLObjective <- function(covariance=NULL, means=NULL, dimnames=NA, thresholds=NA, vector=FALSE, threshnames=dimnames, newobj='new_mxFIMLObjective', datasources=NULL){
  
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
  if(is.null(covariance)){
    stop(" Please provide a character string indicating the name of the expected covariance algebra! ", call.=FALSE)
  }
  if(is.null(means)){
    stop(" Please provide a character string indicating the name of the expected means algebra! ", call.=FALSE)
  }  
  
  # call the server side function that does the job
  cally <- call("mxFIMLObjective", covariance, means, dimnames, thresholds, vector, threshnames)
  datashield.assign(datasources, newobj, cally)
  
  # check that the new object has been created and display a message accordingly
  finalcheck <- isAssigned(datasources, newobj)
  
}
