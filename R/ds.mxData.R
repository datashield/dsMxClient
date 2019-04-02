#' 
#' @title Creates a new MxData object
#' @description This function is similar to OpenMx function \code{mxData}. 
#' @details The same output as the one returned by 'mxData' in the package \code{OpenMx} is generated
#' but an empty (all values = NA) dataframe is returned if the output dataframe is not valid according
#' to the DataSHIELD criteria (i.e. if the dataframe has less than the allowed number of observations/rows.
#' If the output dataframe has 0 observations (number of rows = 0), a data fram with one empty row is returned.
#' @param observed  a string, the name of a matrix or data.frame which provides data to the MxData object.
#' @param type A character string defining the type of data in the 'observed' argument. Must be one of "raw", 
#' "cov", "cor", or "sscp".
#' @param means An optional vector of means for use when 'type' is "cov", or "cor".
#' @param numObs	The number of observations in the data supplied in the 'observed' argument. Required unless 
#' 'type' equals "raw".
#' @param newobj the name of the new variable. By default the name of the new object is "new_mxData".
#' @param datasources a list of \code{\link{DSConnection-class}} objects obtained after login.
#' 
#'  
#' @return a new mxData object.
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
ds.mxData <- function(observed=NULL, type=NULL, means=NA, numObs=NA, newobj='new_mxData', datasources=NULL){
  
  # look for DS connections
  if(is.null(datasources)){
    datasources <- datashield.connections_find()
  }
  
  # Throw an error message if required arguments are not set
  if(is.null(observed)){
    stop(" Please provie the name of a matrix or data.frame for the argument 'observed'! ", call.=FALSE)
  }
  if(is.null(type)){
    stop(" Please provide a string defining the 'type' of data in the 'observed' argument! ", call.=FALSE)
  }
  
  # call the server side function that does the job
  cally <- paste0("mxData(", observed, ", '", type, "', ", means, ", ", numObs, ")")
  datashield.assign(datasources, newobj, as.symbol(cally))
  
  # check that the new object has been created and display a message accordingly
  finalcheck <- isAssigned(datasources, newobj)
  
}
