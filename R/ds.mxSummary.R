#' 
#' @title Summarizes the results of an mx model optimization
#' @description This function is similar to base function 'summary'. 
#' @details to be written 
#' @param mxObj a character string, the name of an object obtained after optimizing a model via 'ds.mxRun'.
#' @param datasources a list of \code{\link{DSConnection-class}} objects obtained after login.
#' 
#'  
#' @return the summary of the output of an 'ds.mxRun' command.
#' @author Gaye, A.
#' @export
#' @examples {
#'
#' }
#' 
ds.mxSummary <- function(mxObj, datasources=NULL){
  
  # look for DS connections
  if(is.null(datasources)){
    datasources <- datashield.connections_find()
  }

  # return the summary
  cally <- paste0("mxSummaryDS(", mxObj, ")")
  datashield.aggregate(datasources, as.symbol(cally))  
  
}
