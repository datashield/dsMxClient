#'
#' @title Creates a new MxAlgebra object
#' @description This function is similar to OpenMx function \code{mxAlgebra}.
#' @details See details of the OpenMx function 'mxAlbegra
#' @param expression an R expression of OpenMx-supported matrix operators and matrix functions.
#' @param name an optional character string indicating the name of the object.
#' @param dimnames a list of character strings, the dimnames attribute for the algebra
#' @param newobj the name of the new variable. If this argument is set to NULL, the name of the new
#' object is "mxAlgebra_output".
#' @param datasources a list of \code{\link{DSConnection-class}} objects obtained after login.
#'
#'  
#' @return an object of type 'mxAlgebra'
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
#' @import DSI
ds.mxAlgebra = function(expression=NULL, name=NA, dimnames=NA, newobj='mxAlgebra_output', datasources=NULL){

  # look for DS connections
  if(is.null(datasources)){
    datasources <- datashield.connections_find()
  }

  # call the inernal function that generates the command for the server side function
  # because OpenMx does uses symbols from its 'omxSymbolTable' we need to deal with that
  # so we first contruct the argument 'expression'
  express <- construct(expression)
  if(!is.na(dimnames[[1]])){ dimnames <- paste0("list('", paste(unlist(dimnames), collapse="','"), "')")}
  if(is.na(name)){
    cally <- paste0("mxAlgebraDS('", express, "', ", name, ", ", dimnames, ")")
  }else{
    cally <- paste0("mxAlgebraDS('", express, "', '", name, "', ", dimnames, ")")
  }

  # call the server side function that does the job
  datashield.assign(datasources, newobj, as.symbol(cally))

  # check that the new object has been created and display a message accordingly
  finalcheck <- isAssigned(datasources, newobj)

}
