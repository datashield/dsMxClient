#' 
#' @title Creates a new MxMatrix object
#' @description This function is similar to OpenMx function \code{mxMatrix}. 
#' @details See details of the OpenMx function 'mxMatrix' in the package \code{OpenMx}.
#' @param type a character string indicating the matrix type, where type indicates the range 
#' of values and #' equalities in the matrix. Must be one of: 'Diag', 'Full', 'Iden', 'Lower', 
#' 'Sdiag', 'Stand', 'Symm', 'Unit', or 'Zero'.
#' @param nrow	the desired number of rows. One or both of 'nrow' and 'ncol' is required when 
#' 'values', 'free', 'labels', 
#' 'lbound', and 'ubound' arguments are not matrices, depending on the matrix type.
#' @param ncol the desired number of columns. One or both of 'nrow' and 'ncol' is required when 
#' 'values', 'free', 'labels', 'lbound', and 'ubound' arguments are not matrices, depending on 
#' the matrix type.
#' @param free a vector or matrix of logicals for free parameter specification. A single 'TRUE' 
#' or 'FALSE' will set all allowable variables to free or fixed, respectively.
#' @param values a vector or matrix of numeric starting values. By default, all values are set to zero.
#' @param labels a vector or matrix of characters for variable label specification.
#' @param lbound a vector or matrix of numeric lower bounds. Default bounds are specified with an NA.
#' @param ubound a vector or matrix of numeric upper bounds. Default bounds are specified with an NA.
#' @param byrow	logical. If 'FALSE' (default), the 'values', 'free', 'labels', 'lbound', and 'ubound' 
#' matrices are populated by column rather than by row.
#' @param dimnames	list. The dimnames attribute for the matrix: a list of length 2 giving the row and 
#' column names respectively. An empty list is treated as NULL, and a list of length one as row names. 
#' The list can be named, and the list names will be used as names for the dimensions.
#' @param name an optional character string indicating the name of the MxMatrix object
#' @param newobj the name of the new object. By default the name of the new object is "new_mxMatrix".
#' @param datasources a list of \code{\link{DSConnection-class}} objects obtained after login.
#' 
#'  
#' @return a new mxMatrix object, which consists of a 'values' matrix of numeric starting values, a 'free' 
#' matrix describing free parameter specification, a 'labels' matrix of labels for the variable names, and 
#' 'lbound' and 'ubound' matrices of the lower and upper parameter bounds.
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
ds.mxMatrix <- function(type="Full", nrow=NA, ncol=NA, free=FALSE, values=NA, labels=NA, lbound=NA, ubound=NA, byrow=FALSE, dimnames=NA, name=NA, newobj='new_mxMatrix', datasources=NULL){
  
  # look for DS connections
  if(is.null(datasources)){
    datasources <- datashield.connections_find()
  }

  # call the server side function that does the job
  values <- preserveNum(values)
  lbound <- preserveNum(lbound)
  ubound <- preserveNum(ubound)  
  if(!is.na(labels[1])){ labels <- paste0("c('", paste(labels, collapse="','"), "')")}
  if(!is.na(dimnames[[1]])){ dimnames <- paste0("list('", paste(unlist(dimnames), collapse="','"), "')")}
  if(is.na(name)){
    cally <- paste0("mxMatrix('", type, "',", nrow, ",", ncol, ",", free, ",", values, ",", labels, ",", lbound, ",", ubound, ",", byrow, ",", dimnames, ",",name,")")    
  }else{
    cally <- paste0("mxMatrix('", type, "',", nrow, ",", ncol, ",", free, ",", values, ",", labels, ",", lbound, ",", ubound, ",", byrow, ",", dimnames, ",'",name,"')")    
  }
  datashield.assign(datasources, newobj, as.symbol(cally))
  
  # check that the new object has been created and display a message accordingly
  finalcheck <- isAssigned(datasources, newobj)
}
