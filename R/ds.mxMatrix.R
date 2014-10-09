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
#' @param datasources a list of opal object(s) obtained after login in to opal servers;
#' these objects hold also the data assign to R, as \code{dataframe}, from opal datasources.
#' By default an internal function looks for 'opal' objects in the environment and sets this parameter. 
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
  
  # generate the arguments of the 'mxMatrix' function on the server site and call the function
  # 'mxMatrix' seems to work only if some of its arguments are already defined on the server site
  # that is why I am using this work around and not the lesser 'hassly' solution of just constructing
  # a call that is passed on the 'datashield.assign' function as done in most of the functions.
  args <- list(type, nrow, ncol, free, values, labels, lbound, ubound, byrow, dimnames, name)
  argnames <- c("typeDS", "nrowDS", "ncolDS", "freeDS", "valuesDS", "labelsDS", "lboundDS", "uboundDS", "byrowDS", "dimnamesDS", "nameDS")
  for(i in 1:length(args)){
    if(is.logical(args[[i]])){
      cally <- paste0("c(", paste(args[[i]], collapse=","), ")")    
    }else{
      if(is.numeric(args[[i]])){
        cally <- paste0("c(", paste(args[[i]], collapse=","), ")")
      }else{
        cally <- paste0("c('", paste(args[[i]], collapse="','"), "')")     
      }
    }
    datashield.assign(datasources, argnames[i], as.symbol(cally))
  }
  datashield.assign(opals, newobj, quote(mxMatrix(type=typeDS,nrow=nrowDS,ncol=ncolDS,free=freeDS,values=valuesDS,labels=labelsDS,lbound=lboundDS,ubound=uboundDS,byrow=byrowDS,dimnames=dimnamesDS,name=nameDS)))
  
  # check that the new object has been created and display a message accordingly
  finalcheck <- isAssigned(datasources, newobj)
}
