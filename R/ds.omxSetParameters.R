#' 
#' @title Assign model parameters
#' @description This function is similar to OpenMx function \code{omxSetParameters}. 
#' @details See details of the OpenMx function 'omxSetParameters' in the package \code{OpenMx}.
#' @param model a MxModel object
#' @param labels a character vector of target parameter names
#' @param free a boolean vector of parameter free/fixed designations
#' @param values a numeric vector of parameter values
#' @param newlabels a character vector of new parameter names
#' @param lbound a numeric vector of lower bound values
#' @param ubound a numeric vector of upper bound values
#' @param indep  boolean. set parameters in independent submodels
#' @param strict boolean. if TRUE then throw an error when a label does not appear in the model
#' @param name character string. (optional) a new name for the model
#' @return a new modified mxModel
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
ds.omxSetParameters <- function(model, labels, free=NULL, values=NULL, newlabels=NULL, lbound=NULL, ubound=NULL, indep=FALSE, strict=TRUE, name=NULL, newobj='new_modified_mxModel', datasources=NULL) {
  
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
  if(!is.na(labels[1])){ labels <- paste0("c('", paste(labels, collapse="','"), "')")}
  if(!is.na(newlabels[1])){ newlabels <- paste0("c('", paste(newlabels, collapse="','"), "')")}
  values = preserveNum(values)
  lbound <- preserveNum(lbound)
  ubound <- preserveNum(ubound) 
#   cally <- paste0("omxSetParameters(model=", model, ", labels=", labels, ", free=", free, ", values=", values, ", newlabels=", newlabels, ", lbound=", lbound, ", ubound=", ubound, ", indep=", indep, ", strict=", strict, ", name='", name,  "')")
  cally <- call('omxSetParameters', model, labels, free, values, newlabels, lbound, ubound, indep, strict, name)
  datashield.assign(datasources, newobj, as.symbol(cally))
  
  return(output)
  
}
