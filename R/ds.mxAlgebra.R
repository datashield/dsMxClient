#' 
#' @title Creates a new MxAlgebra object
#' @description This function is similar to OpenMx function \code{mxAlgebra}. 
#' @details See details of the OpenMx function 'mxAlbegra
#' @param expression an R expression of OpenMx-supported matrix operators and matrix functions.
#' @param names an optional character string indicating the name of the object.
#' @param dimnames a list, the dimnames attribute for the algebra
#' @param newobj the name of the new variable. If this argument is set to NULL, the name of the new 
#' object is "mxAlgebra_result".
#' @param datasources a list of opal object(s) obtained after login in to opal servers;
#' these objects hold also the data assign to R, as \code{dataframe}, from opal datasources.
#' By default an internal function looks for 'opal' objects in the environment and sets this parameter. 
#' @return an object of type 'MxAlgebra' 
#' @author Gaye, A.
#' @export
#' @examples {
#'
#' }
#' 
ds.mxAlgebra = function(expression=NULL, name=NA, dimnames=NA, newObj=NULL, datasources=NULL){
  
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
  
  # create a name by default if user did not provide a name for the new variable
  if(is.null(newobj)){
    newobj <- "mxAlgebra_result"
  }
  
  # call the inernal function that generates the command for the server side function
  # because OpenMx does uses symbols from its 'omxSymbolTable' we need to deal with that
  cally <- getCall(expression, names, dimnames)
  
  # call the server side function that does the job
  datashield.assign(datasources, newobj, cally)
  
  # check that the new object has been created and display a message accordingly
  finalcheck <- isAssigned(datasources, newobj)

}
