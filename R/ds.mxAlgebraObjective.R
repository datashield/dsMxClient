#' 
#' @title Creates a new mxAlgebraObjective object
#' @description This function is similar to OpenMx function \code{mxAlgebraObjective}. 
#' @details See details of the OpenMx function 'mxAlgebraObjective' in the package \code{OpenMx}.
#' @param algebra a character string indicating the name of an mxAlgebra or mxMatrix object to use 
#' for optimization.
#' @param numObs (optional) an adjustment to the total number of observations in the model.
#' @param numStats (optional) An adjustment to the total number of observed statistics in the model.
#' @param newobj the name of the new object. By default the the name of the new object is "new_mxAlgebraObjective".
#' @param datasources a list of opal object(s) obtained after login in to opal servers;
#' these objects hold also the data assign to R, as \code{dataframe}, from opal datasources.
#' By default an internal function looks for 'opal' objects in the environment and sets this parameter. 
#' @return a new mxAlgebraObjective object. MxAlgebraObjective objects should be included with models 
#' with referenced mxAlgebra and mxMatrix objects.
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
ds.mxAlgebraObjective <- function(algebra=NULL, numObs=NA, numStats=NA, newobj=NULL, datasources=NULL){
  
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
  
  # Throw an error message if the required argument is not set
  if(is.null(algebra)){
    stop(" Please provie a character string indicating the name of an mxAlgebra or mxMatrix object to use for optimization! ", call.=FALSE)
  }  
  
  # create a name by default if user did not provide a name for the new object
  if(is.null(newobj)){
    newobj <- "new_mxAlgebraObjective"
  }
  
  # call the server side function that does the job
  cally <- call("mxAlgebraObjective", algebra, numObs, numStats)
  datashield.assign(datasources, newobj, cally)
  
  # check that the new object has been created and display a message accordingly
  finalcheck <- isAssigned(datasources, newobj)
  
}
