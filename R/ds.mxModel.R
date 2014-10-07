#' 
#' @title Creates a new mxModel object
#' @description This function is similar to OpenMx function \code{mxModel}. 
#' @details See details of the OpenMx function 'mxModel' in the package \code{OpenMx}.
#' @param model this argument is either an mxModel object or a string. If 'model' is an 
#' mxModel object, then all elements of that model are placed in the resulting mxModel object. 
#' If 'model' is a string, then a new model is created with the string as its name. If 'model' 
#' is either unspecified or 'model' is a named entity, data source, or MxPath object, then a 
#' new model is created.
#' @param lst a list of character strings, the names of an arbitrary number of mxMatrix, mxPath, 
#' mxData, and other functions such as mxConstraints and mxCI. These will all be added or removed 
#' from the model as specified in the 'model' argument, based on the 'remove' argument.
#' @param manifestVars for RAM-type models, a list of manifest variables to be included in the model.
#' @param latentVars for RAM-type models, A list of latent variables to be included in the model.
#' @param remove logical. If TRUE, elements listed in this statement are removed from the original 
#' model. If FALSE, elements listed in this statement are added to the original model.
#' @param independent	logical. If TRUE then the model is evaluated independently of other models.
#' @param type character vector. The model type to assign to this model. Defaults to options 
#' ("mxDefaultType"). See below for valid types
#' @param name an optional character vector indicating the name of the object.
#' @param newobj the name of the new object.  By default the same as the string provided for the 
#' argument 'name' and if 'name' is set to NA then the name of the new object is "new_mxModel".
#' @param datasources a list of opal object(s) obtained after login in to opal servers;
#' these objects hold also the data assign to R, as \code{dataframe}, from opal datasources.
#' By default an internal function looks for 'opal' objects in the environment and sets this parameter. 
#' @return a new mxModel object. MxModel objects must include an objective function to be used as 
#' arguments in mxRun functions.
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
ds.mxModel <- function(model=NA, lst, manifestVars=NA, latentVars=NA, remove=FALSE, independent=NA, type=NA, name=NA, newobj=name, datasources=NULL){
  
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
  
  # create a name by default if user did not provide a name for the new object
  if(is.na(newobj)){
    newobj <- "new_mxModel"
  }
  
  # call the server side function that does the job
  cally <- call('mxModelDS', model, lst, manifestVars, latentVars, remove, independent, type, name)
  datashield.assign(datasources, newobj, cally)
  
  # check that the new object has been created and display a message accordingly
  finalcheck <- isAssigned(datasources, newobj)
  
}
