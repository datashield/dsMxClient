#' 
#' @title Summarizes the results of an mx model optimization
#' @description This function is similar to base function 'summary'. 
#' @details to be written 
#' @param mxObj a character string, the name of an object obtained after optimizing a model via 'ds.mxRun'.
#' @param datasources a list of opal object(s) obtained after login in to opal servers;
#' these objects hold also the data assign to R, as \code{dataframe}, from opal datasources.
#' By default an internal function looks for 'opal' objects in the environment and sets this parameter. 
#' @return the summary of the output of an 'ds.mxRun' command.
#' @author Gaye, A.
#' @export
#' @examples {
#'
#' }
#' 
ds.mxSummary <- function(mxObj, datasources=NULL){
  
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

  # return the summary
  cally <- paste0("mxSummaryDS(", mxObj, ")")
  datashield.aggregate(datasources, as.symbol(cally))  
  
}
