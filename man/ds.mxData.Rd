\name{ds.mxData}
\alias{ds.mxData}
\title{Creates a new MxData object}
\usage{
ds.mxData(observed = NULL, type = NULL, means = NA, numObs = NA,
  newobj = "new_mxData", datasources = NULL)
}
\arguments{
  \item{observed}{a string, the name of a matrix or
  data.frame which provides data to the MxData object.}

  \item{type}{A character string defining the type of data
  in the 'observed' argument. Must be one of "raw", "cov",
  "cor", or "sscp".}

  \item{means}{An optional vector of means for use when
  'type' is "cov", or "cor".}

  \item{numObs}{The number of observations in the data
  supplied in the 'observed' argument. Required unless
  'type' equals "raw".}

  \item{newobj}{the name of the new variable. By default
  the name of the new object is "new_mxData".}

  \item{datasources}{a list of opal object(s) obtained
  after login in to opal servers; these objects hold also
  the data assign to R, as \code{dataframe}, from opal
  datasources. By default an internal function looks for
  'opal' objects in the environment and sets this
  parameter.}
}
\value{
a new mxData object.
}
\description{
This function is similar to OpenMx function \code{mxData}.
}
\details{


The same output as the one returned by 'mxData' in the
package \code{OpenMx} is generated but an empty (all values
= NA) dataframe is returned if the output dataframe is not
valid according to the DataSHIELD criteria (i.e. if the
dataframe has less than the allowed number of
observations/rows. If the output dataframe has 0
observations (number of rows = 0), a data fram with one
empty row is returned.
}
\examples{
{

}
}
\author{
Gaye, A.
}
\references{
Steven M. Boker, Michael C. Neale, Hermine H. Maes, Michael
J. Wilde, Michael Spiegel, Timothy R. Brick, Jeffrey Spies,
Ryne Estabrook, Sarah Kenny, Timothy C., Bates, Paras
Mehta, and John Fox. (2011) OpenMx: An Open Source Extended
Structural Equation Modeling Framework. Psychometrika.
Steven M. Boker, Michael C. Neale, Hermine H. Maes, Michael
J. Wilde, Michael Spiegel, Timothy R. Brick, Ryne
Estabrook, Timothy C. Bates, Paras Mehta, Timo von Oertzen,
Ross J. Gore, Michael D. Hunter, Daniel C. Hackett, Julian
Karch and Andreas M. Brandmaier. (2012) OpenMx 1.3 User
Guide.
}

