\name{ds.mxFIMLObjective}
\alias{ds.mxFIMLObjective}
\title{Creates a new mxFIMLObjective object}
\usage{
ds.mxFIMLObjective(covariance = NULL, means = NULL, dimnames = NA,
  thresholds = NA, vector = FALSE, threshnames = dimnames,
  newobj = "new_mxFIMLObjective", datasources = NULL)
}
\arguments{
  \item{covariance}{a character string indicating the name
  of the expected covariance algebra.}

  \item{means}{a character string indicating the name of
  the expected means algebra.}

  \item{dimnames}{An optional character vector to be
  assigned to the dimnames of the covariance and means
  algebras.}

  \item{thresholds}{An optional character string indicating
  the name of the thresholds matrix.}

  \item{vector}{a logical value indicating whether the
  objective function result is the likelihood vector.}

  \item{threshnames}{An optional character vector to be
  assigned to the column names of the thresholds matrix.}

  \item{newobj}{the name of the new object. By default the
  the name of the new object is "new_mxFIMLObjective".}

  \item{datasources}{a list of opal object(s) obtained
  after login in to opal servers; these objects hold also
  the data assign to R, as \code{dataframe}, from opal
  datasources. By default an internal function looks for
  'opal' objects in the environment and sets this
  parameter.}
}
\value{
a new MxFIMLObjective object. MxFIMLObjective objects
should be included with models with referenced MxAlgebra,
MxData and MxMatrix objects
}
\description{
This function is similar to OpenMx function
\code{mxFIMLObjective}.
}
\details{


See details of the OpenMx function 'mxFIMLObjective' in the
package \code{OpenMx}.
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

