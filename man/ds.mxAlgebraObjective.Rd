\name{ds.mxAlgebraObjective}
\alias{ds.mxAlgebraObjective}
\title{Creates a new mxAlgebraObjective object}
\usage{
ds.mxAlgebraObjective(algebra = NULL, numObs = NA, numStats = NA,
  newobj = NULL, datasources = NULL)
}
\arguments{
  \item{algebra}{a character string indicating the name of
  an mxAlgebra or mxMatrix object to use for optimization.}

  \item{numObs}{(optional) an adjustment to the total
  number of observations in the model.}

  \item{numStats}{(optional) An adjustment to the total
  number of observed statistics in the model.}

  \item{newobj}{the name of the new object. By default the
  the name of the new object is "new_mxAlgebraObjective".}

  \item{datasources}{a list of opal object(s) obtained
  after login in to opal servers; these objects hold also
  the data assign to R, as \code{dataframe}, from opal
  datasources. By default an internal function looks for
  'opal' objects in the environment and sets this
  parameter.}
}
\value{
a new mxAlgebraObjective object. MxAlgebraObjective objects
should be included with models with referenced mxAlgebra
and mxMatrix objects.
}
\description{
This function is similar to OpenMx function
\code{mxAlgebraObjective}.
}
\details{


See details of the OpenMx function 'mxAlgebraObjective' in
the package \code{OpenMx}.
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

