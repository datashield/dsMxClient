\name{ds.mxAlgebra}
\alias{ds.mxAlgebra}
\title{Creates a new MxAlgebra object}
\usage{
ds.mxAlgebra(expression = NULL, name = NA, dimnames = NA,
  newobj = "mxAlgebra_output", datasources = NULL)
}
\arguments{
  \item{expression}{an R expression of OpenMx-supported
  matrix operators and matrix functions.}

  \item{name}{an optional character string indicating the
  name of the object.}

  \item{dimnames}{a list of character strings, the dimnames
  attribute for the algebra}

  \item{newobj}{the name of the new variable. If this
  argument is set to NULL, the name of the new object is
  "mxAlgebra_output".}

  \item{datasources}{a list of opal object(s) obtained
  after login in to opal servers; these objects hold also
  the data assign to R, as \code{dataframe}, from opal
  datasources. By default an internal function looks for
  'opal' objects in the environment and sets this
  parameter.}
}
\value{
an object of type 'mxAlgebra'
}
\description{
This function is similar to OpenMx function
\code{mxAlgebra}.
}
\details{


See details of the OpenMx function 'mxAlbegra
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

