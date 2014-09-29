\name{ds.mxAlgebra}
\alias{ds.mxAlgebra}
\title{Creates a new MxAlgebra object}
\usage{
ds.mxAlgebra(expression = NULL, name = NA, dimnames = NA, newObj = NULL,
  datasources = NULL)
}
\arguments{
  \item{expression}{an R expression of OpenMx-supported
  matrix operators and matrix functions.}

  \item{names}{an optional character string indicating the
  name of the object.}

  \item{dimnames}{a list, the dimnames attribute for the
  algebra}

  \item{newobj}{the name of the new variable. If this
  argument is set to NULL, the name of the new object is
  "mxAlgebra_result".}

  \item{datasources}{a list of opal object(s) obtained
  after login in to opal servers; these objects hold also
  the data assign to R, as \code{dataframe}, from opal
  datasources. By default an internal function looks for
  'opal' objects in the environment and sets this
  parameter.}
}
\value{
an object of type 'MxAlgebra'
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

