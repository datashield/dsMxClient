\name{preserveNum}
\alias{preserveNum}
\title{ensures values are kept in the right format}
\usage{
preserveNum(x)
}
\arguments{
  \item{input}{a matrix or a numeric vector}
}
\value{
an string character, an expression to be included in a call
expression
}
\description{
This is an internal function.
}
\details{


we do not want some argument given as matrix to be turned
into vector so we ensure the right format is passed on to
the server side function.
}
\keyword{internal}

