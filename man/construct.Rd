\name{construct}
\alias{construct}
\title{Changes an expression to include only characters allowed by opal}
\usage{
construct(input)
}
\arguments{
  \item{input}{a string character}
}
\value{
an string character with the the 'OpemMx' symbols
}
\description{
This is an internal function.
}
\details{


For security reasons some symbols cannot pass the opal
parser and are hence should be replaced by string
characters.
}
\keyword{internal}

