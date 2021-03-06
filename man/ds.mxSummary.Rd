\name{ds.mxSummary}
\alias{ds.mxSummary}
\title{Summarizes the results of an mx model optimization}
\usage{
ds.mxSummary(mxObj, datasources = NULL)
}
\arguments{
  \item{mxObj}{a character string, the name of an object
  obtained after optimizing a model via 'ds.mxRun'.}

  \item{datasources}{a list of opal object(s) obtained
  after login in to opal servers; these objects hold also
  the data assign to R, as \code{dataframe}, from opal
  datasources. By default an internal function looks for
  'opal' objects in the environment and sets this
  parameter.}
}
\value{
the summary of the output of an 'ds.mxRun' command.
}
\description{
This function is similar to base function 'summary'.
}
\details{


to be written
}
\examples{
{

}
}
\author{
Gaye, A.
}

