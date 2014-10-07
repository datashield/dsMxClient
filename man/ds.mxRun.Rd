\name{ds.mxRun}
\alias{ds.mxRun}
\title{Performs the optimization of a model to estimate parameters}
\usage{
ds.mxRun(model, intervals = FALSE, silent = FALSE,
  suppressWarnings = FALSE, unsafe = FALSE, checkpoint = FALSE,
  useSocket = FALSE, onlyFrontend = FALSE, useOptimizer = TRUE,
  newobj = NULL, datasources = NULL)
}
\arguments{
  \item{model}{an mxModel object to be optimized.}

  \item{intervals}{a boolean indicating whether to compute
  the specified confidence intervals.}

  \item{silent}{a boolean indicating whether to print
  status to terminal.}

  \item{suppressWarnings}{a boolean indicating whether to
  suppress warnings.}

  \item{unsafe}{a boolean indicating whether to ignore
  errors.}

  \item{checkpoint}{a boolean indicating whether to
  periodically write parameter values to a file.}

  \item{useSocket}{a boolean indicating whether to
  periodically write parameter values to a socket.}

  \item{onlyFrontend}{a boolean indicating whether to run
  only front-end model transformations.}

  \item{useOptimizer}{a boolean indicating whether to run
  only the log-likelihood of the current free parameter
  values but not move any of the free parameters.}

  \item{newobj}{the name of the new object. By default the
  the name of the new object is "mxRun_output".}

  \item{datasources}{a list of opal object(s) obtained
  after login in to opal servers; these objects hold also
  the data assign to R, as \code{dataframe}, from opal
  datasources. By default an internal function looks for
  'opal' objects in the environment and sets this
  parameter.}
}
\value{
an mxModel object with free parameters updated to their
final values. The return value contains an "output" slot
with the results of optimization.
}
\description{
This function is similar to OpenMx function \code{mxRun}.
}
\details{


See details of the OpenMx function 'mxRun' in the package
\code{OpenMx}.
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

