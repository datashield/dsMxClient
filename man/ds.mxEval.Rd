\name{ds.mxEval}
\alias{ds.mxEval}
\title{Evaluates values in an mxModel}
\usage{
ds.mxEval(expression = NULL, model = NULL, compute = FALSE,
  show = FALSE, defvar.row = 1, cacheBack = FALSE, datasources = NULL)
}
\arguments{
  \item{expression}{a string character an arbitrary R
  expression.}

  \item{model}{a string character, the name of the model in
  which to evaluate the expression.}

  \item{compute}{a boolean, if TRUE then compute the value
  of algebra expressions.}

  \item{show}{if TRUE then print the translated
  expression.}

  \item{defvar.row}{an integer, the row number for
  definition variables when compute=TRUE.}

  \item{cacheBack}{a boolean, if TRUE then return the list
  pair (value, cache).}

  \item{datasources}{a list of opal object(s) obtained
  after login in to opal servers; these objects hold also
  the data assign to R, as \code{dataframe}, from opal
  datasources. By default an internal function looks for
  'opal' objects in the environment and sets this
  parameter.}
}
\value{
the evaluation of an expression.
}
\description{
This function is similar to OpenMx function \code{mxEval}.
}
\details{


See details of the OpenMx function 'mxModel' in the package
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

