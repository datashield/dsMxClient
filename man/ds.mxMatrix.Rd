\name{ds.mxMatrix}
\alias{ds.mxMatrix}
\title{Creates a new MxMatrix object}
\usage{
ds.mxMatrix(type = "Full", nrow = NA, ncol = NA, free = FALSE,
  values = NA, labels = NA, lbound = NA, ubound = NA, byrow = FALSE,
  dimnames = NA, name = NA, newobj = "new_mxMatrix", datasources = NULL)
}
\arguments{
  \item{type}{a character string indicating the matrix
  type, where type indicates the range of values and #'
  equalities in the matrix. Must be one of: 'Diag', 'Full',
  'Iden', 'Lower', 'Sdiag', 'Stand', 'Symm', 'Unit', or
  'Zero'.}

  \item{nrow}{the desired number of rows. One or both of
  'nrow' and 'ncol' is required when 'values', 'free',
  'labels', 'lbound', and 'ubound' arguments are not
  matrices, depending on the matrix type.}

  \item{ncol}{the desired number of columns. One or both of
  'nrow' and 'ncol' is required when 'values', 'free',
  'labels', 'lbound', and 'ubound' arguments are not
  matrices, depending on the matrix type.}

  \item{free}{a vector or matrix of logicals for free
  parameter specification. A single 'TRUE' or 'FALSE' will
  set all allowable variables to free or fixed,
  respectively.}

  \item{values}{a vector or matrix of numeric starting
  values. By default, all values are set to zero.}

  \item{labels}{a vector or matrix of characters for
  variable label specification.}

  \item{lbound}{a vector or matrix of numeric lower bounds.
  Default bounds are specified with an NA.}

  \item{ubound}{a vector or matrix of numeric upper bounds.
  Default bounds are specified with an NA.}

  \item{byrow}{logical. If 'FALSE' (default), the 'values',
  'free', 'labels', 'lbound', and 'ubound' matrices are
  populated by column rather than by row.}

  \item{dimnames}{list. The dimnames attribute for the
  matrix: a list of length 2 giving the row and column
  names respectively. An empty list is treated as NULL, and
  a list of length one as row names. The list can be named,
  and the list names will be used as names for the
  dimensions.}

  \item{name}{an optional character string indicating the
  name of the MxMatrix object}

  \item{newobj}{the name of the new object. By default the
  name of the new object is "new_mxMatrix".}

  \item{datasources}{a list of opal object(s) obtained
  after login in to opal servers; these objects hold also
  the data assign to R, as \code{dataframe}, from opal
  datasources. By default an internal function looks for
  'opal' objects in the environment and sets this
  parameter.}
}
\value{
a new mxMatrix object, which consists of a 'values' matrix
of numeric starting values, a 'free' matrix describing free
parameter specification, a 'labels' matrix of labels for
the variable names, and 'lbound' and 'ubound' matrices of
the lower and upper parameter bounds.
}
\description{
This function is similar to OpenMx function
\code{mxMatrix}.
}
\details{


See details of the OpenMx function 'mxMatrix' in the
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

