\name{params.default}
\alias{params.default}
\title{Retrieve parameters of a point process model}
\description{
  Extract the parameters from a point process object
}
\usage{
\method{params}{default}(x, ...)
}

\arguments{
  \item{x}{a point process object}
  \item{\dots}{other arguments}
}
\details{
  This function retrieves the parameter vector from the given point
  process object.  For objects of class \code{"ptprocFit"}, this is the
  vector of fitted parameters.
}
\value{
  A vector of parameters
}

\author{Roger D. Peng}

\keyword{models}
