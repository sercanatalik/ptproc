\name{ptprocFit}

\alias{ptprocFit}
\alias{dim.ptprocFit}
\alias{extract.ptprocInit.ptprocFit}
\alias{getPts.ptprocFit}
\alias{getData.ptprocFit}
\alias{ranges.ptprocFit}
\alias{getCIF.ptprocFit}
\alias{fixed.ptprocFit}
\alias{condition.ptprocFit}
\alias{as.ptprocInit.ptprocFit}
\alias{as.ptprocSim.ptprocFit}
\alias{print.ptprocFit}

\title{The ptprocFit class}

\description{
  Methods for the ptprocFit class
}

\usage{
\method{dim}{ptprocFit}(x)
\method{extract.ptprocInit}{ptprocFit}(x, ...)
\method{getPts}{ptprocFit}(x, ...)
\method{getData}{ptprocFit}(x, ...)
\method{ranges}{ptprocFit}(object, ...)
\method{getCIF}{ptprocFit}(object, ...)
\method{fixed}{ptprocFit}(x, ...)
\method{condition}{ptprocFit}(object, ...)
\method{as.ptprocInit}{ptprocFit}(x, ...)
\method{as.ptprocSim}{ptprocFit}(x, ...)
\method{print}{ptprocFit}(x, digits = getOption("digits") - 3, ...)
}

\arguments{
  \item{object, x}{an object of class \code{"ptprocFit"}}
  \item{digits}{number of digits for printing}
  \item{\dots}{other arguments}
}

\details{
  These functions extract/set elements of a \code{"ptprocFit"} object.
  An object of class \code{"ptprocFit"} is returned by the
  \code{ptproc.fit} function.

  The print method prints a short summary of the object, including
  the fitted parameters and the initial parameter values for the
  optimizer.

  \code{dim} returns the number of dimensions of the space in which the
  point process resides.

  Note that parameters (and fixed elements) cannot be modified for
  a \code{"ptprocFit"} object.

  The class \code{"ptprocFit"} inherits from \code{"ptproc"}.
}

\seealso{\code{ptproc.fit}}

\keyword{models}

\author{Roger D. Peng}
