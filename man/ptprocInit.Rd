\name{ptprocInit}
\alias{ptprocInit}
\alias{dim.ptprocInit}
\alias{getPts.ptprocInit}
\alias{getData.ptprocInit}
\alias{ranges.ptprocInit}
\alias{getCIF.ptprocInit}
\alias{setCIF<-.ptprocInit}
\alias{fixed.ptprocInit}
\alias{condition.ptprocInit}
\alias{as.ptprocSim.ptprocInit}
\alias{params<-.ptprocInit}
\alias{fixed<-.ptprocInit}
\alias{condition<-.ptprocInit}
\alias{print.ptprocInit}

\title{The ptprocInit class}

\description{
  Methods for the ptprocInit class
}

\usage{
\method{dim}{ptprocInit}(x)
\method{getPts}{ptprocInit}(x, ...)
\method{getData}{ptprocInit}(x, ...)
\method{ranges}{ptprocInit}(object, ...)
\method{getCIF}{ptprocInit}(object, ...)
\method{setCIF}{ptprocInit}(x) <- value
\method{fixed}{ptprocInit}(x, ...)
\method{fixed}{ptprocInit}(x) <- value
\method{condition}{ptprocInit}(object, ...)
\method{condition}{ptprocInit}(x) <- value
\method{as.ptprocSim}{ptprocInit}(x, ...)
\method{params}{ptprocInit}(x) <- value
\method{print}{ptprocInit}(x, digits = getOption("digits") - 3, ...)
}

\arguments{
  \item{object, x}{an object of class \code{"ptprocInit"}}
  \item{digits}{number of digits for printing}
  \item{value}{For \code{setCIF}, a function; otherwise, a numeric
    vector}
  \item{\dots}{other arguments}
}


\details{
  The \code{"ptprocInit"} object is used for fitting a point process
  conditional intensity models.  It will typically be passed to the
  function \code{ptproc.fit} where the parameters in the
  \code{"ptprocInit"} will be used as starting values for minimizing the
  negative log-likelihood.  An object of class \code{"ptprocInit"}
  can be created using the \code{ptproc} function.

  The \code{print} method prints a short summary of the object,
  including the parameter values and fixed parameters.

  \code{dim} returns the number of dimensions of the space in which the
  point process resides.

  The class \code{"ptprocInit"} inherits from \code{"ptproc"}.
}

\seealso{\code{ptproc}}

\author{Roger D. Peng}
\keyword{models}
