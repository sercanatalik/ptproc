\name{ptprocSim}
\alias{ptprocSim}
\alias{dim.ptprocSim}
\alias{getCIF.ptprocSim}
\alias{getData.ptprocSim}
\alias{getPts.ptprocSim}
\alias{print.ptprocSim}
\alias{ranges.ptprocSim}
\alias{params<-.ptprocSim}

\title{The ptprocSim class}

\description{
  The ptprocSim class and methods
}

\usage{
\method{dim}{ptprocSim}(x)
\method{getCIF}{ptprocSim}(object, ...)
\method{getData}{ptprocSim}(x, ...)
\method{print}{ptprocSim}(x, digits = getOption("digits") - 3, ...)
\method{ranges}{ptprocSim}(object, ...)
\method{params}{ptprocSim}(x) <- value
\method{getPts}{ptprocSim}(x, ...)
}

\arguments{
  \item{object, x}{an object of class \code{"ptprocSim"}}
  \item{digits}{number of digits for printing}
  \item{value}{a numeric vector}
  \item{...}{other arguments}
}

\details{
  A \code{"ptprocSim"} object can be created with the function
  \code{ptproc}.  It can subsequently be passed to \code{ptproc.sim} to
  obtain a simulated point process with the given conditional intensity.

  The class \code{"ptprocSim"} inherits from \code{"ptproc"}.
}


\seealso{\code{ptproc.sim}, \code{ptproc}}

\author{Roger D. Peng}

\keyword{models}
