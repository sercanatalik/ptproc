\name{extractors}
\alias{fixed}
\alias{fixed<-}
\alias{params}
\alias{params<-}
\alias{extract.ptprocInit}
\alias{getPts}
\alias{getData}
\alias{ranges}
\alias{getCIF}
\alias{setCIF<-}
\alias{condition}
\alias{condition<-}

\title{Extractor generics for ptproc package}
\description{
  Generics for extracting/setting elements from ptproc objects
}
\usage{
fixed(x, ...)
fixed(x) <- value
params(x, ...)
params(x) <- value
getPts(x, ...)
getData(x, ...)
ranges(object, ...)
getCIF(object, ...)
setCIF(x) <- value
condition(object, ...)
condition(x) <- value
extract.ptprocInit(x, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object,x}{a point process object; i.e. an object of class
    \code{"ptproc"}, \code{"ptprocInit"}, \code{"ptprocFit"}, or
    \code{"ptprocSim"}}
  \item{value}{a numeric vector, or in the case of the assignment
    version of \code{condition}, an R expression}
  \item{\dots}{other arguments}
}

\details{
  The assignment form of \code{condition} can be used in conjunction
  with \code{penalty} to penalize the log-likelihood of a point process
  model.
}

\value{
  Thes functions extract/set the corresponding element of the particular
  object on which they operate.

  \code{extract.ptprocInit} returns the initial point process object
  from a \code{"ptprocFit"} object.
}

\author{Roger D. Peng}

\keyword{models}
