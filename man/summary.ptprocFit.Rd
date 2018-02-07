\name{summary.ptprocFit}
\alias{summary.ptprocFit}
\alias{print.summary.ptprocFit}

\title{Summary for ptprocFit}
\description{
  Summary for fitted point process model objects
}
\usage{
\method{summary}{ptprocFit}(object, ...)
\method{print}{summary.ptprocFit}(x, digits = getOption("digits") - 3, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{an object of class \code{"ptprocFit"}}
  \item{x}{an object of class \code{"summary.ptprocFit"}}
  \item{digits}{number of digits for printing}
  \item{\dots}{other arguments}
}
\details{
  Currently, the only useful information computed by the summary
  information is the AIC of the fitted model and the AIC of the
  homogeneous Poisson model.  The \code{print} method also prints the
  fitted parameters.
}
\value{
  An object of class \code{"summary.ptprocFit"}; i.e. a list containing:
  \item{poisson.AIC}{The AIC of a homogeneous Poisson model fitted to
    the data}
  \item{model.AIC}{AIC of the fitted conditional intensity model}
  \item{ppobj}{The \code{"ptprocInit"} originally passed to
    \code{ptproc.fit}}
}

\author{Roger D. Peng}

\examples{
data(cal.quakes)
ppm <- ptproc(cal.quakes, cond.int = hawkes.cond.int,
              params = c(.07, 10, .1),
              condition = penalty(NULL, quote(any(params < 0))))
fit <- ptproc.fit(ppm, alpha = 1e+5)

summary(fit)
}
\keyword{models}% at least one, from doc/KEYWORDS
