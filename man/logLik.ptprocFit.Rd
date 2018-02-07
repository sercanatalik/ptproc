\name{logLik.ptprocFit}
\alias{logLik.ptprocFit}

\title{Return Log-Likelihood}
\description{
  Return the log-likelihood of a point process model.
}
\usage{
\method{logLik}{ptprocFit}(object, negative = FALSE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{an object of class \code{"ptprocFit"}}
  \item{negative}{Should the negative of the log-likelihood be returned?}
  \item{\dots}{other arguments}
}
\details{
  The \code{logLik} methods allow for the \code{AIC} function to be used
  on point process objects.
}
\value{
  An object of class \code{"logLik"} is returned containing the value of
  the log-likelihood and the degrees of freedom of the model.
}

\seealso{\code{AIC}}

\author{Roger D. Peng}

\examples{
data(cal.quakes)
ppm <- ptproc(cal.quakes, cond.int = hawkes.cond.int,
              params = c(.07, 10, .1),
              condition = penalty(NULL, quote(any(params < 0))))
fit <- ptproc.fit(ppm, alpha = 1e+5)
logLik(fit)
AIC(fit)
}
\keyword{models}
