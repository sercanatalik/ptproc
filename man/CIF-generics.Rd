\name{CIF-generics}
\alias{CIF-generics}
\alias{evalCIF}
\alias{integrateCIF}
\alias{evalCIF.default}
\alias{integrateCIF.default}

\title{Evaluate/Integrate the conditional intensity}
\description{
  The functions are used to access the evaluate and integration portions
  of the conditional intensity function of a point process model.
}
\usage{
\method{evalCIF}{default}(x, xpts = getPts(x), ...)
\method{integrateCIF}{default}(x, TT = ranges(x), ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{a point process object}
  \item{xpts}{a matrix of points at which to evaluate the conditional
    intensity}
  \item{TT}{a matrix of ranges over which to integrate the conditional
    intensity function}
  \item{\dots}{other arguments}
}
\details{
  By default, \code{evalCIF} evaluates the conditional intensity at the
  data points and \code{integrateCIF} attempts to integrate over the
  entire domain of the process.
}
\value{
  \code{evalCIF} returns a vector of numbers representing the
  conditional intensity evaluated at \code{xpts}.

  \code{integrateCIF} returns the integral of the conditional intensity
  function over the range specified in \code{TT}.
}
\author{Roger D. Peng}

\examples{
data(cal.quakes)
pp.model <- ptproc(pts = cal.quakes, cond.int = hawkes.cond.int,
                   params = c(mu = .07, C = 10, rep(0.001, 2)),
                   fixed = c(NA, 1, NA, NA))
condition(pp.model) <- penalty(NULL, quote(any(params < 0)))
fit <- ptproc.fit(pp.model, alpha = 1e+5)

x <- seq(0, 5*365, len = 1825)
e <- evalCIF(fit, xpts = x)
plot(x, e, type = "l", xlab = "Times",
     ylab = "Conditional Intensity (events/day)")

integrateCIF(fit)  ## Should be roughly == length(cal.quakes)
}

\keyword{models}
