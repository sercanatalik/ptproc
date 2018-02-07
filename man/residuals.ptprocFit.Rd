\name{residuals.ptprocFit}
\alias{residuals.ptprocFit}

\title{Point process residuals}
\description{
  Compute and return a residual process given a fitted conditional
  intensity model
}
\usage{
\method{residuals}{ptprocFit}(object, type = c("ordinary", "approx"),
                              m = NULL, K = NULL, R = 1, ...)
}

\arguments{
  \item{object}{an object of class \code{"ptprocFIt"}}
  \item{type}{type of residual process to compute}
  \item{m}{minimum of conditional intensity}
  \item{K}{subsample size}
  \item{R}{number of random realizations to return}
  \item{\dots}{other arguments}
}
\details{
  This function can compute residual process via ordinary random
  thinning or approximate random thinning.  For ordinary random
  thinning, the minimum of the conditional intensity \code{m} must be
  provided.  For approximate random thinning the subsample size \code{K}
  must be provided.
}
\value{
  If \code{R} > 1 a list of length \code{R} is returned where each
  element of the list is a matrix of points representing a residual process.
  Otherwise, a matrix of points is returned.  For approximate random
  thinning the matrix has exactly \code{K} rows.
}

\author{Roger D. Peng}

\examples{
data(cal.quakes)
ppm <- ptproc(cal.quakes, cond.int = hawkes.cond.int,
              params = c(mu = .07, C = 1, a = 0.1),
              condition = penalty(NULL, quote(any(params < 0))))
fit <- ptproc.fit(ppm, optim.control = list(trace = 3), alpha = 1e+5)
res <- residuals(fit, m = params(fit)[1])
hist(res, nclass = 50)

res2 <- residuals(fit, type = "approx", K = 100, R = 10)
}
\keyword{models}% at least one, from doc/KEYWORDS
