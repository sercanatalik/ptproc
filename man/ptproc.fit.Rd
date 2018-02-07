\name{ptproc.fit}
\alias{ptproc.fit}


\title{
  Fit Point Process Models
}

\description{
  Fit point process models via maximum likelihood.
}

\usage{
ptproc.fit(ppobj, optim.control = list(), method = "Nelder-Mead", 
           alpha = 0, ...) 
}

\arguments{
  \item{ppobj}{An object of class \code{"ptprocInit"} containing the
    conditional intensity model to be fit.}
  \item{optim.control}{An optional named list of tuning parameters to be
    passed to \code{optim}.}
  \item{method}{The optimization method to be used.}
  \item{alpha}{A value for the penalty, if one is needed.}
  \item{...}{Other arguments to be passed to \code{optim}.}
}

\details{
  \code{ptproc.fit} calls the R function \code{optim} to minimize the
  negative log-likelihood.  For minimization, \code{ptproc.fit} uses the
  default method of \code{optim}, which is the Nelder-Mead algorithm.
  This method produces reasonable results for a variety of different
  models.

  Scaling information for the parameters can be passed via the
  \code{parscale} tuning parameter of the \code{optim.control} list.
  This can be very useful for guiding the optimizer to a good solution.

  The value chosen for \code{alpha} will depend on the model and the
  nature of the data.  Generally, one should choose \code{alpha} to be a
  1 or 2 orders of magnitude greater than the typical log-likelihood value.
}

\value{
  An object of class \code{"ptprocFit"}, which contains
  \item{ptprocInit}{the original \code{"ptprocInit"} object}
  \item{params}{the fitted parameters}
  \item{hessian}{the hessian matrix (if requested), or \code{NULL}}
  \item{convergence}{the convergence code returned by \code{optim}}
}

\seealso{\code{optim}, \code{penalty}}

\examples{
data(cal.quakes)
ppm <- ptproc(cal.quakes, cond.int = hawkes.cond.int,
              params = c(.07, 10, .1),
              condition = penalty(NULL, quote(any(params < 0))))
fit <- ptproc.fit(ppm, optim.control = list(trace = 2), alpha = 1e+5)

summary(fit)

## Try a slightly different model
params(ppm) <- c(0.07, 10, rep(0.001, 2))
fixed(ppm) <- rep(NA, 4)
fit2 <- ptproc.fit(ppm, optim.control = list(trace = 2), alpha = 1e+5)

## Compare the two fits
AIC(fit, fit2)

}


\author{Roger D. Peng}

\keyword{optimize}
