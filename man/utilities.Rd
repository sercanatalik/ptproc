\name{onedim-diag}
\alias{onedim-diag}
\alias{logSurv}
\alias{stationarity}

\title{
  One-dimensional Point Process Diagnostics
}

\description{
  Diagonstic plots for temporal point processes.
}

\usage{
logSurv(res, theoretical = TRUE, xlab = "Interevent time",
        ylab = "Cumulative number", ...)
stationarity(res, h, std = TRUE, xlab = "Transformed time",
             ylab="Standardized # of events per interval",
             type = "b", ...)
}

\arguments{
  \item{res}{A vector containing the one-dimensional residual process.}
  \item{theoretical}{Should the theoretical survivor curve be plotted?}
  \item{xlab}{Label for the x-axis.}
  \item{ylab}{Label for the y-axis.}
  \item{type}{The type of plot to be made.}
  \item{...}{Other arguments passed to \code{plot}.}
  \item{h}{The size of the bins for counting events.}
  \item{std}{Should standard deviation lines be plotted?}
}

\details{
  \code{logSurv} plots the log-survivor plot of the interevent times of
  the residual process in \code{res}.  If the residual process is
  homogeneous Poisson, then the log-survivor function should appear as a
  straight line.

  The \code{stationarity} function divides the domain into
  bins of a given length (specified in \code{h}) and counts the number
  of points falling into each bin is counted.  The number of points in each
  bin is standardized by the theoretical mean and standard deviation
  (assuming a homogeneous Poisson process) and
  the standardized counts are plotted against the left endpoints of the
  bins.
}


\author{Roger D. Peng}

\keyword{models}


\examples{
data(fires)
times <- fires[,1]
ppm <- ptproc(pts = times, cond.int = hawkes.cond.int,
              params = c(mu = .004, C = .004, a = rep(0,1)))
condition(ppm) <- penalty(code = NULL, condition = quote(any(params < 0)))
fit <- ptproc.fit(ppm, list(trace = 2, maxit = 100), alpha = 1e+5)

## Generate ordinary thinned residuals
r <- residuals(fit, "ord", m = params(fit)[1])

logSurv(r)
stationarity(r, 200)
}
