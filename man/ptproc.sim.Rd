\name{ptproc.sim}
\alias{ptproc.sim}

\title{
  Simulate a point process
}

\description{
  Simulate a point process, given a conditional intensity model.
}

\usage{
ptproc.sim(ppobj, M)
}

\arguments{
  \item{ppobj}{An object of class \code{"ptproc"}  containing the conditional
    intensity model to be simulated.}
  \item{M}{The maximum of the given conditional intensity over the
    range.}
}


\details{
  The value of \code{M} should be such that M >= lambda(x) for all x in
  the domain (where lambda(x) is the conditional intensity function).
}

\value{
  The simulated point process points.
}


\examples{
## Simulate from a linear model
ppsim <- ptproc(pts = NULL, cond.int = linear.cond.int,
                params = c(100, 10, -50, 50),
                ranges = cbind(c(0,2),c(0,2),c(0,2)),
                is.pois = TRUE)
sim <- ptproc.sim(ppsim, M = 330)
colnames(sim) <- c("Time", "X", "Y")
pairs(sim, pch = ".")

## Simulate from Hawkes model

psim <- ptproc(pts = NULL,cond.int = hawkes.cond.int,
               params = c(mu = .07, C = 10, a=2.7),
               fixed = c(1, 1, 1),
               ranges = array(c(0,1000),c(2,1)), data = NULL, 
               is.pois = FALSE) 
set.seed(20)
sim <- ptproc.sim(psim, M = 10)
hist(sim)
rug(sim)

## Simulate from a fitted model

data(fires)
ppm <- ptproc(fires, cond.int = linear.cond.int,
              params = c(.004, .001, 0, 0))
extra.code <- expression(ranges <- as.list(as.data.frame(ranges(ppobj))),
                         corners <- expand.grid(ranges),
                         ci <- evalCIF(ppobj, xpts = corners))
condition(ppm) <- penalty(code = extra.code,
                          condition = quote(any(ci < 0)))
fit <- ptproc.fit(ppm, optim.control = list(trace = 2), alpha = 1e+5)

set.seed(10000)
sim <- ptproc.sim(fit, M = .1)

}


\author{Roger D. Peng}
\keyword{models}
