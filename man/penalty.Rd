\name{penalty}
\alias{penalty}


\title{
  Penalties for point process models
}

\description{
  Construct a penalty to a point process object.
}

\usage{
penalty(code = NULL, condition)
}

\arguments{
  \item{code}{An expression object containing one or many valid R
    expressions that can be evaluated in an environment containing the
    point process object.}
  \item{condition}{A test to be performed to determine whether or not a
    penalty is incurred (see Details).}
}


\details{
  \code{condition} should be an unevaluated R expression (i.e.
  by using \code{quote})
  representing a "test" which will determine whether or not a penalty
  will be returned before evaluation of the log-likelihood.  The penalty
  will be returned if the test evaluates to \code{TRUE}.  The value of
  the penalty is specified in the \code{alpha} parameter to
  \code{ptproc.fit}.

  The \code{code} argument can be used for passing extra code into the
  version of the log-likelihood function passed to the optimizer.
  One can access the full \code{"ptprocInit"} object via the name `ppobj'.
}

\seealso{\code{condition}}

\examples{
data(cal.quakes)

## Construct a K = 3 Hawkes cluster model
ppm <- ptproc(cal.quakes, cond.int = hawkes.cond.int,
              params = c(.07, 10, rep(1e-4, 3)))

## Restrict parameters to be positive
condition(ppm) <- penalty(code = NULL, condition = quote(any(params < 0)))

data(fires)

## Use simple linear model
ppm <- ptproc(fires, cond.int = linear.cond.int,
              params = c(.004, .001, 0, 0))

## Evaluate at the "corners" of the domain
extra.code <- expression(ranges <- as.list(as.data.frame(ranges(ppobj))),
                         corners <- expand.grid(ranges),
                         ci <- evalCIF(ppobj, xpts = corners))

## Add penalty
condition(ppm) <- penalty(code = extra.code,
                          condition = quote(any(ci < 0)))

## Fit model with penalty
fit <- ptproc.fit(ppm, optim.control = list(trace = 2), alpha = 1e+5)

}

\author{Roger D. Peng}

\keyword{models}
