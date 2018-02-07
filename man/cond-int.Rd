\name{cond.int}
\alias{cond.int}
\alias{linear.cond.int}
\alias{hawkes.cond.int}
\alias{hPois.cond.int}

\title{
  Conditional intensity functions
}

\description{
  Example point process conditional intensity functions.
}

\usage{
linear.cond.int(params, eval.pts, pts = NULL, data = NULL, TT = NULL)
hawkes.cond.int(params, eval.pts, pts = NULL, data = NULL, TT = NULL)
hPois.cond.int(params, eval.pts, pts = NULL, data = NULL, TT = NULL)
}

\arguments{
  \item{params}{A vector of parameters for a point process model.}
  \item{eval.pts}{A matrix of points at which the conditional intensity
    should be evaluated.}
  \item{pts}{A matrix containing original data.}
  \item{data}{Other information that may be needed to evaluate or
    integrate the conditional intensity.}
  \item{TT}{A matrix containing the ranges of integration for each
    dimension.}
}


\details{
  \code{linear.cond.int} implements a simple space-time linear model
  with a background parameter mu and a parameter for the time and
  spatial dimensions.

  \code{hawkes.cond.int} implements a one-dimensional Hawkes-type
  cluster model with a Laguerre-type polynomial as the trigger
  function.  See the reference for more details on this model.

  \code{hPois.cond.int} is a simple homogeneous Poisson model.  It is
  unlikely to be useful but is included as yet another example.

  The design of the package is influenced heavily by the Statistical
  Seismology Library (SSLib) of Harte (1998).  In particular the
  template for the conditional intensity function is similar to that of
  SSLib.  However, SSLib is particularly focused on applications in
  seismology.  Those who are interested in
  seismological applications are encouraged to look at SSLib because of
  its many other useful functions related to earthquake analysis.   
}

\references{
  Ogata, Y. (1988) Statistical models for earthquake occurrences and
  residual analysis for point processes. \emph{JASA}, \bold{83}, 9--27.

  Harte, D. (1998) Documentation for the Statistical Seismology
  Library.  Tech. Rep. 98-10, School of Mathematical and Computing
  Sciences, Victoria University of Wellington.
}
   

\examples{
## See ?ptproc for examples
}



\author{Roger D. Peng}

\keyword{models}
