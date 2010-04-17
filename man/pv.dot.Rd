\name{pv.dot}
\alias{pv.dot}
\title{Add a dot to the visualization.}
\usage{pv.dot(left.name="x", bottom.name="y", bottom.scale=paste("linear", bottom.name, "y", sep = "."), left.scale=paste("linear", left.name, "x", sep = "."), ...)}
\description{Add a dot to the visualization.}
\details{\code{pv.dot} Add a dot to the visualization.}
\value{A wv object.}
\keyword{hplot}
\author{Shane Conway \email{shane.conway@gmail.com}}
\references{\url{http://code.google.com/p/protovis-js/wiki/PvWedge}}
\seealso{\code{\link{pv.chart}}, a more low-level charting function.}
\arguments{\item{bottom.name}{The name of the field in the supplied data frame or vector.}
\item{left.name}{The name of the field in the supplied data frame or vector.}
\item{bottom.scale}{The name of the field in the supplied data frame or vector.}
\item{left.scale}{The name of the field in the supplied data frame or vector.}
\item{...}{The parameters from pv.chart}}
\examples{wv <- pv.panel(width=150, height=150)
render.webvis(wv + pv.dot(wv=wv, data=c(1, 1.2, 1.7, 1.5, .7, .5, .2), ymax=140, xmax=140, xmin=10, ymin=10, scale.min=0))}
