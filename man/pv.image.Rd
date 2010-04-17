\name{pv.image}
\alias{pv.image}
\title{Add an image to the visualization.}
\usage{pv.image(url, left.name="x", bottom.name="y", bottom.scale=paste("linear", bottom.name, "y", sep = "."), left.scale=paste("linear", left.name, "x", sep =
    "."), ...)}
\description{Add an image to the visualization.}
\details{\code{pv.image} Add an image to the visualization.}
\value{A wv object.}
\keyword{hplot}
\author{Shane Conway \email{shane.conway@gmail.com}}
\references{\url{http://code.google.com/p/protovis-js/wiki/PvWedge}}
\seealso{\code{\link{pv.chart}}, a more low-level charting function.}
\arguments{\item{url}{The url for the image to be displayed.}
\item{bottom.name}{The name of the field in the supplied data frame or vector.}
\item{left.name}{The name of the field in the supplied data frame or vector.}
\item{bottom.scale}{The name of the field in the supplied data frame or vector.}
\item{left.scale}{The name of the field in the supplied data frame or vector.}
\item{...}{The parameters from pv.chart}}
\examples{wv <- pv.panel(width=150, height=150)
render.webvis(wv + pv.image(url="http://vis.stanford.edu/protovis/ex/stanford.png", left.name=NULL, bottom.name=NULL))
pv.image(url="http://vis.stanford.edu/protovis/ex/stanford.png", width=100, height=100, left.name=NULL, bottom.name=NULL, render=TRUE)
pv.image(data=c(1, 1.2, 1.7, 1.5, .7, .5, .2), url="http://vis.stanford.edu/protovis/ex/stanford.png", width=50, height=50, render=TRUE)}
