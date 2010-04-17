\name{pv.area}
\alias{pv.area}
\title{Add a bar to the visualization.}
\usage{pv.area(bottom=0, height.name="y", left.name="x", height.scale=paste("linear", height.name, "y", sep = "."), left.scale=paste("linear", left.name, "x", sep =
    "."), scale.min=0, ...)}
\description{Add a bar to the visualization.}
\details{\code{pv.area} Adds a bar plot to the visualization}
\value{A wv object.}
\keyword{hplot}
\author{Shane Conway \email{shane.conway@gmail.com}}
\references{\url{http://vis.stanford.edu/protovis/}}
\seealso{\code{\link{pv.chart}} that creates the webvis object.}
\arguments{\item{height.name}{The name of the field in the supplied data frame or vector.}
\item{left.name}{The name of the field in the supplied data frame or vector.}
\item{height.scale}{The scale of the field in the supplied data frame or vector.}
\item{left.scale}{The scale of the field in the supplied data frame or vector.}
\item{bottom}{The distance from the bottom of the panel.}
\item{scale.min}{The minimum value for the y-axis scale.}
\item{...}{The parameters from pv.chart}}
\examples{# line example 1 (using layers)
wv <- pv.panel(width=150, height=150)
render.webvis(wv + pv.area(wv=wv, data=c(1, 1.2, 1.7, 1.5, .7, .5, .2), bottom=0, height.name="y", left.name="x", height.scale="linear.y.y", left.scale="linear.x.x"))

wv <- pv.panel(width=150, height=150)
render.webvis(wv + pv.area(wv=wv, data=c(1, 1.2, 1.7, 1.5, .7, .5, .2)))

# line example 2 (need to make sure that it doesn't go over the edge
wv <- pv.panel(width=150, height=150)
wv <- wv + (pv.area(wv=wv, data=c(1, 1.2, 1.7, 1.5, .7, .5, .2), height.name="y", left.name="x", height.scale="linear.y.y", left.scale="linear.x.x"))
render.webvis(wv)

# line example 4
wv <- pv.panel(width=150, height=150)
render.webvis(wv + pv.area(wv=wv, data=c(1, 1.2, 1.7, 1.5, .7, .5, .2), top.name="y", left.name="x", top.scale="linear.y.y", left.scale="linear.x.x"))

# line example 5
wv <- pv.panel(width=150, height=150)
render.webvis(wv + pv.area(wv=wv, data=c(1, 1.2, 1.7, 1.5, .7, .5, .2), bottom=0, top.name="y", right.name="x", top.scale="linear.y.y", right.scale="linear.x.x"))

# line example 7
wv <- pv.panel(width=150, height=150)
render.webvis(wv + pv.area(wv=wv, data=c(1, 1.2, 1.7, 1.5, .7, .5, .2), bottom.name="y", left.name="x", bottom.scale="linear.y.y", left.scale="linear.x.x", interpolate="step-after"))

#pv.area(data=c(1, 1.2, 1.7, 1.5, .7, .5, .2), height.name="y", left.name="x", height.scale="linear.y.y", left.scale="linear.x.x", ymin=10, ymax=100, bottom=0, render=TRUE)}
