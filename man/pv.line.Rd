\name{pv.line}
\alias{pv.line}
\title{Add a line to the visualization.}
\usage{pv.line(bottom.name="y", left.name="x", bottom.scale=paste("linear", bottom.name, "y", sep = "."), left.scale="linear.x.x", ...)}
\description{Add a line to the visualization.}
\details{\code{pv.line} Adds a line plot to the visualization}
\value{A wv object.}
\keyword{hplot}
\author{Shane Conway \email{shane.conway@gmail.com}}
\references{\url{http://vis.stanford.edu/protovis/}}
\seealso{\code{\link{pv.chart}} that creates the webvis object.}
\arguments{\item{bottom.name}{The name of the field in the supplied data frame or vector.}
\item{left.name}{The name of the field in the supplied data frame or vector.}
\item{bottom.scale}{The name of the field in the supplied data frame or vector.}
\item{left.scale}{The name of the field in the supplied data frame or vector.}
\item{...}{The parameters from pv.chart}}
\examples{#pv.line()
#pv.line(anchor="top")	
#pv.line(data=c(1, 1.2, 1.7, 1.5, .7, .5, .2), bottom.name="y", left.name="x", bottom.scale="linear.y.y", left.scale="linear.x.x", line.width=5, render=TRUE)

# line example 1
pv.line(data=c(1, 1.2, 1.7, 1.5, .7, .5, .2), render=FALSE)
pv.line(data=data.frame(z=c(1, 1.2, 1.7, 1.5, .7, .5, .2)), bottom.name="z", render=TRUE)
pv.line(data=c(1, 1.2, 1.7, 1.5, .7, .5, .2), bottom.name="y", left.name="x", bottom.scale=NULL, left.scale="linear.x.x", render=TRUE)

# line example 1 (using layers)
wv <- pv.panel(width=150, height=150)
render.webvis(wv + pv.line(wv=wv, data=c(1, 1.2, 1.7, 1.5, .7, .5, .2), bottom.name="y", left.name="x", bottom.scale="linear.y.y", left.scale="linear.x.x"))

# line example 2 (need to make sure that it doesn't go over the edge
wv <- pv.panel(width=150, height=150)
wv <- wv + (pv.line(wv=wv, data=c(1, 1.2, 1.7, 1.5, .7, .5, .2), bottom.name="y", left.name="x", bottom.scale="linear.y.y", left.scale="linear.x.x")
+ pv.dot())
render.webvis(wv)

# line example 4
wv <- pv.panel(width=150, height=150)
render.webvis(wv + pv.line(wv=wv, data=c(1, 1.2, 1.7, 1.5, .7, .5, .2), top.name="y", left.name="x", top.scale="linear.y.y", left.scale="linear.x.x"))

# line example 5
wv <- pv.panel(width=150, height=150)
render.webvis(wv + pv.line(wv=wv, data=c(1, 1.2, 1.7, 1.5, .7, .5, .2), top.name="y", right.name="x", top.scale="linear.y.y", right.scale="linear.x.x"))

# line example 7
wv <- pv.panel(width=150, height=150)
render.webvis(wv + pv.line(wv=wv, data=c(1, 1.2, 1.7, 1.5, .7, .5, .2), bottom.name="y", left.name="x", bottom.scale="linear.y.y", left.scale="linear.x.x", interpolate="step-after"))}
