\name{pv.bar}
\alias{pv.bar}
\title{Add a bar to the visualization.}
\usage{pv.bar(height.name="y", left.name="x", height.scale="linear.y.y", left.scale="linear.x.x", bottom=0, width, width.name, xmax, scale.min=0, ...)}
\description{Add a bar to the visualization.}
\details{\code{pv.bar} Adds a bar plot to the visualization}
\value{A wv object.}
\keyword{graphics}
\author{Shane Conway \email{shane.conway@gmail.com}}
\references{\url{http://vis.stanford.edu/protovis/}}
\seealso{\code{\link{.pv.chart}} that creates the webvis object.}
\arguments{\item{height.name}{The name of the field in the supplied data frame or vector.}
\item{left.name}{The name of the field in the supplied data frame or vector.}
\item{height.scale}{The name of the field in the supplied data frame or vector.}
\item{left.scale}{The name of the field in the supplied data frame or vector.}
\item{height.name}{The name of the field in the supplied data frame or vector.}
\item{bottom}{}
\item{width}{}
\item{width.name}{}
\item{xmax}{}
\item{scale.min}{}
\item{...}{The parameters from .pv.chart}}
\examples{plot.webvis(data=c(1, 2, 1.5, 3, 1.2), "bar", scale.min=0)
pv.bar(data=c(1, 1.2, 1.7, 1.5, .7, .5, .2), height.name="y", left.name="x", height.scale="linear.y.y", left.scale="linear.x.x", bottom=0, width=25, render=TRUE)

pv.bar(data=c(1, 1.2, 1.7, 1.5, .7, .5, .2), width=20, render=TRUE)
pv.bar(data=data.frame(z=c(1, 1.2, 1.7, 1.5, .7, .5, .2)), height.name="z", height.scale="linear.z.y", width=20, render=TRUE)
pv.bar(data=c(1, 1.2, 1.7, 1.5, .7, .5, .2), height.name="y", left.name="x", height.scale=NULL, left.scale="linear.x.x", render=TRUE)

# bar example 1 
wv <- new.webvis(width=150, height=150)
render.webvis(wv + pv.bar(wv=wv, data=c(1, 1.2, 1.7, 1.5, .7), height.name="y", left.name="x", height.scale="linear.y.y", left.scale="linear.x.x"))

# bar example 2 (doesn't work properly)
d <- data.frame(y=c(1, 1.2, 1.7, 1.5, .7), z=c(0, 0.5, 0.9, 0.2, 0.7))
d <- cbind(d, k=d$y-d$z)
wv <- new.webvis(width=150, height=150)
render.webvis(wv + pv.bar(wv=wv, data=d, height=20, height.name=NULL, bottom=NULL, bottom.name="x", width=NULL, width.name="k", left.name="z", left.scale="linear.z.y", bottom.scale="linear.x.x", width.scale="linear.k.x"))

# bar example 3
wv <- new.webvis(width=150, height=150)
render.webvis(wv + pv.bar(wv=wv, data=c(1, 1.2, 1.7, 1.5, .7), height.name="y", left.name="x", bottom=NULL, top=0, height.scale="linear.y.y", left.scale="linear.x.x"))}
