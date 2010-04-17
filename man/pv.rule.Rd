\name{pv.rule}
\alias{pv.rule}
\title{Add an rule to the visualization.}
\usage{pv.rule(axis, data, left, left.name, left.scale, bottom, bottom.name, bottom.scale, ...)}
\description{Add an rule to the visualization.}
\details{\code{pv.rule} Add an rule to the visualization.}
\value{A wv object.}
\keyword{hplot}
\author{Shane Conway \email{shane.conway@gmail.com}}
\references{\url{http://code.google.com/p/protovis-js/wiki/PvWedge}}
\seealso{\code{\link{pv.chart}}, a more low-level charting function.}
\arguments{\item{axis}{can be either "x" or "y"}
\item{data}{The data to be used in the function.}
\item{left}{an exact position w.r.t. the left side of the panel}
\item{bottom}{an exact position w.r.t. the bottom side of the panel}
\item{bottom.name}{The name of the field in the supplied data frame or vector.}
\item{left.name}{The name of the field in the supplied data frame or vector.}
\item{bottom.scale}{The name of the field in the supplied data frame or vector.}
\item{left.scale}{The name of the field in the supplied data frame or vector.}
\item{...}{The parameters from pv.chart}}
\examples{data <- data.frame(y=c(1, 1.2, 1.7, 1.5, .7, .5, .2))
wv <- pv.panel(width=300, height=200, left=20, top=20, right=20, bottom=20)
wv <- wv + (pv.rule(wv=wv, data=data, axis="y", stroke.style="rgba(128,128,128,.2)", scale.min=0) + pv.label(anchor="right", text.name="y"))
wv <- wv + (pv.rule(wv=wv, data=1:10, axis="x", stroke.style="rgba(128,128,128,.2)") + pv.label(anchor="bottom", text.name="x"))
wv <- wv + pv.line(wv=wv, data=data, scale.min=0)
render.webvis(wv)}
