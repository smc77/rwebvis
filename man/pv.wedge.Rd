\name{pv.wedge}
\alias{pv.wedge}
\title{Add a wedge to the visualization (for pie charts, etc).}
\usage{pv.wedge(wv=pv.panel(), left, bottom, angle.name="y", inner.radius, outer.radius, outer.radius.name, ...)}
\description{Add a wedge to the visualization (for pie charts, etc).}
\details{\code{pv.wedge} Add a wedge to the visualization (for pie charts, etc).}
\value{A wv object.}
\keyword{hplot}
\author{Shane Conway \email{shane.conway@gmail.com}}
\references{\url{http://code.google.com/p/protovis-js/wiki/PvWedge}}
\seealso{\code{\link{pv.chart}}, a more low-level charting function.}
\arguments{\item{wv}{A webvis object (defaults to an empty panel).}
\item{left}{Where the pie chart will be centered w.r.t. the left side of the panel.}
\item{bottom}{Where the pie chart will be centered w.r.t. the bottom of the panel.}
\item{angle.name}{The name of the data field which contains the angle of each wedge.}
\item{inner.radius}{The inner radius of the chart.}
\item{outer.radius}{The outer radius of the chart (defaults to the width/height).}
\item{outer.radius.name}{The name of a data field for varying radius for each wedge.}
\item{...}{The parameters from pv.chart}}
\examples{pv.wedge(data=c(1, 1.2, 1.7, 1.5, .7, .5, .2), render=TRUE)
pv.wedge(data=c(1, 1.2, 1.7, 1.5, .7, .5, .2), outer.radius=70, angle.name="y", render=TRUE)
pv.wedge(data=c(1, 1.2, 1.7, 1.5, .7, .5, .2), inner.radius=50, outer.radius=70, angle.name="y", render=TRUE)
pv.wedge(data=data.frame(y=c(1, 1.2, 1.7, 1.5, .7, .5, .2), rad=15*(1:7)), inner.radius=50, outer.radius.name="rad", angle.name="y", render=TRUE)}
