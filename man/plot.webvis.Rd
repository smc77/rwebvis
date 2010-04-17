\name{plot.webvis}
\alias{plot.webvis}
\title{Simplified plot function for web vis plots.}
\usage{plot.webvis(x, y, type="dot", width=300, height=200, add.grid=TRUE, add.axes=TRUE, scale.min, ...)}
\description{Simplified plot function for web vis plots.}
\details{\code{plot.webvis} Simplified plot function for web vis plots}
\value{Opens a plot in a browser window.}
\keyword{hplot}
\author{Shane Conway \email{shane.conway@gmail.com}}
\references{\url{http://vis.stanford.edu/protovis/}}
\seealso{\code{\link{new.webvis}} that creates the webvis object.}
\arguments{\item{x}{Either the "x" axis data or all the data for the visualization (can be vector or dataset).}
\item{y}{Optional, can specify the "y" axis data.}
\item{type}{The type of plot.  Can be "bar", "line", "area", "pie", "dot", or "shape"}
\item{width}{The width of the panel in pixels.}
\item{height}{The width of the panel in pixels.}
\item{add.grid}{Logical value for whether to add a grid.}
\item{add.axes}{Whether to add x-y axes.}
\item{scale.min}{Whether the y-axis should be scaled to zero or the minimum value in the data}
\item{...}{Other parmaeters for pv.chart.}}
\examples{plot.webvis(x=c(1, 2, 1.5, 3, 1.2), type="line")
plot.webvis(x=c(1, 2, 1.5, 3, 1.2), type="area")
plot.webvis(c(1, 2, 1.5, 3, 1.2, 1.7, 2.5, 6, 5), add.grid=FALSE)
plot.webvis(c(1, 2, 1.5, 3, 1.2, 1.7, 2.5, 6, 5), type="area")
plot.webvis(c(1, 2, 1.5, 3, 1.2, 1.7, 2.5, 6, 5), type="line", scale.min=0)
plot.webvis(c(1, 2, 1.5, 3, 1.2, 1.7, 2.5, 6, 5), type="line", scale.min=NULL)
plot.webvis(x=10*rnorm(20), width=500, height=500, type="line")
plot.webvis(x=100*rnorm(20), y=100*rnorm(20), width=500, height=500, type="dot")
plot.webvis(x=c(1, 2, 1.5, 3, 1.2), type="pie")
plot.webvis(x=c(1, 2, 1.5, 3, 1.2), type="pie", inner.radius=80)
plot.webvis(x=1:5, y=c(1, 2, 1.5, 3, 1.2), type="area")}
