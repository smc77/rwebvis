\name{pv.scale}
\alias{pv.scale}
\title{A scaling function for protovis.}
\usage{pv.scale(type, width, height, data, data.name, scale.min, scale.max, xmin, xmax, ymin, ymax)}
\description{A scaling function for protovis.}
\details{\code{pv.scale} A scaling function for protovis.}
\value{The HTML output}
\keyword{hplot}
\author{Shane Conway \email{shane.conway@gmail.com}}
\references{\url{http://vis.stanford.edu/protovis/}}
\arguments{\item{type}{The type is a "." separated string which determines whether the scaling is linear, log, or ..., and what parameter should be scaled.}
\item{width}{The width of the panel.}
\item{height}{The height of the panel.}
\item{data}{The data for the panel.}
\item{data.name}{The name of the variable to be scaled.}
\item{scale.min}{The minimum scaled value (or defaults to zero) in pixels.}
\item{scale.max}{The maximum scaled value (or defaults to the height/width of the visualization) in pixels.}
\item{xmin}{The minimum x value for the scaled output.}
\item{xmax}{The maximum x value for the scaled output.}
\item{ymin}{The minimum y value for the scaled output.}
\item{ymax}{The maximum y value for the scaled output.}}
\examples{pv.scale(type="linear.value.y", width=200, height=200, data=data.frame(value=c(1:5)))}
