\name{pv.param}
\alias{pv.param}
\title{A protovis mark parameter.}
\usage{pv.param(name, data, data.name, value, scale, scale.min, scale.max, xmin, xmax, ymin, ymax, default, quote=TRUE)}
\description{A protovis mark parameter.}
\details{\code{pv.param} A protovis mark parameter.}
\value{A webvis.param object.}
\keyword{hplot}
\author{Shane Conway \email{shane.conway@gmail.com}}
\references{\url{http://vis.stanford.edu/protovis/}}
\arguments{\item{name}{The name of mark parameter.}
\item{data}{The data used in the parameter settings.}
\item{data.name}{The name of the field in the dataset.}
\item{value}{An explicit value for the parameter.}
\item{scale}{Whether the value or data should be scaled.  Can be "linear", "log", or ...}
\item{scale.min}{The minimum scaled value (or defaults to zero) in pixels.}
\item{scale.max}{The maximum scaled value (or defaults to the height/width of the visualization) in pixels.}
\item{xmin}{The minimum x value for the scaled output.}
\item{xmax}{The maximum x value for the scaled output.}
\item{ymin}{The minimum y value for the scaled output.}
\item{ymax}{The maximum y value for the scaled output.}
\item{default}{The default value for the parameter.}
\item{quote}{Whether character values should be quoted.}}
\examples{pv.param(name="data", value="d")}
