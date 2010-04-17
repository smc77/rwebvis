\name{append.param}
\alias{append.param}
\title{Used in pv...}
\usage{append.param(paramlist, name, value, param.name, param.scale, scale.min, scale.max, xmin, xmax, ymin, ymax)}
\description{Used in pv.chart to correctly combine parameters from the input.}
\details{\code{append.param} Used in pv.chart to correctly combine parameters from the input.  A "value" has a higher priority than
a name in the data.}
\value{A list of parameters.}
\keyword{hplot}
\author{Shane Conway \email{shane.conway@gmail.com}}
\references{\url{http://vis.stanford.edu/protovis/}}
\arguments{\item{paramlist}{A list of parameters, to be appended.}
\item{name}{The name of the Protovis parameter (e.g. "width").}
\item{value}{A specific value for the parameter (e.g. 200).}
\item{param.name}{The name of the field in the dataset (if not supplying specific value).}
\item{param.scale}{The scaling for the parameter.}
\item{scale.min}{The minimum scaled value (or defaults to zero) in pixels.}
\item{scale.max}{The maximum scaled value (or defaults to the height/width of the visualization) in pixels.}
\item{xmin}{The minimum x value for the scaled output.}
\item{xmax}{The maximum x value for the scaled output.}
\item{ymin}{The minimum y value for the scaled output.}
\item{ymax}{The maximum y value for the scaled output.}}
