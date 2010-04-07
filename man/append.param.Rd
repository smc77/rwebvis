\name{append.param}
\alias{append.param}
\title{Generic function for all Protovis mark types.}
\usage{append.param(paramlist, name, value, param.name, param.scale, scale.min, scale.max, xmin, xmax, ymin, ymax)}
\description{Generic function for all Protovis mark types.}
\details{\code{append.param} Generic function for all Protovis mark types.}
\value{A webvis object.}
\keyword{graphics}
\author{Shane Conway \email{shane.conway@gmail.com}}
\references{\url{http://vis.stanford.edu/protovis/}}
\arguments{\item{wv}{A webvis object}
\item{type}{Can be "Line", "Bar", etc. (see Protovis API)}
\item{data}{A dataset for plotting.}
\item{...}{Any number of pv.param objects.}
\item{anchor}{If anchoring to another object.}}
