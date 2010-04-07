\name{new.webvis}
\alias{new.webvis}
\title{Create a new webvis object to store each layer of the visualization.}
\usage{new.webvis(name="vis", root=pv.panel(width = width, height = height,
    ...), description, width=300, height=200, dataset, branch=list(),
    render, ...)}
\description{Create a new webvis object to store each layer of the visualization.}
\details{\code{new.webvis} Create a new webvis object to store each layer of the visualization.}
\value{A webvis objectt.}
\keyword{graphics}
\author{Shane Conway \email{shane.conway@gmail.com}}
\references{\url{http://vis.stanford.edu/protovis/}}
\arguments{\item{root}{The root node of the visualization.}
\item{branch}{A node layer underneath the root visualization.}
\item{width}{The width in pixels.}
\item{height}{The height in pixels.}
\item{name}{The name of the visualization.}
\item{description}{A description of the visualization.}
\item{dataset}{A dataset associated with the visualization.}}
\examples{new.webvis()}
