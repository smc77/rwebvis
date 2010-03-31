\name{new.webvis}
\alias{new.webvis}
\title{Create a new webvis object to store each layer of the visualization.}
\usage{new.webvis(name="vis", root=pv.panel(width = width, height = height, ...), description, width=300, height=200, dataset, branch=list(), ...)}
\description{Create a new webvis object to store each layer of the visualization.}
\details{\code{new.webvis} Create a new webvis object to store each layer of the visualization.}
\value{A webvis objectt.}
\keyword{graphics}
\author{Shane Conway \email{shane.conway@gmail.com}}
\references{\url{http://vis.stanford.edu/protovis/}}
\arguments{\item{root}{The root node of the visualization (defaults to a panel).}
\item{branch}{(optional) a node layer underneath the root visualization.}
\item{width}{The root node of the visualization (defaults to a panel).}
\item{height}{The root node of the visualization (defaults to a panel).}
\item{name}{(optional)}
\item{description}{(optional) A description of the visualization.}
\item{dataset}{(optional) A dataset associated with the visualization.}}
\examples{new.webvis()}
