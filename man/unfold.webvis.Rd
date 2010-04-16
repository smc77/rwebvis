\name{unfold.webvis}
\alias{unfold.webvis}
\title{Unfolds the visualization tree structure into a flat form.}
\usage{unfold.webvis(wv, name="vis", parent)}
\description{Unfolds the visualization tree structure into a flat form.}
\details{\code{unfold.webvis} Unfolds the visualization tree structure into a flat form.}
\value{A wv.flat object.}
\keyword{hplot}
\author{Shane Conway \email{shane.conway@gmail.com}}
\references{\url{http://vis.stanford.edu/protovis/}}
\seealso{\code{\link{new.webvis}} that creates the webvis object.}
\arguments{\item{wv}{The webvis object containing the visualization.}
\item{name}{The name of the visualization (will show up as variables in the javascript).}
\item{parent}{If the node has a parent (particularly used when function is called recursively).}}
