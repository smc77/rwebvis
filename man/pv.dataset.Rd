\name{pv.dataset}
\alias{pv.dataset}
\title{Add a dataset as a variable to the visualization.}
\usage{pv.dataset(data, name)}
\description{Add a dataset as a variable to the visualization.}
\details{\code{pv.dataset} Add a dataset as a variable to the visualization.}
\value{A string of the relevant javascript.}
\keyword{hplot}
\author{Shane Conway \email{shane.conway@gmail.com}}
\references{\url{http://vis.stanford.edu/protovis/}}
\arguments{\item{data}{The dataset to be used in the graphic.}
\item{name}{The name of the dataset.}}
\examples{pv.dataset(data=data.frame(wheat=1:10), name="wheat")}
