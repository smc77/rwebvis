\name{pv.parse}
\alias{pv.parse}
\title{Takes a parameter and a webvis object and parses them.}
\usage{pv.parse(param, wv, data)}
\description{Takes a parameter and a webvis object and parses them.}
\details{\code{pv.parse} Takes a parameter and a webvis object and parses them.}
\value{The HTML output}
\keyword{hplot}
\author{Shane Conway \email{shane.conway@gmail.com}}
\references{\url{http://vis.stanford.edu/protovis/}}
\arguments{\item{param}{A webvis param object from pv.param().}
\item{wv}{A webvis object.}
\item{data}{A dataset.}}
\examples{pv.parse(pv.param(name="text", data.name="y"))
pv.parse(pv.param(name="text", data.name="y"), data=data.frame(y=1:5))}
