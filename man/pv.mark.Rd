\name{pv.mark}
\alias{pv.mark}
\title{Generic function for all Protovis mark types.}
\usage{pv.mark(wv, type, data, ..., anchor)}
\description{Generic function for all Protovis mark types.}
\details{\code{pv.mark} Generic function for all Protovis mark types.}
\value{A webvis object.}
\keyword{graphics}
\author{Shane Conway \email{shane.conway@gmail.com}}
\references{\url{http://vis.stanford.edu/protovis/}}
\arguments{\item{wv}{A webvis object}
\item{type}{Can be "Line", "Bar", etc. (see Protovis API)}
\item{data}{A dataset for plotting.}
\item{...}{Any number of pv.param objects.}
\item{anchor}{If anchoring to another object.}}
\examples{pv.mark(wv=new.webvis(), type="Line", data=data.frame(y=1:5), 
pv.param(name="data", value=data), 
pv.param(name="bottom", data.name="y", scale="linear.y.y"))}
