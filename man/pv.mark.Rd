\name{pv.mark}
\alias{pv.mark}
\title{Generic function for all Protovis mark types.}
\usage{pv.mark(wv, type, data, ..., anchor)}
\description{Generic function for all Protovis mark types.}
\details{\code{pv.mark} Generic function for all Protovis mark types.  This function can be used
to create any kind of Protovis object regardless of whether it has been exposed separately.}
\value{A webvis object.}
\keyword{hplot}
\author{Shane Conway \email{shane.conway@gmail.com}}
\references{\url{http://vis.stanford.edu/protovis/}}
\arguments{\item{wv}{A webvis object}
\item{type}{Can be "Line", "Bar", etc. (see Protovis API)}
\item{data}{A dataset for plotting.}
\item{...}{Any number of pv.param objects.}
\item{anchor}{If anchoring to another object.}}
\examples{data <- data.frame(y=1:5)
pv.mark(wv=new.webvis(), type="Line", data=data, 
pv.param(name="data", value=data), 
pv.param(name="bottom", data.name="y", scale="linear.y.y"))
pv.mark(type="Label", ...=pv.param(name="text", data.name="y"))
pv.parse(pv.param(name="text", data.name="y"), data=data.frame(y=1:5))}
