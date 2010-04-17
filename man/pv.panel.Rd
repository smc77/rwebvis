\name{pv.panel}
\alias{pv.panel}
\title{A protovis panal.}
\usage{pv.panel(wv, data, width=300, height=200, left, right, bottom, top, anchor)}
\description{A protovis panal.}
\details{\code{pv.panel} A protovis panal.}
\value{The HTML output}
\keyword{hplot}
\author{Shane Conway \email{shane.conway@gmail.com}}
\references{\url{http://vis.stanford.edu/protovis/}}
\arguments{\item{wv}{A webvis param object from pv.param().}
\item{data}{A webvis object.}
\item{width}{The width of the panel (in pixels).}
\item{height}{The height of the panel (in pixels).}
\item{left}{Where the panel should start with respect to the left of the window.}
\item{right}{Where the panel should start with respect to the right of the window.}
\item{bottom}{Where the panel should start with respect to the bottom of the window.}
\item{top}{Where the panel should start with respect to the top of the window.}
\item{anchor}{Whether the panel should be anchored to the parent object.}}
\examples{pv.panel() # the default panel size
pv.panel(width=NULL, height=NULL, anchor="bottom") # a panel with no settings anchored to a parent object}
