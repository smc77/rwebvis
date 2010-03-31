\name{webvisToHTML}
\alias{webvisToHTML}
\title{Convert webvis to HTML.}
\usage{webvisToHTML(wv, div.id="id", html.wrap=TRUE, title=webvis$name, head=getHead(title = title, protovis.path = protovis.path), tail=getTail(),
    protovis.path=PROTOVIS.PATH)}
\description{Convert webvis to HTML.}
\details{\code{webvisToHTML} Convert webvis to HTML.}
\value{A string of the relevant javascript.}
\keyword{graphics}
\author{Shane Conway \email{shane.conway@gmail.com}}
\references{\url{http://vis.stanford.edu/protovis/}}
\arguments{\item{data}{The dataset to be used in the graphic.}
\item{name}{}}
\examples{webvisToHTML(new.webvis())}
