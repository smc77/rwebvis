\name{webvisToHTML}
\alias{webvisToHTML}
\title{Convert webvis to HTML.}
\usage{webvisToHTML(wv, div.id="id", html.wrap=TRUE, title, head=getHead(title = title, protovis.path = protovis.path), tail=getTail(), protovis.path)}
\description{Convert webvis to HTML.}
\details{\code{webvisToHTML} Convert webvis to HTML.}
\value{The HTML output}
\keyword{hplot}
\author{Shane Conway \email{shane.conway@gmail.com}}
\references{\url{http://vis.stanford.edu/protovis/}}
\arguments{\item{wv}{A webvis.flat object (from the unfold.webvis function()).}
\item{div.id}{The div tag id.}
\item{html.wrap}{Whether to wrap the visualization in other supplied HTML.}
\item{title}{The title of the HTML page.}
\item{head}{The HTML above the webvis.}
\item{tail}{The HTML below the webvis.}
\item{protovis.path}{The path to the protovis javascript.}}
\examples{webvisToHTML(wv=unfold.webvis(new.webvis() + pv.line(data=c(1, 1.2, 1.7, 1.5, .7, .5, .2), bottom.name="y", left.name="x", bottom.scale="linear.y.y", left.scale="linear.x.x", line.width=5, render=FALSE)))}
