\name{pv.label}
\alias{pv.label}
\title{Add a label to the visualization.}
\usage{pv.label(text, text.name, ...)}
\description{Add a label to the visualization.}
\details{\code{pv.label} Add a label to the visualization.}
\value{A wv object.}
\keyword{hplot}
\author{Shane Conway \email{shane.conway@gmail.com}}
\references{\url{http://code.google.com/p/protovis-js/wiki/PvWedge}}
\seealso{\code{\link{pv.chart}}, a more low-level charting function.}
\arguments{\item{text}{The text for the label.}
\item{text.name}{The name of the column from supplied data.}
\item{...}{The parameters from pv.chart}}
\examples{wv <- pv.panel(width=300, height=200, left=20, top=20, right=20, bottom=20)
wv <- wv + (pv.rule(wv=wv, data=1:10, axis="y", stroke.style="rgba(128,128,128,.2)") + pv.label(anchor="right", text.name="y"))
wv <- wv + (pv.rule(wv=wv, data=1:10, axis="x", stroke.style="rgba(128,128,128,.2)") + pv.label(anchor="bottom", text.name="x"))
wv <- wv + pv.dot(wv=wv, data=c(1, 1.2, 1.7, 1.5, .7, .5, .2), scale.min=0)
render.webvis(wv)}
