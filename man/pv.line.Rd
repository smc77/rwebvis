\name{pv.line}
\alias{pv.line}
\title{Add a line to the visualization.}
\usage{pv.line(wv, data, bottom, bottom.name, bottom.scale, top, top.name,
    top.scale, left, left.name, left.scale, right, right.name,
    right.scale, line.width, stroke.style,
    segmented=(!missing(line.width) || field.exists(field = "width",
    data = data)), fill.style, interpolate, x.padding=(wv$width)/50,
    y.padding=(wv$height)/50, xmin, xmax, ymin, ymax, scale="linear",
    anchor, ...)}
\description{Add a line to the visualization.}
\details{\code{pv.line} Adds a line plot to the visualization}
\value{A wv object.}
\keyword{graphics}
\author{Shane Conway \email{shane.conway@gmail.com}}
\references{\url{http://vis.stanford.edu/protovis/}}
\seealso{\code{\link{new.webvis}} that creates the webvis object.}
\arguments{\item{wv}{The webvis object containing the visualization.}
\item{data}{The webvis object containing the visualization.}
\item{bottom}{The width of the panel in pixels.}
\item{top}{The width of the panel in pixels.}
\item{left}{The width of the panel in pixels.}
\item{right}{The width of the panel in pixels.}
\item{line.width}{The width of the panel in pixels.}
\item{stroke.style}{The width of the panel in pixels.}
\item{fill.style}{The width of the panel in pixels.}
\item{interpolate}{The width of the panel in pixels.}
\item{spacing}{The width of the panel in pixels.}
\item{equal.spacing}{The width of the panel in pixels.}}
