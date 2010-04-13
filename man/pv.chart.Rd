\name{pv.chart}
\alias{pv.chart}
\title{Add a line to the visualization.}
\usage{pv.chart(type, wv, data, bottom, bottom.name, bottom.scale, top, top.name, top.scale, left, left.name, left.scale,
    right, right.name, right.scale, height, height.name, height.scale, width, width.name, width.scale, line.width,
    line.width.name, line.width.scale, size, size.name, size.scale, shape, shape.name, shape.scale, inner.radius,
    inner.radius.name, inner.radius.scale, outer.radius, outer.radius.name, outer.radius.scale, angle, angle.name,
    angle.scale, start.angle, start.angle.name, start.angle.scale, end.angle, end.angle.name, end.angle.scale,
    text, text.name, text.scale, font, text.style, text.align, text.baseline, text.margin, text.angle,
    stroke.style, stroke.style.name, stroke.style.scale, fill.style, fill.style.name, fill.style.scale, segmented,
    interpolate, x.padding, y.padding, xmin, xmax, ymin, ymax, scale.min, scale.max, anchor, render=FALSE, url,
    normalize=FALSE, ...)}
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
\examples{pv.line(data=c(1, 1.2, 1.7, 1.5, .7, .5, .2), bottom.name="y", left.name="x")}
