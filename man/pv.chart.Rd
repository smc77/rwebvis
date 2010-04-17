\name{pv.chart}
\alias{pv.chart}
\title{Add a chart to the visualization.}
\usage{pv.chart(type, wv, data, bottom, bottom.name, bottom.scale, top, top.name, top.scale, left, left.name, left.scale, right, right.name, right.scale, height,
    height.name, height.scale, width, width.name, width.scale, line.width, line.width.name, line.width.scale, size, size.name, size.scale, shape, shape.name,
    shape.scale, inner.radius, inner.radius.name, inner.radius.scale, outer.radius, outer.radius.name, outer.radius.scale, angle, angle.name, angle.scale,
    start.angle, start.angle.name, start.angle.scale, end.angle, end.angle.name, end.angle.scale, text, text.name, text.scale, font, text.style, text.align,
    text.baseline, text.margin, text.angle, stroke.style, stroke.style.name, stroke.style.scale, fill.style, fill.style.name, fill.style.scale, segmented,
    interpolate, x.padding, y.padding, xmin, xmax, ymin, ymax, scale.min, scale.max, anchor, render=FALSE, url, normalize=FALSE, ...)}
\description{Add a chart to the visualization.}
\details{\code{pv.chart} Adds a chart to the visualization.  This is the core charting function which all other charting functions reference.
Function is generally not used directly, but is called by the higher level functions (e.g. pv.line, pv.dot).}
\value{A wv object.}
\keyword{hplot}
\author{Shane Conway \email{shane.conway@gmail.com}}
\references{\url{http://vis.stanford.edu/protovis/}}
\seealso{\code{\link{new.webvis}} that creates the webvis object.}
\arguments{\item{type}{The type of chart (can be "Line", "Bar", "Dot", "Image", "Area")}
\item{wv}{The webvis object containing the visualization.}
\item{data}{The webvis object containing the visualization.}
\item{bottom.name}{A protovis mark parameter (see API documentation http://protovis-js.googlecode.com/svn/trunk/jsdoc/index.html).}
\item{bottom.scale}{A protovis mark parameter (see API documentation http://protovis-js.googlecode.com/svn/trunk/jsdoc/index.html).}
\item{top.name}{A protovis mark parameter (see API documentation http://protovis-js.googlecode.com/svn/trunk/jsdoc/index.html).}
\item{top.scale}{A protovis mark parameter (see API documentation http://protovis-js.googlecode.com/svn/trunk/jsdoc/index.html).}
\item{left.name}{A protovis mark parameter (see API documentation http://protovis-js.googlecode.com/svn/trunk/jsdoc/index.html).}
\item{left.scale}{A protovis mark parameter (see API documentation http://protovis-js.googlecode.com/svn/trunk/jsdoc/index.html).}
\item{right.name}{A protovis mark parameter (see API documentation http://protovis-js.googlecode.com/svn/trunk/jsdoc/index.html).}
\item{right.scale}{A protovis mark parameter (see API documentation http://protovis-js.googlecode.com/svn/trunk/jsdoc/index.html).}
\item{height}{A protovis mark parameter (see API documentation http://protovis-js.googlecode.com/svn/trunk/jsdoc/index.html).}
\item{height.name}{A protovis mark parameter (see API documentation http://protovis-js.googlecode.com/svn/trunk/jsdoc/index.html).}
\item{height.scale}{A protovis mark parameter (see API documentation http://protovis-js.googlecode.com/svn/trunk/jsdoc/index.html).}
\item{width}{A protovis mark parameter (see API documentation http://protovis-js.googlecode.com/svn/trunk/jsdoc/index.html).}
\item{width.name}{A protovis mark parameter (see API documentation http://protovis-js.googlecode.com/svn/trunk/jsdoc/index.html).}
\item{width.scale}{A protovis mark parameter (see API documentation http://protovis-js.googlecode.com/svn/trunk/jsdoc/index.html).}
\item{line.width.name}{A protovis mark parameter (see API documentation http://protovis-js.googlecode.com/svn/trunk/jsdoc/index.html).}
\item{line.width.scale}{A protovis mark parameter (see API documentation http://protovis-js.googlecode.com/svn/trunk/jsdoc/index.html).}
\item{size}{A protovis mark parameter (see API documentation http://protovis-js.googlecode.com/svn/trunk/jsdoc/index.html).}
\item{size.name}{A protovis mark parameter (see API documentation http://protovis-js.googlecode.com/svn/trunk/jsdoc/index.html).}
\item{size.scale}{A protovis mark parameter (see API documentation http://protovis-js.googlecode.com/svn/trunk/jsdoc/index.html).}
\item{shape}{A protovis mark parameter (see API documentation http://protovis-js.googlecode.com/svn/trunk/jsdoc/index.html).}
\item{shape.name}{A protovis mark parameter (see API documentation http://protovis-js.googlecode.com/svn/trunk/jsdoc/index.html).}
\item{shape.scale}{A protovis mark parameter (see API documentation http://protovis-js.googlecode.com/svn/trunk/jsdoc/index.html).}
\item{inner.radius}{A protovis mark parameter (see API documentation http://protovis-js.googlecode.com/svn/trunk/jsdoc/index.html).}
\item{inner.radius.name}{A protovis mark parameter (see API documentation http://protovis-js.googlecode.com/svn/trunk/jsdoc/index.html).}
\item{inner.radius.scale}{A protovis mark parameter (see API documentation http://protovis-js.googlecode.com/svn/trunk/jsdoc/index.html).}
\item{outer.radius}{A protovis mark parameter (see API documentation http://protovis-js.googlecode.com/svn/trunk/jsdoc/index.html).}
\item{outer.radius.name}{A protovis mark parameter (see API documentation http://protovis-js.googlecode.com/svn/trunk/jsdoc/index.html).}
\item{outer.radius.scale}{A protovis mark parameter (see API documentation http://protovis-js.googlecode.com/svn/trunk/jsdoc/index.html).}
\item{angle}{A protovis mark parameter (see API documentation http://protovis-js.googlecode.com/svn/trunk/jsdoc/index.html).}
\item{angle.name}{A protovis mark parameter (see API documentation http://protovis-js.googlecode.com/svn/trunk/jsdoc/index.html).}
\item{angle.scale}{A protovis mark parameter (see API documentation http://protovis-js.googlecode.com/svn/trunk/jsdoc/index.html).}
\item{start.angle}{A protovis mark parameter (see API documentation http://protovis-js.googlecode.com/svn/trunk/jsdoc/index.html).}
\item{start.angle.name}{A protovis mark parameter (see API documentation http://protovis-js.googlecode.com/svn/trunk/jsdoc/index.html).}
\item{start.angle.scale}{A protovis mark parameter (see API documentation http://protovis-js.googlecode.com/svn/trunk/jsdoc/index.html).}
\item{end.angle}{A protovis mark parameter (see API documentation http://protovis-js.googlecode.com/svn/trunk/jsdoc/index.html).}
\item{end.angle.name}{A protovis mark parameter (see API documentation http://protovis-js.googlecode.com/svn/trunk/jsdoc/index.html).}
\item{end.angle.scale}{A protovis mark parameter (see API documentation http://protovis-js.googlecode.com/svn/trunk/jsdoc/index.html).}
\item{text}{A protovis mark parameter (see API documentation http://protovis-js.googlecode.com/svn/trunk/jsdoc/index.html).}
\item{text.name}{A protovis mark parameter (see API documentation http://protovis-js.googlecode.com/svn/trunk/jsdoc/index.html).}
\item{text.scale}{A protovis mark parameter (see API documentation http://protovis-js.googlecode.com/svn/trunk/jsdoc/index.html).}
\item{font}{A protovis mark parameter (see API documentation http://protovis-js.googlecode.com/svn/trunk/jsdoc/index.html).}
\item{text.style}{A protovis mark parameter (see API documentation http://protovis-js.googlecode.com/svn/trunk/jsdoc/index.html).}
\item{text.align}{A protovis mark parameter (see API documentation http://protovis-js.googlecode.com/svn/trunk/jsdoc/index.html).}
\item{text.baseline}{A protovis mark parameter (see API documentation http://protovis-js.googlecode.com/svn/trunk/jsdoc/index.html).}
\item{text.margin}{A protovis mark parameter (see API documentation http://protovis-js.googlecode.com/svn/trunk/jsdoc/index.html).}
\item{text.angle}{A protovis mark parameter (see API documentation http://protovis-js.googlecode.com/svn/trunk/jsdoc/index.html).}
\item{stroke.style.name}{A protovis mark parameter (see API documentation http://protovis-js.googlecode.com/svn/trunk/jsdoc/index.html).}
\item{stroke.style.scale}{A protovis mark parameter (see API documentation http://protovis-js.googlecode.com/svn/trunk/jsdoc/index.html).}
\item{fill.style.name}{A protovis mark parameter (see API documentation http://protovis-js.googlecode.com/svn/trunk/jsdoc/index.html).}
\item{fill.style.scale}{A protovis mark parameter (see API documentation http://protovis-js.googlecode.com/svn/trunk/jsdoc/index.html).}
\item{segmented}{A protovis mark parameter (see API documentation http://protovis-js.googlecode.com/svn/trunk/jsdoc/index.html).}
\item{x.padding}{A protovis mark parameter (see API documentation http://protovis-js.googlecode.com/svn/trunk/jsdoc/index.html).}
\item{y.padding}{A protovis mark parameter (see API documentation http://protovis-js.googlecode.com/svn/trunk/jsdoc/index.html).}
\item{xmin}{A protovis mark parameter (see API documentation http://protovis-js.googlecode.com/svn/trunk/jsdoc/index.html).}
\item{xmax}{A protovis mark parameter (see API documentation http://protovis-js.googlecode.com/svn/trunk/jsdoc/index.html).}
\item{ymin}{A protovis mark parameter (see API documentation http://protovis-js.googlecode.com/svn/trunk/jsdoc/index.html).}
\item{ymax}{A protovis mark parameter (see API documentation http://protovis-js.googlecode.com/svn/trunk/jsdoc/index.html).}
\item{scale.min}{A protovis mark parameter (see API documentation http://protovis-js.googlecode.com/svn/trunk/jsdoc/index.html).}
\item{scale.max}{A protovis mark parameter (see API documentation http://protovis-js.googlecode.com/svn/trunk/jsdoc/index.html).}
\item{anchor}{A protovis mark parameter (see API documentation http://protovis-js.googlecode.com/svn/trunk/jsdoc/index.html).}
\item{render}{A protovis mark parameter (see API documentation http://protovis-js.googlecode.com/svn/trunk/jsdoc/index.html).}
\item{url}{A protovis mark parameter (see API documentation http://protovis-js.googlecode.com/svn/trunk/jsdoc/index.html).}
\item{normalize}{A protovis mark parameter (see API documentation http://protovis-js.googlecode.com/svn/trunk/jsdoc/index.html).}
\item{...}{A protovis mark parameter (see API documentation http://protovis-js.googlecode.com/svn/trunk/jsdoc/index.html).}
\item{bottom}{The width of the panel in pixels.}
\item{top}{The width of the panel in pixels.}
\item{left}{The width of the panel in pixels.}
\item{right}{The width of the panel in pixels.}
\item{line.width}{The width of the panel in pixels.}
\item{stroke.style}{The width of the panel in pixels.}
\item{fill.style}{The width of the panel in pixels.}
\item{interpolate}{The width of the panel in pixels.}}
