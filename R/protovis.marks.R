# TODO: Add comment
# 
# Author: TMC
###############################################################################



#pv.line()
#pv.line(anchor="top")	
#pv.line(data=c(1, 1.2, 1.7, 1.5, .7, .5, .2), bottom.name="y", left.name="x", bottom.scale="linear.y.y", left.scale="linear.x.x", line.width=5, render=TRUE)

# line example 1
pv.line(data=c(1, 1.2, 1.7, 1.5, .7, .5, .2), render=TRUE)
pv.line(data=data.frame(z=c(1, 1.2, 1.7, 1.5, .7, .5, .2)), bottom.name="z", render=TRUE)
pv.line(data=c(1, 1.2, 1.7, 1.5, .7, .5, .2), bottom.name="y", left.name="x", bottom.scale=NULL, left.scale="linear.x.x", render=TRUE)

# line example 1 (using layers)
wv <- new.webvis(width=150, height=150)
render.webvis(wv + pv.line(wv=wv, data=c(1, 1.2, 1.7, 1.5, .7, .5, .2), bottom.name="y", left.name="x", bottom.scale="linear.y.y", left.scale="linear.x.x"))

# line example 2 (need to make sure that it doesn't go over the edge
wv <- new.webvis(width=150, height=150)
wv <- wv + (new.webvis(wv=wv, root=pv.line(wv=wv, data=c(1, 1.2, 1.7, 1.5, .7, .5, .2), bottom.name="y", left.name="x", bottom.scale="linear.y.y", left.scale="linear.x.x"))
			+ pv.dot())
render.webvis(wv)

# line example 4
wv <- new.webvis(width=150, height=150)
render.webvis(wv + pv.line(wv=wv, data=c(1, 1.2, 1.7, 1.5, .7, .5, .2), top.name="y", left.name="x", top.scale="linear.y.y", left.scale="linear.x.x"))

# line example 5
wv <- new.webvis(width=150, height=150)
render.webvis(wv + pv.line(wv=wv, data=c(1, 1.2, 1.7, 1.5, .7, .5, .2), top.name="y", right.name="x", top.scale="linear.y.y", right.scale="linear.x.x"))

# line example 7
wv <- new.webvis(width=150, height=150)
render.webvis(wv + pv.line(wv=wv, data=c(1, 1.2, 1.7, 1.5, .7, .5, .2), bottom.name="y", left.name="x", bottom.scale="linear.y.y", left.scale="linear.x.x", interpolate="step-after"))

pv.line <- function(bottom.name="y", left.name="x", bottom.scale=paste("linear", bottom.name, "y", sep="."), left.scale="linear.x.x", ...) {
	vis <- .pv.chart(type="Line", bottom.name=bottom.name, left.name=left.name, bottom.scale=bottom.scale, left.scale=left.scale, ...)
	vis
}

#' Add a bar to the visualization.
#'
#' \code{pv.bar} Adds a line plot to the visualization
#'
#' @param height.name The name of the field in the supplied data frame or vector. 
#' @param left.name The name of the field in the supplied data frame or vector. 
#' @param height.scale The name of the field in the supplied data frame or vector. 
#' @param left.scale The name of the field in the supplied data frame or vector. 
#' @param height.name The name of the field in the supplied data frame or vector. 
#' @param ... The parameters from .pv.chart 
#' @return A wv object.
#' @keywords graphics
#' @author Shane Conway \email{shane.conway@@gmail.com}
#' @references
#' \url{http://vis.stanford.edu/protovis/}
#' @seealso \code{\link{.pv.chart}} that creates the webvis object.
#' @examples
#' plot.webvis(data=c(1, 2, 1.5, 3, 1.2), "bar", scale.min=0)
#' pv.bar(data=c(1, 1.2, 1.7, 1.5, .7, .5, .2), height.name="y", left.name="x", height.scale="linear.y.y", left.scale="linear.x.x", bottom=0, width=25, render=TRUE)

pv.bar(data=c(1, 1.2, 1.7, 1.5, .7, .5, .2), width=20, render=TRUE)
pv.bar(data=data.frame(z=c(1, 1.2, 1.7, 1.5, .7, .5, .2)), height.name="z", height.scale="linear.z.y", width=20, render=TRUE)
pv.bar(data=c(1, 1.2, 1.7, 1.5, .7, .5, .2), height.name="y", left.name="x", height.scale=NULL, left.scale="linear.x.x", render=TRUE)

# bar example 1 (using layers)
wv <- new.webvis(width=150, height=150)
render.webvis(wv + pv.bar(wv=wv, data=c(1, 1.2, 1.7, 1.5, .7), height.name="y", left.name="x", height.scale="linear.y.y", left.scale="linear.x.x"))

# bar example 2 (doesn't work properly)
d <- data.frame(y=c(1, 1.2, 1.7, 1.5, .7), z=c(0, 0.5, 0.9, 0.2, 0.7))
d <- cbind(d, k=d$y-d$z)
wv <- new.webvis(width=150, height=150)
render.webvis(wv + pv.bar(wv=wv, data=d, height=20, height.name=NULL, bottom=NULL, bottom.name="x", width=NULL, width.name="k", left.name="z", left.scale="linear.z.y", bottom.scale="linear.x.x", width.scale="linear.k.x"))

# bar example 3
wv <- new.webvis(width=150, height=150)
render.webvis(wv + pv.bar(wv=wv, data=c(1, 1.2, 1.7, 1.5, .7), height.name="y", left.name="x", bottom=NULL, top=0, height.scale="linear.y.y", left.scale="linear.x.x"))

pv.bar <- function(height.name="y", left.name="x", height.scale="linear.y.y", left.scale="linear.x.x", bottom=0, width=NULL, width.name=NULL, xmax=NULL, scale.min=0, ...) {
	args <- list(...)
	panel.width <- args[names(args)=="wv"]$wv$width
	n <- args[names(args)=="data"]$data
	if(is.null(width) && is.null(width.name)) if(is.data.frame(n)) width <- (panel.width/nrow(n))/1.2 else width <- (panel.width/length(n))/1.2
	if(is.null(xmax) && esse(panel.width)) xmax <- panel.width - width
	vis <- .pv.chart(type="Bar", height.name=height.name, left.name=left.name, height.scale=height.scale, left.scale=left.scale, bottom=bottom, width=width, width.name=width.name, xmax=xmax, scale.min=scale.min, ...)
	vis
}


# line example 1 (using layers)
wv <- new.webvis(width=150, height=150)
render.webvis(wv + pv.area(wv=wv, data=c(1, 1.2, 1.7, 1.5, .7, .5, .2), bottom=0, height.name="y", left.name="x", height.scale="linear.y.y", left.scale="linear.x.x"))

wv <- new.webvis(width=150, height=150)
render.webvis(wv + pv.area(wv=wv, data=c(1, 1.2, 1.7, 1.5, .7, .5, .2)))

# line example 2 (need to make sure that it doesn't go over the edge
wv <- new.webvis(width=150, height=150)
wv <- wv + (new.webvis(wv=wv, root=pv.area(wv=wv, data=c(1, 1.2, 1.7, 1.5, .7, .5, .2), height.name="y", left.name="x", height.scale="linear.y.y", left.scale="linear.x.x"))
			+ pv.dot())
render.webvis(wv)

# line example 4
wv <- new.webvis(width=150, height=150)
render.webvis(wv + pv.area(wv=wv, data=c(1, 1.2, 1.7, 1.5, .7, .5, .2), top.name="y", left.name="x", top.scale="linear.y.y", left.scale="linear.x.x"))

# line example 5
wv <- new.webvis(width=150, height=150)
render.webvis(wv + pv.area(wv=wv, data=c(1, 1.2, 1.7, 1.5, .7, .5, .2), top.name="y", right.name="x", top.scale="linear.y.y", right.scale="linear.x.x"))

# line example 7
wv <- new.webvis(width=150, height=150)
render.webvis(wv + pv.area(wv=wv, data=c(1, 1.2, 1.7, 1.5, .7, .5, .2), bottom.name="y", left.name="x", bottom.scale="linear.y.y", left.scale="linear.x.x", interpolate="step-after"))


#pv.area(data=c(1, 1.2, 1.7, 1.5, .7, .5, .2), height.name="y", left.name="x", height.scale="linear.y.y", left.scale="linear.x.x", ymin=10, ymax=100, bottom=0, render=TRUE)
pv.area <- function(bottom=0, height.name="y", left.name="x", height.scale=paste("linear", height.name, "y", sep="."), left.scale=paste("linear", left.name, "x", sep="."), scale.min=0, ...) {
	vis <- .pv.chart(type="Area", bottom=bottom, height.name=height.name, left.name=left.name, height.scale=height.scale, left.scale=left.scale, scale.min=0, ...)
	vis
}

#
#pv.area <- function(...) {
#	vis <- .pv.chart(type="Area", ...)
#	vis
#}
#pv.wedge(data=c(1, 1.2, 1.7, 1.5, .7, .5, .2), left=75, bottom=75, outer.radius=70, angle.name="y", render=TRUE)
#pv.wedge(data=c(1, 1.2, 1.7, 1.5, .7, .5, .2), left=150, bottom=75, inner.radius=50, outer.radius=70, angle.name="y", render=TRUE)
#pv.wedge(data=data.frame(y=c(1, 1.2, 1.7, 1.5, .7, .5, .2), rad=20*(1:7)), left=75, bottom=75, inner.radius=50, outer.radius.name="rad", angle.name="y", render=TRUE)
pv.wedge <- function(...) {
	vis <- .pv.chart(type="Wedge", ..., normalize=TRUE)
	vis
}

pv.dot <- function(...) {
	vis <- .pv.chart(type="Dot", ...)
	vis
}

pv.shape <- function(...) {
	vis <- .pv.chart(type="Shape", ...)
	vis
}

pv.image <- function(...) {
	vis <- .pv.chart(type="Image", ...)
	vis
}

pv.rule <- function(...) {
	vis <- .pv.chart(type="Rule", ...)
	vis
}

pv.label <- function(...) {
	vis <- .pv.chart(type="Label", ...)
	vis
}



#
# http://code.google.com/p/protovis-js/wiki/PvWedge
#
#wv <- new.webvis(width=150, height=150)
#wv <- wv + pv.wedge(wv, data=data.frame(y=c(1, 2, 1.5, 3, 1.2)))
#render.webvis(wv=wv)

#wv <- new.webvis(width=150, height=150)
#wv <- wv + pv.wedge(wv, data=data.frame(y=c(1, 2, 1.5, 3, 1.2)), inner.radius=50)
#render.webvis(wv=wv)

#plot.webvis(data=c(1, 2, 1.5, 3, 1.2), "pie")
#pv.wedge <- function(wv, data, bottom, left, right, inner.radius, outer.radius, fill.style, angle, equal.spacing=TRUE, anchor=NULL) {
#	data$y <- (data$y/sum(data$y)) * 2 * pi
#	vis <- list(type="pv.Wedge",
#			parameters=collapse(
#					pv.parameter("data", value=data),
#					pv.parameter("bottom", value=bottom, default=wv$height/2),
#					pv.parameter("left", value=left, default=wv$width/2),
#					pv.parameter("innerRadius", value=inner.radius),
#					pv.parameter("outerRadius", value=outer.radius, default=min(wv$width,wv$height)/2),
#					pv.parameter("fillStyle", value=fill.style),
#					pv.parameter("angle", value=angle, data=data, field="y"),
#					";"),
#			anchor=anchor)
#	vis
#}
#
#
#pv.dot <- function(wv, data, bottom=(wv$height/2), top, left=(wv$width/2), right, size, shape, stroke.style, fill.style) {
#	multiplier <- (wv$height / max(data)) - 5
#	interval <- ((wv$width-30) / length(data))
#	vis <- list(type="pv.Dot",
#			parameters=paste(if(!missing(data) || length(data)) paste(".data(", pv.data(data), ")") else "",
#			if(!missing(bottom) || length(bottom)) paste(".bottom(function(d) d * ", multiplier, ")"),
#			if(!missing(left) || length(left)) paste(".left(function() this.index * ", interval, " + 15)"),
#			if(!missing(size)) paste(".size(function() this.index * ", interval, " + 15)") else "",
#			if(!missing(shape)) paste(".shape(", shape, "") else "",
#			";", sep=""),
#	anchor=anchor)
#	vis
#}
#
#pv.rule <- function(wv, data, y.name, x.name, bottom, height, left, right, bar.width, line.width, stroke.style, segmented=(!missing(line.width) || field.exists(field="width", data=data)), interpolate, fill.style, x.padding=(wv$width)/50, y.padding=(wv$height)/50, xmin, xmax, ymin, ymax, scale="linear", anchor=NULL) {
#	vis <- list(type="pv.Rule",
#			parameters=collapse(
#					pv.parameter("data", value=data),
#					pv.parameter("bottom", data=data, field=y.name, scale.min=ymin, scale.max=ymax, value=bottom, range.min=y.padding, range.max=wv$height-y.padding, scale=scale),
#					pv.parameter("left", data=data, field=x.name, scale.min=xmin, scale.max=xmax, value=left, range.min=x.padding, range.max=wv$width-x.padding, scale=scale),
#					pv.parameter("strokeStyle", value=stroke.style),
#					pv.parameter("fillStyle", value=fill.style),
#					pv.parameter("interpolate", value=interpolate),
#					";"),
#			anchor=anchor)
#	vis
#}
##
##wv <- new.webvis(width=150, height=150)
##pw <- new.webvis(root=pv.bar(wv, data=data.frame(y=c(1, 2, 1.5, 3, 1.2)))) + pv.label(text="function(d) d.y")
##wv <- wv + pw
##render.webvis(wv=wv)
#
#pv.label <- function(wv, data, y.name="y", x.name="x", bottom, height, left, right, width, text, font, textAlign, textBaseline, textMargin, textAngle, text.style, anchor=NULL) {
#	if(!missing(data) && !field.exists("x", data))
#		data$x <- 1:length(data$y)
#	vis <- list(type="pv.Label",
#			parameters=collapse(
#					pv.parameter("data", value=data),
#					pv.parameter("bottom", data=data, field="bottom", value=bottom, range.min=y.padding, range.max=wv$height-y.padding, scale=scale),
#					#pv.parameter("bottom", data=data, field=y.name, scale.min=ymin, scale.max=ymax, range.min=y.padding, range.max=wv$height-y.padding, scale=scale),
#					pv.parameter("left", data=data, field=x.name, scale.min=xmin, scale.max=xmax, value=left, range.min=x.padding, range.max=wv$width-x.padding, scale=scale),
#					pv.parameter("text", value=text),
#					pv.parameter("font", value=font),
#					pv.parameter("text.style", value=text.style),
#					";"),
#			anchor=anchor)
#	vis
#}


