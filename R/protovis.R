#
# webvis: An R package to create web visualizations.
# author: Shane Conway <shane.conway@gmail.com>
#
# This is released under a GPL license.
#
# Any reproduction of this code must include attribution to the NY Times.
#
#
# setwd("C:/Programming/src/R/")
# library(roxygen)
# roxygenize('webvis',
#		roxygen.dir='rwebvis',
#		copy.package=FALSE,
#		unlink.target=FALSE)
#
###############################################################################

new.webvis <- function() {
	wv <- list(panel=NULL,
		width=NULL, 
		height=NULL,
		vis=list())
	return(wv)
}

getTail <- function() {
	t <- "</body></html>"
	return(t)
}

getHead <- function(title="", protovis.path="../protovis-r3.1.js") {
	h <- paste("<html>
  <head>
    <title>", title, "</title>
    <link type='text/css' rel='stylesheet' href='ex.css?3.1'/>
    <script type='text/javascript' src='", protovis.path, "'></script>
  </head>
  <body>")
	return(h)
}

webvisToHTML <- function(wv, div.id="id", html.wrap=TRUE, title="", protovis.path="../protovis-r3.1.js") {
	c(getHead(title=title, protovis.path=protovis.path), 
			paste("<center><div id='", div.id, "'>", sep=""), 
		"<script type='text/javascript+protovis'>",
		as.character(wv),
		"</script></div></center>", getTail())
}

add.line <- function(wv, data, bottom, top, left, right, line.width, stroke.style, fill.style, interpolate, equal.spacing=TRUE) {
	if(!("x" %in% colnames(data)))
		data$x <- 1:length(data$y)
	vis <- paste("vis.add(pv.Line)",
			if(!missing(data)) paste(".data(", protovis.data(data), ")") else "",
			if(!missing(bottom)) paste(".bottom(", bottom, ")") else paste(".bottom(function(d) d.y", if(equal.spacing) paste("*", (wv$height / max(data$y)) - 5), ")"),
			if(!missing(left)) paste(".left(", left, ")") else paste(".left(function(d) d.x", if(equal.spacing) paste("*", (wv$width / max(data$x)) - 5), ")"),
			if(!missing(line.width)) paste(".lineWidth(", line.width, ")") else "",
			if(!missing(stroke.style)) paste(".strokeStyle(", stroke.style, ")") else "", 
			if(!missing(fill.style)) paste(".fillStyle(", fill.style, ")") else "", 
			if(!missing(interpolate)) paste(".interpolate(", interpolate, ")") else "", 
			";", sep="")
	wv$vis <- c(wv$vis, vis)
	wv
}

add.bar <- function(wv, data, bottom, height, top, left, right, width, stroke.style, fill.style, interpolate, equal.spacing=TRUE) {
	if(!("x" %in% colnames(data)))
		data$x <- 1:length(data$y)
	vis <- paste("vis.add(pv.Bar)",
			if(!missing(data)) paste(".data(", protovis.data(data), ")") else "",
			if(!missing(bottom)) paste(".bottom(", bottom, ")") else if(("bottom" %in% colnames(data))) paste(".bottom(function(d) d.bottom", if(equal.spacing) paste("*", (wv$height / max(data$y)) - 5), ")") else paste(".bottom(0)"),
			if(!missing(height)) paste(".height(", height, ")") else paste(".height(function(d) d.y", if(equal.spacing) paste("*", (wv$height / max(data$y)) - 5), ")"),
			if(!missing(left)) paste(".left(", left, ")") else paste(".left(function(d) d.x", if(equal.spacing) paste("*", (wv$width / max(data$x)) - 15), ")"),
			if(!missing(width)) paste(".width(", width, ")") else paste(".width(", ((wv$width)/nrow(data))-25, ")"),
			if(!missing(stroke.style)) paste(".strokeStyle(", stroke.style, ")") else "", 
			if(!missing(fill.style)) paste(".fillStyle(", fill.style, ")") else "", 
			if(!missing(interpolate)) paste(".interpolate(", interpolate, ")") else "", 
			";", sep="")
	wv$vis <- c(wv$vis, vis)
	wv
}

add.area <- function(wv, data, bottom, height, left, right, line.width, stroke.style, fill.style, interpolate, equal.spacing=TRUE) {
	if(!("x" %in% colnames(data)))
		data$x <- 1:length(data$y)
	vis <- paste("vis.add(pv.Area)",
			if(!missing(data)) paste(".data(", protovis.data(data), ")") else "",
			if(!missing(bottom)) paste(".bottom(", bottom, ")") else paste(".bottom(0)"),
			if(!missing(height)) paste(".height(", height, ")") else paste(".height(function(d) d.y", if(equal.spacing) paste("*", (wv$height / max(data$y)) - 5), ")"),
			if(!missing(left)) paste(".left(", left, ")") else paste(".left(function(d) d.x", if(equal.spacing) paste("*", (wv$width / max(data$x)) - 5), ")"),
			if(!missing(line.width)) paste(".lineWidth(", line.width, ")") else "",
			if(!missing(stroke.style)) paste(".strokeStyle(", stroke.style, ")") else "", 
			if(!missing(fill.style)) paste(".fillStyle(", fill.style, ")") else "", 
			if(!missing(interpolate)) paste(".interpolate(", interpolate, ")") else "", 
			";", sep="")
	wv$vis <- c(wv$vis, vis)
	wv
}

add.wedge <- function(wv, data, bottom=(wv$height/2), top, left=(wv$width/2), right, inner.radius, outer.radius=(0.4 * min(wv$width, wv$height)), angle, start.angle, end.angle, stroke.style) {
	print(data)
	vis <- paste("vis.add(pv.Wedge)",
			if(!missing(data)) paste(".data(pv.normalize(", protovis.data(data), "))") else "",
			if(!missing(bottom) || length(bottom)) paste(".bottom(", bottom, ")") else "1",
			if(!missing(left) || length(left)) paste(".left(", left, ")") else "2",
			if(!missing(inner.radius) || length(inner.radius)) paste(".innerRadius(", inner.radius, ")") else "",
			if(!missing(outer.radius) || length(outer.radius)) paste(".outerRadius(", outer.radius, ")") else "",
			if(!missing(angle)) paste(".angle(", angle, ")", sep="") else ".angle(function(d) d * 2 * Math.PI)",
			";", sep="")
	wv$vis <- c(wv$vis, vis)
	wv
}

add.wedge <- function(wv, data, bottom, left, right, inner.radius, outer.radius, fill.style, angle, equal.spacing=TRUE) {
	vis <- paste("vis.add(pv.Wedge)",
			if(!missing(data)) paste(".data(pv.normalize(", protovis.data(data), "))") else "",
			if(!missing(bottom)) paste(".bottom(", bottom, ")") else paste(".bottom(", wv$height/2, ")"),
			if(!missing(left)) paste(".left(", left, ")") else paste(".left(", wv$width/2, ")"),
			if(!missing(inner.radius)) paste(".innerRadius(", inner.radius, ")") else "",
			if(!missing(outer.radius)) paste(".outerRadius(", outer.radius, ")") else paste(".outerRadius(", min(wv$width,wv$height)/2, ")"),
			if(!missing(fill.style)) paste(".fillStyle(", fill.style, ")") else "", 
			if(!missing(angle)) paste(".angle(", angle, ")", sep="") else ".angle(function(d) d * 2 * Math.PI)",
			";", sep="")
	wv$vis <- c(wv$vis, vis)
	wv
}

add.dot <- function(wv, data, bottom=(wv$height/2), top, left=(wv$width/2), right, size, shape, stroke.style, fill.style) {
	multiplier <- (wv$height / max(data)) - 5
	interval <- ((wv$width-30) / length(data))
	vis <- paste("vis.add(pv.Dot)",
			if(!missing(data) || length(data)) paste(".data(", protovis.data(data), ")") else "",
			if(!missing(bottom) || length(bottom)) paste(".bottom(function(d) d * ", multiplier, ")"),
			if(!missing(left) || length(left)) paste(".left(function() this.index * ", interval, " + 15)"),
			if(!missing(size)) paste(".size(function() this.index * ", interval, " + 15)") else "",
			if(!missing(shape)) paste(".shape(", shape, "") else "",
			";", sep="")
	wv$vis <- c(wv$vis, vis)
	wv
}

protovis.data <- function(data) {
	if(class(data) %in% c("character", "numeric", "vector") || ncol(data) == 1) {
		data <- paste("[", 
				paste(as.matrix(data), collapse=", "), 
				"]", sep="")		
	} else if(class(data) %in% c("data.frame")) {
		data <- paste("[", paste(lapply(1:nrow(data), function(i) { k <- data[i,]; nm <- colnames(data); paste("{", paste(paste(nm, ":", k), collapse=", "), "}") }), collapse=","), "]")
	}
	return(data)
}

#' Create the final visualization from the webvis object.
#'
#' \code{render.webvis} Renders the visualization from the webvis object.
#'
#' @param wv The webvis object containing the visualization. 
#' @param vis.name The file name of the output HTML.
#' @param path The file path to the HTML file. 
#' @return A wv object.
#' @keywords graphics
#' @author Shane Conway \email{shane.conway@@gmail.com}
#' @references
#' \url{http://vis.stanford.edu/protovis/}
#' @seealso \code{\link{new.webvis}} that creates the webvis object.
#' @examples
#' 
render.webvis <- function(wv, vis.name="demo", path="c:\\temp\\protovis-3.1\\examples\\", file.name=paste(path, vis.name, ".html", sep=""), con=file(file.name, "w"), title="", protovis.path="../protovis-r3.1.js") {
	wv <- c(wv, render="vis.render();")
	#check.webvis(wv)
	writeLines(webvisToHTML(c(wv$panel, wv$vis, wv$render), title=title, protovis.path=protovis.path), con=con)
	close(con)
	browseURL(url=file.name)
}

#' Add a panel to the visualization.
#'
#' \code{new.panel} Adds a panel to the visualization
#'
#' @param wv The webvis object containing the visualization. 
#' @param width The width of the panel in pixels.
#' @param height The width of the panel in pixels. 
#' @return A wv object.
#' @keywords graphics
#' @author Shane Conway \email{shane.conway@@gmail.com}
#' @references
#' \url{http://vis.stanford.edu/protovis/}
#' @seealso \code{\link{new.webvis}} that creates the webvis object.
#' @examples
#' 
new.panel <- function(wv, width=500, height=500) {
	panel <- paste("var vis = new pv.Panel()",
		paste(".width(",width,")"),
		paste(".height(",height,");"), sep="\n")
	wv$panel=panel
	wv$width=width
	wv$height=height
	return(wv)
}


plot.webvis <- function(data, type="bar", width=500, height=500, ...) {
	if(!(class(data) == "data.frame") && is.vector(data))
		data <- data.frame(y=data)
	wv <- new.webvis()
	wv <- new.panel(wv, width=width, height=height)
	wv <- if(type=="bar") {
		add.bar(wv=wv, data=data, ...)
	} else if(type=="line") {
		add.line(wv=wv, data=data, ...)
	} else if(type=="dot") {
		add.dot(wv=wv, data=data, ...)
	} else if(type=="pie") {
		add.wedge(wv=wv, data=data, ...)
	} else if(type=="area") {
		add.area(wv=wv, data=data, ...)
	}
	render.webvis(wv)
}

#' Provides web graphics for R by wrapping visualization API's.  Currently supports part of Protovis.
#'
#' \tabular{ll}{
#' Package: \tab webvis\cr
#' Type: \tab Package\cr
#' Version: \tab 0.1\cr
#' Date: \tab 2010-03-22\cr
#' License: \tab BSD (>= 2)\cr
#' LazyLoad: \tab no\cr
#' }
#'
#' Uses Protovis to provide web graphics for R.
#' 
#' @name webvis-package
#' @aliases webvis
#' @docType package
#' @title Web graphics for R.
#' @author Shane Conway \email{shane.conway@@gmail.com}
#' @references
#' \url{http://vis.stanford.edu/protovis}
#' @keywords package
NULL


plot.webvis(data=c(1, 2, 1.5, 3, 1.2))
plot.webvis(data=c(1, 2, 1.5, 3, 1.2), "area")
plot.webvis(data=c(1, 2, 1.5, 3, 1.2), "pie")