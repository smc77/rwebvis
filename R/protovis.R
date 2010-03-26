#
# webvis: An R package to create web visualizations.
# author: Shane Conway <shane.conway@gmail.com>
#
# This is released under a BSD license.
#
#
# Documentation was created using roxygen:
# roxygenize('webvis', roxygen.dir='rwebvis', copy.package=FALSE, unlink.target=FALSE)
#
###############################################################################

`+.webvis` <- function (parent, child) {
	# check that the parent is a "webvis" object; if not, use normal + operation
	i <- length(parent$branch)
	parent$branch[[i+1]] <- child
	parent
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
new.webvis <- function(name="vis", root=pv.panel(width=width, height=height, ...), description=NULL, width=300, height=200, ...) {
	wv <- list(name="vis",
		description=NULL, 
		width=width,
		height=height,
		root=root,
		branch=list())
    class(wv) <- "webvis"
	return(wv)
}

getTail <- function() {
	t <- "</body></html>"
	return(t)
}

getHead <- function(title="", protovis.path=PROTOVIS.PATH) {
	h <- paste("<html>
  <head>
    <title>", title, "</title>
    <link type='text/css' rel='stylesheet' href='ex.css?3.1'/>
    <script type='text/javascript' src='", protovis.path, "'></script>
  </head>
  <body>")
	return(h)
}

webvisToHTML <- function(wv, div.id="id", html.wrap=TRUE, title="", protovis.path=PROTOVIS.PATH) {
	c(getHead(title=title, protovis.path=protovis.path), 
			paste("<center><div id='", div.id, "'>", sep=""), 
		"<script type='text/javascript+protovis'>",
		as.character(wv),
		"</script></div></center>", getTail())
}

pv.panel <- function(width=300, height=200, data) {
	vis <- list(type="pv.Panel",
			parameters=paste(paste(".width(",width,")"),
			paste(".height(",height,");"), sep="\n"))
	vis
}

#' Add a line to the visualization.
#'
#' \code{add line} Adds a line plot to the visualization
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
pv.line <- function(wv, data, bottom, top, left, right, line.width, stroke.style, fill.style, interpolate, spacing=10, equal.spacing=TRUE) {
	if(!("x" %in% colnames(data)))
		data$x <- 1:length(data$y)
	if(equal.spacing) width <- ((wv$width)/nrow(data)-spacing) else if(missing(width)) width <- 10
	vis <- list(type="pv.Line",
			parameters=paste(if(!missing(data)) paste(".data(", protovis.data(data), ")") else "",
			if(!missing(bottom)) paste(".bottom(", bottom, ")") else paste(".bottom(function(d) d.y", if(equal.spacing) paste("*", (wv$height / max(data$y)) - 5), ")"),
			if(!missing(left)) paste(".left(", left, ")") else paste(".left(function(d) (d.x", if(equal.spacing) paste("*", (wv$width / max(data$x)), ")-", width) else ")", ")"),
			if(!missing(line.width)) paste(".lineWidth(", line.width, ")") else "",
			if(!missing(stroke.style)) paste(".strokeStyle(", stroke.style, ")") else "", 
			if(!missing(fill.style)) paste(".fillStyle(", fill.style, ")") else "", 
			if(!missing(interpolate)) paste(".interpolate(", interpolate, ")") else "", 
			";", sep=""))
	vis
}

pv.bar <- function(wv, data, bottom, height, top, left, right, width, stroke.style, fill.style, interpolate, spacing=1, equal.spacing=TRUE) {
	if(!("x" %in% colnames(data)))
		data$x <- 1:length(data$y)
	if(missing(width) && equal.spacing) width <- ((wv$width)/nrow(data)-spacing) else if(missing(width)) width <- 10
	vis <- list(type="pv.Bar",
			parameters=paste(if(!missing(data)) paste(".data(", protovis.data(data), ")") else "",
			if(!missing(bottom)) paste(".bottom(", bottom, ")") else if(("bottom" %in% colnames(data))) paste(".bottom(function(d) d.bottom", if(equal.spacing) paste("*", (wv$height / max(data$y)) - 5), ")") else paste(".bottom(0)"),
			if(!missing(height)) paste(".height(", height, ")") else paste(".height(function(d) d.y", if(equal.spacing) paste("*", (wv$height / max(data$y)) - 5), ")"),
			if(!missing(left)) paste(".left(", left, ")") else paste(".left(function(d) ", if(equal.spacing) paste("(d.x *", (wv$width / max(data$x)), ") - ",width, ")") else "d.x)"),
			paste(".width(", width, ")"),
			if(!missing(stroke.style)) paste(".strokeStyle(", stroke.style, ")") else "", 
			if(!missing(fill.style)) paste(".fillStyle(", fill.style, ")") else "", 
			if(!missing(interpolate)) paste(".interpolate(", interpolate, ")") else "", 
			";", sep=""))
	vis
}

pv.area <- function(wv, data, bottom, height, left, right, line.width, stroke.style, fill.style, interpolate, equal.spacing=TRUE) {
	if(!("x" %in% colnames(data)))
		data$x <- 1:length(data$y)
	vis <- list(type="pv.Area",
			parameters=paste(if(!missing(data)) paste(".data(pv.normalize(", protovis.data(data), "))") else "",
			if(!missing(bottom)) paste(".bottom(", bottom, ")") else paste(".bottom(0)"),
			if(!missing(height)) paste(".height(", height, ")") else paste(".height(function(d) d.y", if(equal.spacing) paste("*", (wv$height / max(data$y)) - 5), ")"),
			if(!missing(left)) paste(".left(", left, ")") else paste(".left(function(d) d.x", if(equal.spacing) paste("*", (wv$width / max(data$x)) - 5), ")"),
			if(!missing(line.width)) paste(".lineWidth(", line.width, ")") else "",
			if(!missing(stroke.style)) paste(".strokeStyle(", stroke.style, ")") else "", 
			if(!missing(fill.style)) paste(".fillStyle(", fill.style, ")") else "", 
			if(!missing(interpolate)) paste(".interpolate(", interpolate, ")") else "", 
			";", sep=""))
	vis
}

pv.wedge <- function(wv, data, bottom, left, right, inner.radius, outer.radius, fill.style, angle, equal.spacing=TRUE) {
	vis <- list(type="pv.Wedge",
			parameters=paste(if(!missing(data)) paste(".data(pv.normalize(", protovis.data(data), "))") else "",
			if(!missing(bottom)) paste(".bottom(", bottom, ")") else paste(".bottom(", wv$height/2, ")"),
			if(!missing(left)) paste(".left(", left, ")") else paste(".left(", wv$width/2, ")"),
			if(!missing(inner.radius)) paste(".innerRadius(", inner.radius, ")") else "",
			if(!missing(outer.radius)) paste(".outerRadius(", outer.radius, ")") else paste(".outerRadius(", min(wv$width,wv$height)/2, ")"),
			if(!missing(fill.style)) paste(".fillStyle(", fill.style, ")") else "", 
			if(!missing(angle)) paste(".angle(", angle, ")", sep="") else ".angle(function(d) d * 2 * Math.PI)",
			";", sep=""))
	vis
}

pv.dot <- function(wv, data, bottom=(wv$height/2), top, left=(wv$width/2), right, size, shape, stroke.style, fill.style) {
	multiplier <- (wv$height / max(data)) - 5
	interval <- ((wv$width-30) / length(data))
	vis <- list(type="pv.Dot",
			parameters=paste(if(!missing(data) || length(data)) paste(".data(", protovis.data(data), ")") else "",
			if(!missing(bottom) || length(bottom)) paste(".bottom(function(d) d * ", multiplier, ")"),
			if(!missing(left) || length(left)) paste(".left(function() this.index * ", interval, " + 15)"),
			if(!missing(size)) paste(".size(function() this.index * ", interval, " + 15)") else "",
			if(!missing(shape)) paste(".shape(", shape, "") else "",
			";", sep=""))
	vis
}

pv.rule <- function(wv, data, bottom, height, top, left, right, width, stroke.style, fill.style, equal.spacing=TRUE) {
	if(!missing(data))
		if(!("x" %in% colnames(data)))
			data$x <- 1:length(data$y)
	vis <- list(type="pv.Rule",
			parameters=paste(if(!missing(data)) paste(".data(", protovis.data(data), ")") else "",
			if(!missing(bottom)) paste(".bottom(", bottom, ")") else "",
			if(!missing(height)) paste(".height(", height, ")") else "",
			if(!missing(left)) paste(".left(", left, ")") else "",
			if(!missing(width)) paste(".width(", width, ")") else "",
			if(!missing(stroke.style)) paste(".strokeStyle(", stroke.style, ")") else "", 
			if(!missing(fill.style)) paste(".fillStyle(", fill.style, ")") else "", 
			";", sep=""))
	vis
}

pv.label <- function(wv, data, bottom, height, top, left, right, width, text, textAlign, textBaseline, textMargin, textAngle, text.style, equal.spacing=TRUE, anchor=NULL) {
	if(!missing(data))
		if(!("x" %in% colnames(data)))
			data$x <- 1:length(data$y)
	vis <- list(type="pv.Label",
			parameters=paste(if(!missing(data)) paste(".data(", protovis.data(data), ")") else "",
			if(!missing(bottom)) paste(".bottom(", bottom, ")") else "", 
			if(!missing(height)) paste(".height(", height, ")") else "",
			if(!missing(left)) paste(".left(", left, ")") else "",
			if(!missing(width)) paste(".width(", width, ")") else "",
			if(!missing(text)) paste(".text(", text, ")") else paste(".text(function(d) d.y)") , 
			if(!missing(text.style)) paste(".textStyle('", text.style, "')") else "", 
			";", sep=""),
			anchor=anchor)
	vis
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
render.webvis <- function(wv, vis.name="demo", path=OUTPUT.PATH, file.name=paste(path, vis.name, ".html", sep=""), title="", protovis.path=PROTOVIS.PATH) {
	con=file(file.name, "w")
	if(!isOpen(con)) stop("unable to connect to output file")
	#check.webvis(wv)
	wv <- c(wv, render="vis.root.render();")
	writeLines(webvisToHTML(unfold.webvis(wv), title=title, protovis.path=protovis.path), con=con)
	close(con)
	browseURL(url=file.name)
}

unfold.webvis <- function(wv, name="vis", parent=NULL) {
	root <- paste("var", name, "=", if(is.null(parent)) paste("new ", wv$root$type, "()", sep="") else paste(parent, ".add(", wv$root$type, ")", sep=""), wv$root$parameters)
	wv2 <- as.list(wv$branch)
	if(length(wv2)) {
		wv2 <- unlist(lapply(wv2, function(x) {
			x <- if(class(x) == "webvis") {
					unfold.webvis(x, name=paste(name, ceiling(runif(1, 1, 100)), sep=""), parent=name)
				 } else { 
					paste(paste(name, if(!is.null(x$anchor)) paste(".anchor('", x$anchor, "')", sep="") else "", ".add(", x$type, ")", sep=""), x$parameters, sep="") 
				 }
			return(x)
			}))
	}
	wv2 <- unlist(c(root, wv2, wv$render))
	wv2
}

#' Simplified plot function for web vis plots.
#'
#' \code{plot.webvis} Simplified plot function for web vis plots
#'
#' @param data The webvis object containing the visualization. 
#' @param type The type of plot.  Can be "bar", "line", "area", "pie", "dot", or "..."
#' @param width The width of the panel in pixels.
#' @param height The width of the panel in pixels. 
#' @return Opens a plot.
#' @keywords graphics
#' @author Shane Conway \email{shane.conway@@gmail.com}
#' @references
#' \url{http://vis.stanford.edu/protovis/}
#' @seealso \code{\link{new.webvis}} that creates the webvis object.
#' @examples
#' 
plot.webvis <- function(data, type="bar", width=500, height=500, ...) {
	if(!(class(data) == "data.frame") && is.vector(data))
		data <- data.frame(y=data)
	wv <- new.webvis(width=width, height=height)
	wv <- if(type=="bar") {
		wv + pv.bar(wv, data=data, ...)
	} else if(type=="line") {
		wv + pv.line(wv, data=data, ...)
	} else if(type=="dot") {
		wv + pv.dot(wv, data=data, ...)
	} else if(type=="pie") {
		wv + pv.wedge(wv, data=data, ...)
	} else if(type=="area") {
		wv + pv.area(wv, data=data, ...)
	}
	wv <- wv + pv.rule(wv, bottom=0)
	wv <- wv + pv.rule(wv, left=0)
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

.onLoad <- function() {
	PROTOVIS.PATH <- as.character(Sys.getenv("PROTOVIS_PATH"))
	if(PROTOVIS.PATH == "") PROTOVIS.PATH <- "http://protovis-js.googlecode.com/svn/trunk/protovis-d3.1.js"
	OUTPUT.PATH <- as.character(Sys.getenv("WEBVIS_PATH"))
	if(OUTPUT.PATH == "") OUTPUT.PATH <- tempdir()
}

#
wv <- new.webvis()
wv <- wv + pv.bar(wv, data=data.frame(y=c(1, 2, 1.5, 3, 1.2)))
render.webvis(wv=wv)

wv <- new.webvis()
line <- new.webvis(root=pv.line(wv, data=data.frame(y=c(1, 2, 1.5, 3, 1.2))))
line <- line + pv.label(line, text.style="white", anchor="top") 
wv <- wv + line
wv <- wv + pv.rule(wv, bottom=0)
wv <- wv + pv.rule(wv, left=0)
render.webvis(wv=wv)

#
#plot.webvis(data=c(1, 2, 1.5, 3, 1.2))
#plot.webvis(data=c(1, 2, 1.5, 3, 1.2), "area")
#plot.webvis(data=c(1, 2, 1.5, 3, 1.2), "pie")