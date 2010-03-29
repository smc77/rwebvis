#
# webvis: An R package to create web visualizations.
# author: Shane Conway <shane.conway@gmail.com>
#
# This is released under a BSD license.
#
# Documentation was created using roxygen:
# roxygenize('webvis', roxygen.dir='webvis', copy.package=FALSE, unlink.target=FALSE)
#
###############################################################################

`+.webvis` <- function (parent, child) {
	# check that the parent is a "webvis" object; if not, use normal + operation
	i <- length(parent$branch)
	parent$branch[[i+1]] <- child
	parent
}

new.webvis <- function(name="vis", root=pv.panel(width=width, height=height, ...), description=NULL, width=300, height=200, dataset=NULL, ...) {
	wv <- list(name="vis",
		description=NULL, 
		width=width,
		height=height,
		data=dataset,
		root=root,
		branch=list())
    class(wv) <- "webvis"
	return(wv)
}

webvisToHTML <- function(wv, div.id="id", html.wrap=TRUE, title="", protovis.path=PROTOVIS.PATH) {
	c(getHead(title=title, protovis.path=protovis.path), 
			paste("<center><div id='", div.id, "'>", sep=""), 
		"<script type='text/javascript+protovis'>",
		as.character(wv),
		"</script></div></center>", getTail())
}

pv.parameter <- function(name, default, data, field, value, range.min, range.max, scale.min, scale.max, scale=NA) {
	if(!missing(data) && !missing(field)) {
		if(field.exists(field=field, data=data))
			return(collapse(".", name, "(function(d) ", if(!is.na(scale)) collapse("pv.Scale.", scale, "(", if(missing(scale.min)) min(data[,field]) else scale.min, ", ", if(missing(scale.max)) max(data[,field]) else scale.max, ").range(", range.min, ",", range.max, ")") else "", "(d.", field, ")", ")"))
	} 
	if(!missing(value)) {
		return(paste(".", name, "(", pv.data(value), ")")) 
	}  
	if(!missing(default)) {
		paste(".", name, "(", pv.data(default), ")") 
	} else {
		""
	}
}

pv.param <- function(name, data=NULL, data.name=NULL, value=NULL, scale=NULL, range.min=NULL, range.max=NULL, scale.min=NULL, scale.max=NULL, default=NULL, quote=TRUE) {
	if(missing(name)) stop("'name' is a required field for a webvis.param")
	param <- list(name=name, data=data, data.name=data.name, value=value, scale=scale, range.min=range.min, range.max=range.max, scale.min=scale.min, scale.max=scale.max, quote=quote)
	class(param) <- "webvis.param"
	param
}

pv.scale <- function(type, width, height, data=NULL, data.name=NULL, range.min=NULL, range.max=NULL, scale.min=NULL, scale.max=NULL) {
	type <- unlist(strsplit(type, ".", fixed=TRUE))
	if(length(type) != 3) stop("scale type must be of format type.datarange.scale.range (e.g. linear.y.y)")
	if(is.null(data.name)) data.name <- type[2]
	if(is.null(range.max)) range.max <- if(type[3] == "y") height else width
	if(is.null(range.min)) range.min <- 0
	type <- type[1]
	collapse("pv.Scale.", type, "(", 
			if(is.null(scale.min)) min(data[,data.name]) else scale.min, ", ", 
			if(is.null(scale.max)) max(data[,data.name]) else scale.max, 
			").range(", range.min, ",", range.max, ")")
}

pv.parse <- function(param, wv, data) {
	if(!class(param) == "webvis.param") stop(paste("Function pv.parse expects a webvis.param input but received", class(param), "instead"))
	if(!missing(data) && !field.exists("x", data)) 
		data$x <- 1:length(data$y)
	if(!is.null(param$value)) if(param$value == "d") param$value <- data
	if(is.null(param$data) && !missing(data)) param$data <- data
	if(!is.null(param$data) && field.exists(field=param$data.name, data=param$data)) { 
		return(collapse(".", param$name, "(function(d) ", 
				if(!is.null(param$scale)) pv.scale(type=param$scale, width=wv$width, height=wv$height, data=param$data, range.min=param$range.min, range.max=param$range.max, scale.min=param$scale.min, scale.max=param$scale.max) else "", 
					"(d.", param$data.name, ")", ")"))
	} else if(!is.null(param$value)) {
		return(collapse(".", param$name, "(", pv.data(param$value, quote=param$quote), ")")) 
	}  
	if(!is.null(param$default)) {
		collapse(".", param$name, "(", pv.data(param$default), ")") 
	} else {
		collapse(".", param$name)
	}
}

pv.panel <- function(data, width=300, height=200, left, right, bottom, top) {
	vis <- list(type="pv.Panel",
			parameters=collapse(
				pv.parameter("data", value=data),
				pv.parameter("width", value=width),
				pv.parameter("height", value=height),
				pv.parameter("left", value=left),
				pv.parameter("right", value=right),
				pv.parameter("bottom", value=bottom),
				pv.parameter("top", value=top)
	))
	vis
}
#pv.panel()

#' Add a dataset as a variable to the visualization.
#'
#' \code{pv.dataset} Add a dataset as a variable to the visualization.
#'
#' @param data The dataset to be used in the graphic.
#' @param name
#' @return A string of the relevant javascript.
#' @keywords graphics
#' @author Shane Conway \email{shane.conway@@gmail.com}
#' @references
#' \url{http://vis.stanford.edu/protovis/}
#' @examples
#' pv.dataset(data=wheat, name="wheat")
pv.dataset <- function(data, name) {
	paste("var", name, "=", pv.data(data))
}

#' Generic function for all Protovis mark types.
#'
#' \code{pv.mark} Generic function for all Protovis mark types.
#'
#' @param wv A webvis object
#' @param type Can be "Line", "Bar", etc. (see Protovis API)
#' @param data A dataset for plotting.
#' @param ... Any number of pv.param objects.
#' @param anchor If anchoring to another object.
#' @return A webvis object.
#' @keywords graphics
#' @author Shane Conway \email{shane.conway@@gmail.com}
#' @references
#' \url{http://vis.stanford.edu/protovis/}
#' @examples
#' pv.mark(wv=new.webvis(), type="Line", data=data.frame(y=1:5), 
#' 		pv.param(name="data", value=data), 
#' 		pv.param(name="bottom", data.name="y", scale="linear.y.y"))
pv.mark <- function(wv, type, data, ..., anchor=NULL) {
	vis <- list(type=collapse("pv.", type),
			parameters=collapse(
					unlist(lapply(list(...), function(x, data, wv) {
										if(!is.null(x$data) && is.null(x$data.name)) pv.parse(x, wv=wv) else pv.parse(x, wv=wv, data=data)
									}, data=data, wv=wv)),";"),
			anchor=anchor)
	vis
}

#
#pv.parameter("lineWidth", data=data.frame(y=c(1, 2, 1.5, 3, 1.2), width=1:5), field="width", scale=NA)
#
#plot.webvis(data=c(1, 2, 1.5, 3, 1.2), "line", interpolate="step-after", line.width=5)
#plot.webvis(data=data.frame(y=c(1, 2, 1.5, 3, 1.2), width=1:5), "line", interpolate="step-after")

#' Add a line to the visualization.
#'
#' \code{pv.line} Adds a line plot to the visualization
#'
#' @param wv The webvis object containing the visualization. 
#' @param data The webvis object containing the visualization. 
#' @param bottom The width of the panel in pixels.
#' @param top The width of the panel in pixels.
#' @param left The width of the panel in pixels.
#' @param right The width of the panel in pixels.
#' @param line.width The width of the panel in pixels.
#' @param stroke.style The width of the panel in pixels.
#' @param fill.style The width of the panel in pixels.
#' @param interpolate The width of the panel in pixels.
#' @param spacing The width of the panel in pixels. 
#' @param equal.spacing The width of the panel in pixels.
#' @return A wv object.
#' @keywords graphics
#' @author Shane Conway \email{shane.conway@@gmail.com}
#' @references
#' \url{http://vis.stanford.edu/protovis/}
#' @seealso \code{\link{new.webvis}} that creates the webvis object.
# @examples
#' 
pv.line <- function(wv, data, bottom, bottom.name, bottom.scale, top, top.name, top.scale, left, left.name, left.scale, right, right.name, right.scale, line.width, stroke.style, segmented=(!missing(line.width) || field.exists(field="width", data=data)), fill.style, interpolate, x.padding=(wv$width)/50, y.padding=(wv$height)/50, xmin, xmax, ymin, ymax, scale="linear", anchor=NULL, ...) {
	if(!missing(data) && !field.exists("x", data))
		data$x <- 1:length(data$y)
	vis <- list(type="pv.Line",
			parameters=collapse(
				pv.parameter("data", value=data),
				pv.parameter("bottom", data=data, field="y", value=bottom, scale.min=ymin, scale.max=ymax, range.min=y.padding, range.max=wv$height-y.padding, scale=scale),
				pv.parameter("left", data=data, field="x", value=left, scale.min=xmin, scale.max=xmax, range.min=x.padding, range.max=wv$width-x.padding, scale=scale),
				pv.parameter("lineWidth", data=data, field="line.width", value=line.width, scale=NA),
				pv.parameter("strokeStyle", value=stroke.style),
				pv.parameter("fillStyle", value=fill.style),
				pv.parameter("interpolate", value=interpolate),
				pv.parameter("segmented", value=segmented),
				";"),
		anchor=anchor)
	vis
}

pv.bar <- function(wv, data, y.name="y", x.name="x", bottom=0, height, left, right, bar.width, line.width, stroke.style, segmented=(!missing(line.width) || field.exists(field="width", data=data)), fill.style, x.padding=(wv$width)/50, y.padding=(wv$height)/50, xmin, xmax, ymin=y.padding, ymax, scale="linear", anchor=NULL) {
	if(!missing(data) && !field.exists("x", data))
		data$x <- 1:length(data$y)
	if(!missing(data) && missing(bar.width)) bar.width <- (((wv$width)/nrow(data))-(x.padding))
	vis <- list(type="pv.Bar",
			parameters=collapse(
					pv.parameter("data", value=data),
					pv.parameter("bottom", data=data, field="bottom", value=bottom, range.min=y.padding, range.max=wv$height-y.padding, scale=scale),
					pv.parameter("height", data=data, field=y.name, range.min=ymin, range.max=wv$height-y.padding, scale=scale),
					pv.parameter("left", data=data, field=x.name, value=left, range.min=x.padding, range.max=wv$width-bar.width, scale=scale),
					pv.parameter("width", data=data, field="bar.width", value=bar.width, scale=NA),
					pv.parameter("lineWidth", data=data, field="line.width", value=line.width, scale=NA),
					pv.parameter("strokeStyle", value=stroke.style),
					pv.parameter("fillStyle", value=fill.style),
					pv.parameter("segmented", value=segmented),
					";"),
			anchor=anchor)
	vis
}
#
#pv.bar()
#
#plot.webvis(data=c(1, 2, 1.5, 3, 1.2), "line")

#
#plot.webvis(data=c(1, 2, 1.5, 3, 1.2), "area")

pv.area <- function(wv, data, y.name="y", x.name="x", bottom=0, height, left, right, bar.width, line.width, stroke.style, segmented=(!missing(line.width) || field.exists(field="width", data=data)), interpolate, fill.style, x.padding=(wv$width)/50, y.padding=(wv$height)/50, xmin, xmax, ymin, ymax, scale="linear", anchor=NULL) {
	if(!missing(data) && !field.exists("x", data))
		data$x <- 1:length(data$y)
	vis <- list(type="pv.Area",
			parameters=collapse(
					pv.parameter("data", value=data),
					pv.parameter("bottom", data=data, field="bottom", value=bottom, range.min=y.padding, range.max=wv$height-y.padding, scale=scale),
					pv.parameter("height", data=data, field=y.name, scale.min=ymin, scale.max=ymax, range.min=y.padding, range.max=wv$height-y.padding, scale=scale),
					pv.parameter("left", data=data, field=x.name, scale.min=xmin, scale.max=xmax, value=left, range.min=x.padding, range.max=wv$width-x.padding, scale=scale),
					pv.parameter("lineWidth", data=data, field="line.width", value=line.width, scale=NA),
					pv.parameter("strokeStyle", value=stroke.style),
					pv.parameter("fillStyle", value=fill.style),
					pv.parameter("interpolate", value=interpolate),
					pv.parameter("segmented", value=segmented),
					";"),
			anchor=anchor)
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
pv.wedge <- function(wv, data, bottom, left, right, inner.radius, outer.radius, fill.style, angle, equal.spacing=TRUE, anchor=NULL) {
	data$y <- (data$y/sum(data$y)) * 2 * pi
	vis <- list(type="pv.Wedge",
			parameters=collapse(
					pv.parameter("data", value=data),
					pv.parameter("bottom", value=bottom, default=wv$height/2),
					pv.parameter("left", value=left, default=wv$width/2),
					pv.parameter("innerRadius", value=inner.radius),
					pv.parameter("outerRadius", value=outer.radius, default=min(wv$width,wv$height)/2),
					pv.parameter("fillStyle", value=fill.style),
					pv.parameter("angle", value=angle, data=data, field="y"),
					";"),
			anchor=anchor)
	vis
}


pv.dot <- function(wv, data, bottom=(wv$height/2), top, left=(wv$width/2), right, size, shape, stroke.style, fill.style) {
	multiplier <- (wv$height / max(data)) - 5
	interval <- ((wv$width-30) / length(data))
	vis <- list(type="pv.Dot",
			parameters=paste(if(!missing(data) || length(data)) paste(".data(", pv.data(data), ")") else "",
			if(!missing(bottom) || length(bottom)) paste(".bottom(function(d) d * ", multiplier, ")"),
			if(!missing(left) || length(left)) paste(".left(function() this.index * ", interval, " + 15)"),
			if(!missing(size)) paste(".size(function() this.index * ", interval, " + 15)") else "",
			if(!missing(shape)) paste(".shape(", shape, "") else "",
			";", sep=""),
	anchor=anchor)
	vis
}

pv.rule <- function(wv, data, y.name, x.name, bottom, height, left, right, bar.width, line.width, stroke.style, segmented=(!missing(line.width) || field.exists(field="width", data=data)), interpolate, fill.style, x.padding=(wv$width)/50, y.padding=(wv$height)/50, xmin, xmax, ymin, ymax, scale="linear", anchor=NULL) {
	vis <- list(type="pv.Rule",
			parameters=collapse(
					pv.parameter("data", value=data),
					pv.parameter("bottom", data=data, field=y.name, scale.min=ymin, scale.max=ymax, value=bottom, range.min=y.padding, range.max=wv$height-y.padding, scale=scale),
					pv.parameter("left", data=data, field=x.name, scale.min=xmin, scale.max=xmax, value=left, range.min=x.padding, range.max=wv$width-x.padding, scale=scale),
					pv.parameter("strokeStyle", value=stroke.style),
					pv.parameter("fillStyle", value=fill.style),
					pv.parameter("interpolate", value=interpolate),
					";"),
			anchor=anchor)
	vis
}
#
#wv <- new.webvis(width=150, height=150)
#pw <- new.webvis(root=pv.bar(wv, data=data.frame(y=c(1, 2, 1.5, 3, 1.2)))) + pv.label(text="function(d) d.y")
#wv <- wv + pw
#render.webvis(wv=wv)

pv.label <- function(wv, data, y.name="y", x.name="x", bottom, height, left, right, width, text, font, textAlign, textBaseline, textMargin, textAngle, text.style, anchor=NULL) {
	if(!missing(data) && !field.exists("x", data))
		data$x <- 1:length(data$y)
	vis <- list(type="pv.Label",
			parameters=collapse(
					pv.parameter("data", value=data),
					#pv.parameter("bottom", data=data, field="bottom", value=bottom, range.min=y.padding, range.max=wv$height-y.padding, scale=scale),
					pv.parameter("bottom", data=data, field=y.name, scale.min=ymin, scale.max=ymax, range.min=y.padding, range.max=wv$height-y.padding, scale=scale),
					pv.parameter("left", data=data, field=x.name, scale.min=xmin, scale.max=xmax, value=left, range.min=x.padding, range.max=wv$width-x.padding, scale=scale),
					pv.parameter("text", value=text),
					pv.parameter("font", value=font),
					pv.parameter("text.style", value=text.style),
					";"),
			anchor=anchor)
	vis
}


#' Add a panel to the visualization.
#'
#' \code{protovis.data} Adds a panel to the visualization
#'
#' @param data The webvis object containing the visualization. 
#' @return A wv object.
#' @keywords graphics
#' @author Shane Conway \email{shane.conway@@gmail.com}
#' @references
#' \url{http://vis.stanford.edu/protovis/}
#' @seealso \code{\link{new.webvis}} that creates the webvis object.
# @examples
#' 
pv.data <- function(data, quote=TRUE) {
	if(is.character(data) && length(data)==1) if(quote) return(collapse("'", data, "'")) else data
	if(is.numeric(data) && length(data)==1) return(data)
	if(is.logical(data) && length(data)==1) return(tolower(as.character(data)))
	if(is.vector(data)) {
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
#' @param file.name The file path to the HTML file. 
#' @param title The file path to the HTML file. 
#' @param protovis.path The file path to the HTML file.
#' @return A wv object.
#' @keywords graphics
#' @author Shane Conway \email{shane.conway@@gmail.com}
#' @references
#' \url{http://vis.stanford.edu/protovis/}
#' @seealso \code{\link{new.webvis}} that creates the webvis object.
# @examples
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
# @examples
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
	if(type!="pie") {
		wv <- wv + pv.rule(wv, bottom=0)
		wv <- wv + pv.rule(wv, left=0)
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


#
##
#wv <- new.webvis()
#wv <- wv + pv.bar(wv, data=data.frame(y=c(1, 2, 1.5, 3, 1.2)))
#render.webvis(wv=wv)
#
#wv <- new.webvis()
#line <- new.webvis(root=pv.line(wv, data=data.frame(y=c(1, 2, 1.5, 3, 1.2))))
#line <- line + pv.label(line, text.style="white", anchor="top") 
#wv <- wv + line
#wv <- wv + pv.rule(wv, bottom=0)
#wv <- wv + pv.rule(wv, left=0)
#render.webvis(wv=wv)

#plot.webvis(data=c(1, 2, 1.5, 3, 1.2))
#plot.webvis(data=c(1, 2, 1.5, 3, 1.2), "area")
#plot.webvis(data=c(1, 2, 1.5, 3, 1.2), "pie")

#plot.webvis(data=c(1, 2, 1.5, 3, 1.2), "area")