###############################################################################
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

#' Create a new webvis object to store each layer of the visualization.
#'
#' \code{new.webvis} Create a new webvis object to store each layer of the visualization.
#'
#' @param root The root node of the visualization.
#' @param branch A node layer underneath the root visualization.
#' @param width  The width in pixels.
#' @param height The height in pixels.
#' @param name The name of the visualization.
#' @param description A description of the visualization.
#' @param dataset A dataset associated with the visualization.
#' @return A webvis objectt.
#' @keywords graphics
#' @author Shane Conway \email{shane.conway@@gmail.com}
#' @references
#' \url{http://vis.stanford.edu/protovis/}
#' @examples
#' new.webvis()
new.webvis <- function(name="vis", root=pv.panel(width=width, height=height, ...), description=NULL, width=300, height=200, dataset=NULL, branch=list(), render=NULL, ...) {
	wv <- list(name=name,
		description=description, 
		width=width,
		height=height,
		data=dataset,
		root=root,
		branch=branch,
		render=render)
    class(wv) <- "webvis"
	return(wv)
}

#' Convert webvis to HTML.
#'
#' \code{webvisToHTML} Convert webvis to HTML.
#'
#' @param wv A webvis object.
#' @param div.id The div tag id.
#' @param html.wrap Whether to wrap the visualization in other supplied HTML.
#' @param title The title of the HTML page.
#' @param head The HTML above the webvis.
#' @param tail The HTML below the webvis.
#' @param protovis.path The path to the protovis javascript.
#' @return The HTML output
#' @keywords graphics
#' @author Shane Conway \email{shane.conway@@gmail.com}
#' @references
#' \url{http://vis.stanford.edu/protovis/}
#' @examples
#' webvisToHTML(new.webvis())
#' webvisToHTML(wv=unfold.webvis(new.webvis() + pv.line(data=c(1, 1.2, 1.7, 1.5, .7, .5, .2), bottom.name="y", left.name="x", bottom.scale="linear.y.y", left.scale="linear.x.x", line.width=5, render=FALSE)))
webvisToHTML <- function(wv, div.id="id", html.wrap=TRUE, title=NULL, head=getHead(title=title, protovis.path=protovis.path), tail=getTail(), protovis.path=PROTOVIS.PATH) {
	if(!is.webvis.flat(wv)) stop("webvisToHTML requires a webvis object")
	wv.html <- c(paste("<center><div id='", div.id, "'>", sep=""), 
		"<script type='text/javascript+protovis'>",
		as.character(wv),
		"</script></div></center>")
	if(html.wrap) wv.html <- c(head, wv.html, tail)
	wv.html
}

#' A protovis mark parameter.
#'
#' \code{pv.parameter} A protovis mark parameter.
#'
#' @param name The name of mark parameter.
#' @param data The data used in the parameter settings.
#' @param data.name The name of the field in the dataset.
#' @param value An explicit value for the parameter.
#' @param scale Whether the value or data should be scaled.  Can be "linear", "log", or ...
#' @param range.min The minimum value for the range (or defaults to the minimum from the data).
#' @param range.max The maximum value for the range (or defaults to the maximum from the data.
#' @param scale.min The minimum scaled value (or defaults to zero) in pixels.
#' @param scale.max The maximum scaled value (or defaults to the height/width of the visualization) in pixels.
#' @param default The default value for the parameter.
#' @param quote Whether character values should be quoted.
#' @return A webvis.param object.
#' @keywords graphics
#' @author Shane Conway \email{shane.conway@@gmail.com}
#' @references
#' \url{http://vis.stanford.edu/protovis/}
#' @examples
#' pv.param(name="data", value="d")
pv.param <- function(name, data=NULL, data.name=NULL, value=NULL, scale=NULL, scale.min=NULL, scale.max=NULL, xmin=NULL, xmax=NULL, ymin=NULL, ymax=NULL, default=NULL, quote=TRUE) {
	if(missing(name)) stop("'name' is a required field for a webvis.param")
	param <- list(name=name, data=data, data.name=data.name, value=value, scale=scale, scale.min=scale.min, scale.max=scale.max, xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax, quote=quote)
	class(param) <- "webvis.param"
	param
}

pv.area(data=c(1, 1.2, 1.7, 1.5, .7, .5, .2), height.name="y", left.name="x", height.scale="linear.y.y", left.scale="linear.x.x", ymin=10, bottom=0, render=TRUE)

#' A scaling function for protovis.
#'
#' \code{pv.scale} A scaling function for protovis.
#'
#' @param type The type is a "." separated string which determines whether the scaling is linear, log, or ..., and what parameter should be scaled.
#' @param width The width of the panel.
#' @param height The height of the panel.
#' @param data The data for the panel.
#' @param data.name The name of the variable to be scaled.
#' @param range.min The minimum value for the data to scale.
#' @param range.max The maximum value for the data to scale.
#' @param scale.min The minimum scaled value.
#' @param scale.max The maximum scaled value.
#' @return The HTML output
#' @keywords graphics
#' @author Shane Conway \email{shane.conway@@gmail.com}
#' @references
#' \url{http://vis.stanford.edu/protovis/}
#' @examples
#' pv.scale(type="linear.value.y", width=200, height=200, data=data.frame(value=c(1:5)
pv.scale <- function(type, width, height, data=NULL, data.name=NULL, scale.min=NULL, scale.max=NULL, xmin=NULL, xmax=NULL, ymin=NULL, ymax=NULL) {
	type <- unlist(strsplit(type, ".", fixed=TRUE))
	if(length(type) != 3) stop("scale type must be of format type.datarange.scale.range (e.g. linear.y.y)")
	if(is.null(data.name)) data.name <- type[2]
	range.max <- if(type[3] == "y") { if(is.null(ymax)) height else ymax } else { if(is.null(xmax)) width else xmax }
	range.min <- if(type[3] == "y") { if(is.null(ymin)) 0 else ymin } else { if(is.null(xmin)) 0 else xmin }
	type <- type[1]
	collapse("pv.Scale.", type, "(", 
			if(is.null(scale.min)) min(data[,data.name]) else scale.min, ", ", 
			if(is.null(scale.max)) max(data[,data.name]) else scale.max, 
			").range(", range.min, ",", range.max, ")")
}

#' Takes a parameter and a webvis object and parses them.
#'
#' \code{pv.parse} Takes a parameter and a webvis object and parses them.
#'
#' @param param A webvis param object from pv.param().
#' @param wv A webvis object.
#' @param data A dataset.
#' @return The HTML output
#' @keywords graphics
#' @author Shane Conway \email{shane.conway@@gmail.com}
#' @references
#' \url{http://vis.stanford.edu/protovis/}
#' pv.parse(pv.param(name="text", data.name="y"))
#' pv.parse(pv.param(name="text", data.name="y"), data=data.frame(y=1:5))
pv.parse <- function(param, wv, data) {
	if(!class(param) == "webvis.param") stop(paste("Function pv.parse expects a webvis.param input but received", class(param), "instead"))
	if(esse(data))
		if(!field.exists("x", data)) 
			data$x <- 1:length(data$y)
	if(!is.null(param$value)) if(param$value == "d") param$value <- data
	if(is.null(param$data) && esse(data)) param$data <- data
	if(!is.null(param$data) && field.exists(field=param$data.name, data=param$data)) { 
		return(collapse(".", param$name, "(function(d) ", 
				if(!is.null(param$scale)) pv.scale(type=param$scale, width=wv$width, height=wv$height, data=param$data, scale.min=param$scale.min, scale.max=param$scale.max, xmin=param$xmin, xmax=param$xmax, ymin=param$ymin, ymax=param$ymax) else "", 
					"(d.", param$data.name, ")", ")"))
	} else if(!is.null(param$data.name)) {
		return(collapse(".", param$name, "(function(d) ", 
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

#' A protovis panal.
#'
#' \code{pv.panel} A protovis panal.
#'
#' @param param A webvis param object from pv.param().
#' @param wv A webvis object.
#' @param data A dataset.
#' @return The HTML output
#' @keywords graphics
#' @author Shane Conway \email{shane.conway@@gmail.com}
#' @references
#' \url{http://vis.stanford.edu/protovis/}
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
pv.mark(type="Label", ...=pv.param(name="text", data.name="y"))
pv.parse(pv.param(name="text", data.name="y"), data=data.frame(y=1:5))
pv.mark <- function(wv=NULL, type, data=NULL, ..., anchor=NULL) {
	args <- if(length(list(...)) > 0) { if(is.webvis.param(list(...)[[1]])) list(...) else list(...)[[1]] } else list()
	vis <- list(type=collapse("pv.", type),
			parameters=collapse(
					unlist(lapply(args, function(x, data, wv) {
										#if(!is.null(x$data) && is.null(x$data.name)) pv.parse(x, wv=wv) else 
										pv.parse(param=x, wv=wv, data=data)
									}, data=data, wv=wv)),";"),
			anchor=anchor)
	vis
}

append.param <- function(paramlist, name, value, param.name, param.scale, scale.min=NULL, scale.max=NULL, xmin, xmax, ymin, ymax) {
	if(esse(value)) {
		paramlist[[length(paramlist) + 1]] <- pv.param(name=name, value=if(name=="data") "d" else value) 
	} else if(esse(param.name)) {
		paramlist[[length(paramlist) + 1]] <- pv.param(name=name, data.name=param.name, scale=(if(esse(param.scale)) param.scale else NULL), scale.min=scale.min, scale.max=scale.max, xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax)
	}
	paramlist
}

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
#' @examples
#' pv.line(data=c(1, 1.2, 1.7, 1.5, .7, .5, .2), bottom.name="y", left.name="x")
.pv.chart <- function(type, wv, data=NULL, bottom, bottom.name, bottom.scale, top, top.name, top.scale, left, left.name, left.scale, right, right.name, right.scale, 
		height, height.name, height.scale, width, width.name, width.scale, line.width, line.width.name, line.width.scale, 
		size, size.name, size.scale, shape, shape.name, shape.scale, inner.radius, inner.radius.name, inner.radius.scale, outer.radius, outer.radius.name, outer.radius.scale, 
		angle, angle.name, angle.scale, start.angle, start.angle.name, start.angle.scale, end.angle, end.angle.name, end.angle.scale, 
		text, text.name, text.scale, font, text.style, text.align, text.baseline, text.margin, text.angle,  
		stroke.style, stroke.style.name, stroke.style.scale, fill.style, fill.style.name, fill.style.scale, segmented, interpolate, x.padding, y.padding, xmin=NULL, xmax=NULL, ymin=NULL, ymax=NULL, scale.min=NULL, scale.max=NULL, anchor=NULL, render=FALSE, normalize=FALSE, ...) {
	if(!esse(wv)) { wv <- new.webvis() }
	if(esse(data)) {
		if(is.vector(data))
			data <- data.frame(y=data)
		if(!field.exists("x", data))
			data$x <- 1:length(data[,1])
		if(normalize)
			data[,angle.name] <- (data[,angle.name]/sum(data[,angle.name])) * 2 * pi
	}	
	# build the final parameter list
	paramlist <- list()
	paramlist <- append.param(paramlist=paramlist, name="data", value=data)
	paramlist <- append.param(paramlist=paramlist, name="bottom", value=bottom, param.name=bottom.name, param.scale=bottom.scale, xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax, scale.min=scale.min, scale.max=scale.max)
	paramlist <- append.param(paramlist=paramlist, name="top", value=top, param.name=top.name, param.scale=top.scale, xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax)
	paramlist <- append.param(paramlist=paramlist, name="right", value=right, param.name=right.name, param.scale=right.scale, xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax)
	paramlist <- append.param(paramlist=paramlist, name="left", value=left, param.name=left.name, param.scale=left.scale, xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax)
	paramlist <- append.param(paramlist=paramlist, name="height", value=height, param.name=height.name, param.scale=height.scale, xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax, scale.min=scale.min, scale.max=scale.max)
	paramlist <- append.param(paramlist=paramlist, name="lineWidth", value=line.width, param.name=line.width.name, param.scale=line.width.scale, xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax, scale.max=scale.max)
	paramlist <- append.param(paramlist=paramlist, name="width", value=width, param.name=width.name, param.scale=width.scale, xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax)
	paramlist <- append.param(paramlist=paramlist, name="strokeStyle", value=stroke.style, param.name=stroke.style.name, param.scale=stroke.style.scale, xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax)
	paramlist <- append.param(paramlist=paramlist, name="fillStyle", value=fill.style, param.name=fill.style.name, param.scale=fill.style.scale, xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax)
	paramlist <- append.param(paramlist=paramlist, name="interpolate", value=interpolate)
	paramlist <- append.param(paramlist=paramlist, name="segmented", value=segmented)
	paramlist <- append.param(paramlist=paramlist, name="shape", value=shape, param.name=shape.name, param.scale=shape.scale, xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax)
	paramlist <- append.param(paramlist=paramlist, name="size", value=size, param.name=size.name, param.scale=size.scale, xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax)
	paramlist <- append.param(paramlist=paramlist, name="innerRadius", value=inner.radius, param.name=inner.radius.name, param.scale=inner.radius.scale, xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax)
	paramlist <- append.param(paramlist=paramlist, name="outerRadius", value=outer.radius, param.name=outer.radius.name, param.scale=outer.radius.scale, xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax)
	paramlist <- append.param(paramlist=paramlist, name="angle", value=angle, param.name=angle.name, param.scale=angle.scale, xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax)
	paramlist <- append.param(paramlist=paramlist, name="startAngle", value=start.angle, param.name=start.angle.name, param.scale=start.angle.scale, xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax)
	paramlist <- append.param(paramlist=paramlist, name="endAngle", value=end.angle, param.name=end.angle.name, param.scale=end.angle.scale, xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax)
	paramlist <- append.param(paramlist=paramlist, name="text", value=text, param.name=text.name, param.scale=text.scale, xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax)
	paramlist <- append.param(paramlist=paramlist, name="font", value=font)
	paramlist <- append.param(paramlist=paramlist, name="textStyle", value=text.style)
	paramlist <- append.param(paramlist=paramlist, name="textAlign", value=text.align)
	paramlist <- append.param(paramlist=paramlist, name="textBaseline", value=text.baseline)
	paramlist <- append.param(paramlist=paramlist, name="textMargin", value=text.margin)
	paramlist <- append.param(paramlist=paramlist, name="textAngle", value=text.angle)
	# assemble the mark
	vis <- pv.mark(wv=wv, data=data, type=type, paramlist, anchor=anchor)
	if(render) render.webvis(wv=(wv + vis)) else vis
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
pv.data <- function(data, quote=FALSE) {
	if(data == "null") return(data)
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
render.webvis <- function(wv, vis.name=NULL, file.name=if(!is.null(OUTPUT.PATH)) collapse(OUTPUT.PATH, vis.name, ".html") else collapse(tempfile(), ".html"), title="", protovis.path=PROTOVIS.PATH) {
	con=file(file.name, "w")
	if(!isOpen(con)) stop("unable to connect to output file")
	#check.webvis(wv)
	wv$render <- "vis.root.render();"
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
	class(wv2) <- "webvis.flat"
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
#' plot.webvis(data=c(1, 2, 1.5, 3, 1.2), "line")
#
##
##plot.webvis(data=c(1, 2, 1.5, 3, 1.2), "area")

plot.webvis <- function(data, type="bar", width=500, height=500, ...) {
	if(!(class(data) == "data.frame") && is.vector(data))
		data <- data.frame(y=data)
	wv <- new.webvis(width=width, height=height)
	wv <- if(type=="bar") {
		wv + pv.bar(wv=wv, data=data, ...)
	} else if(type=="line") {
		wv + pv.line(wv=wv, data=data, ...)
	} else if(type=="dot") {
		wv + pv.dot(wv=wv, data=data, ...)
	} else if(type=="pie") {
		wv + pv.wedge(wv=wv, data=data, ...)
	} else if(type=="area") {
		wv + pv.area(wv=wv, data=data, ...)
	}
	if(type!="pie") {
		wv <- wv + pv.rule(wv=wv, bottom=0)
		wv <- wv + pv.rule(wv=wv, left=0)
	}
	render.webvis(wv)
}
# plot.webvis(data=c(1, 2, 1.5, 3, 1.2), "line")
# plot.webvis(data=c(1, 2, 1.5, 3, 1.2), "line", scale.min=0)

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
##line <- line + pv.label(line, text.style="white", anchor="top") 
#wv <- wv + line
#wv <- wv + pv.rule(wv, bottom=0)
#wv <- wv + pv.rule(wv, left=0)
#render.webvis(wv=wv)

#plot.webvis(data=c(1, 2, 1.5, 3, 1.2))
#plot.webvis(data=c(1, 2, 1.5, 3, 1.2), "area")
#plot.webvis(data=c(1, 2, 1.5, 3, 1.2), "line")
#plot.webvis(data=c(1, 2, 1.5, 3, 1.2), "pie")

#plot.webvis(data=c(1, 2, 1.5, 3, 1.2), "area")
#
#
#plot.webvis(data=c(1, 2, 1.5, 3, 1.2), "line", interpolate="step-after", line.width=5)
#plot.webvis(data=data.frame(y=c(1, 2, 1.5, 3, 1.2), width=1:5), "line", interpolate="step-after")


#
#render.webvis(new.webvis() + (new.webvis(root=pv.bar(data=c(1, 1.2, 1.7, 1.5, .7, .5, .2), height.name="y", left.name="x", height.scale="linear.y.y", left.scale="linear.x.x", bottom=0, width=30))+ pv.label(data=1:7, text.name="y", anchor="top", text.style="white")))
#
#wv <- new.webvis(root=pv.panel(width=150, height=150), width=150, height=150)
#render.webvis((wv + 
#		(new.webvis(wv=wv, root=pv.bar(wv=wv, data=c(1, 1.2, 1.5, 1.5, .7, .5, .2), height.name="y", left.name="x", height.scale="linear.y.y", left.scale="linear.x.x", bottom=10, width=20, ymin=0, ymax=140))
#			+ pv.label(wv=wv, data=1:7, text.name="y", anchor="top", text.style="white")))
#	+ (new.webvis(wv=wv, root=pv.rule(wv=wv, data=1:4, bottom.name="y", bottom.scale="linear.y.y", ymin=10)))# + pv.label(data=data, text="d"))
#	+ pv.rule(wv=wv, left=0, bottom=0)
#)
#remove(wv)



