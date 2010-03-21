#
# webvis: An R package to create web visualizations.
# author: Shane Conway <shane.conway@gmail.com>
#
# This is released under a GPL license.
#
# Any reproduction of this code must include attribution to the NY Times.
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
			paste("<div id='", div.id, "'>", sep=""), 
		"<center id='title'> 
				<large><b>CHART</b>,<br> 
				Shewing at One View<br> 
				The Price of The Quarter of Wheat,</large><br> &amp;
				Wages of Labour by the Week,<br> 
				from The Year 1565 to 1821,<br> 
				by WILLIAM PLAYFAIR
		</center> 
		",
		"<script type='text/javascript+protovis'>",
		as.character(wv),
		"</script></div>", getTail())
}

add.line <- function(wv, data, bottom=(wv$height/2), top, left=(wv$width/2), right, stroke.style) {
	multiplier <- (wv$height / max(data)) - 5
	interval <- ((wv$width-30) / length(data))
	vis <- paste("vis.add(pv.Line)",
			if(!missing(data) || length(data)) paste(".data(", protovis.data(data), ")") else "",
			if(!missing(bottom) || length(bottom)) paste(".bottom(function(d) d * ", multiplier, ")"),
			if(!missing(left) || length(left)) paste(".left(function() this.index * ", interval, " + 15)"),
			";", sep="")
	wv$vis <- c(wv$vis, vis)
	wv
}

add.line <- function(wv, data, bottom, top, left, right, stroke.style, equal.spacing=TRUE) {
	if(!("x" %in% colnames(data)))
		data$x <- 1:length(data$y)
	vis <- paste("vis.add(pv.Line)",
			if(!missing(data)) paste(".data(", protovis.data(data), ")") else "",
			if(!missing(bottom)) paste(".bottom(", bottom, ")") else paste(".bottom(function(d) d.y", if(equal.spacing) paste("*", (wv$height / max(data$y)) - 5), ")"),
			if(!missing(left)) paste(".left(", left, ")") else paste(".left(function(d) d.x", if(equal.spacing) paste("*", (wv$width / max(data$x)) - 5), ")"),
			";", sep="")
	wv$vis <- c(wv$vis, vis)
	wv
}

add.bar <- function(wv, data, bottom=(wv$height/2), top, height, width, left=(wv$width/2), right, stroke.style) {
	multiplier <- (wv$height / max(data)) - 5
	interval <- ((wv$width-30) / length(data))
	vis <- paste("vis.add(pv.Bar)",
			if(!missing(data)) paste(".data(", protovis.data(data), ")") else "",
			if(!missing(bottom) || length(bottom)) paste(".bottom(0)"),
			if(!missing(left) || length(left)) paste(".left(function() this.index * ", interval, ")"),
			if(!missing(height)) paste(".bottom(", height, ")") else paste(".height(function(d) d * ", multiplier, ")"),
			if(!missing(width)) paste(".bottom(", width, ")") else paste(".width(", interval - 5, ")"),
			";", sep="")
	wv$vis <- c(wv$vis, vis)
	wv
}

add.area <- function(wv, data, bottom=(wv$height/2), top, height, width, left=(wv$width/2), right, stroke.style) {
	multiplier <- (wv$height / max(data)) - 5
	interval <- ((wv$width-30) / length(data))
	vis <- paste("vis.add(pv.Area)",
			if(!missing(data)) paste(".data(", protovis.data(data), ")") else "",
			if(!missing(bottom) || length(bottom)) paste(".bottom(0)"),
			if(!missing(left) || length(left)) paste(".left(function() this.index * ", interval, ")"),
			if(!missing(height)) paste(".bottom(", height, ")") else paste(".height(function(d) d * ", multiplier, ")"),
			#if(!missing(width)) paste(".bottom(", width, ")") else paste(".width(", interval - 5, ")"),
			";", sep="")
	wv$vis <- c(wv$vis, vis)
	wv
}

add.wedge <- function(wv, data, bottom=(wv$height/2), top, left=(wv$width/2), right, inner.radius, outer.radius=(0.4 * min(wv$width, wv$height)), angle, start.angle, end.angle, stroke.style) {
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
	if(class(data) %in% c("character", "numeric", "vector")) {
		data <- paste("[", 
				paste(data, collapse=", "), 
				"]", sep="")		
	} else if(class(data) %in% c("data.frame")) {
		data <- paste("[", paste(lapply(1:nrow(data), function(i) { k <- data[i,]; paste("{", paste(paste(colnames(k), ":", k), collapse=", "), "}") }), collapse=","), "]")
	}
	return(data)
}

render.webvis <- function(wv, vis.name="demo", path="c:\\temp\\protovis-3.1\\examples\\", file.name=paste(path, vis.name, ".html", sep=""), con=file(file.name, "w"), title="", protovis.path="../protovis-r3.1.js") {
	wv <- c(wv, render="vis.render();")
	#check.webvis(wv)
	writeLines(webvisToHTML(c(wv$panel, wv$vis, wv$render), title=title, protovis.path=protovis.path), con=con)
	close(con)
	browseURL(url=file.name)
}

#' Pull vote data from NY Times Congress API.
#'
#' \code{getNYTCongress} pulls vote data from the NY Times Congress API.
#'
#' @param congress.number 
#' @param chamber 
#' @param session.number 
#' @param roll.call.number 
#' @param type Specifies the type of data to retrieve 
#' @param api.key The Times API requires an API 
#' @return XML results from NY Times API
#' @keywords data
#' @author Shane Conway \email{shane.conway@@gmail.com}
#' @references
#' \url{http://developer.nytimes.com/}
#' @seealso \code{\link{parseVotes}} which fully parse the output from this function
#' @examples
#' \dontrun{
#' roll.call.xml <- getNYTCongress(110, "senate", 2, 194)
#' }
#' data(xml, package="nytR")
#' votes <- parseVotes(roll.call.xml)
#' vote.details <- voteDetail(roll.call.xml)
new.panel <- function(wv, width=500, height=500) {
	panel <- paste("var vis = new pv.Panel()",
		paste(".width(",width,")"),
		paste(".height(",height,");"), sep="\n")
	wv$panel=panel
	wv$width=width
	wv$height=height
	return(wv)
}

#' Pull data from the NY Times API
#'
#' \tabular{ll}{
#' Package: \tab nytR\cr
#' Type: \tab Package\cr
#' Version: \tab 0.1\cr
#' Date: \tab 2009-12-23\cr
#' License: \tab GPL (>= 2)\cr
#' LazyLoad: \tab no\cr
#' }
#'
#' Pulls congressional data from NY Times API.  Currently only exposes the Congress API.  Requires an API key from \url{http://developer.nytimes.com/}.
#' 
#' @name Rwebvis-package
#' @aliases Rwebvis
#' @docType package
#' @title Pulls data from NY Times API.
#' @author Shane Conway \email{shane.conway@@gmail.com}
#' @references
#' \url{http://developer.nytimes.com/}
#' @keywords package
NULL



wv <- new.webvis()
wv <- new.panel(wv, width=1000)
#wv <- add.bar(wv, data=c(1, 1.2, 1.7, 1.5, .7, .5, .2))
#wv <- add.line(wv, data=c(1, 1.2, 1.7, 1.5, .7, .5, .2))
#wv <- add.dot(wv, data=c(1, 1.2, 1.7, 1.5, .7, .5, .2))
#wv <- add.wedge(wv, data=c(1, 1.2, 1.7, 1.5, .7, .5, .2))
#wv <- add.wedge(wv, data=c(1, 1.2, 1.7, 1.5, .7, .5, .2), inner.radius=80)
#wv <- add.line(wv, data=data.frame(y=c(1, 1.2, 1.7, 1.5, .7, .5, .2)))
wv <- add.line(wv, data=data.frame(x=c(1, 1.2, 1.7, 1.5, .7, .5, .2), y=c(1, 1.2, 1.7, 1.5, .7, .5, .2)))
#wv2 <- add.bar(wv, data=c(1, 1.2, 1.7, 1.5, .7, .5, .2))
#wv <- add.area(wv, data=c(1, 1.2, 1.7, 1.5, .7, .5, .2))
#wv2 <- add.dot(wv, data=c(1, 1.2, 1.7, 1.5, .7, .5, .2))
render.webvis(wv)