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

is.webvis <- function(x) class(x) == "webvis"
is.webvis.flat <- function(x) class(x) == "webvis.flat"
is.webvis.param <- function(x) class(x) == "webvis.param"

esse <- function(x) {
	if(missing(x)) 
		return(FALSE)
	#test <- try(length(x))
	#if(class(test) == "try-error") if(all(grepl("is missing", test))) x <- NULL
	#if(!exists(x)) 
	#	return(FALSE)
	if(!length(x)) 
		return(FALSE)
	if(all(is.null(x)))
		return(FALSE)
	if(all(is.na(x)))
		return(FALSE)
	if(is.character(x) && all(x==""))
		return(FALSE)
	return(TRUE)
}

#' A simplied version of paste.
#'
#' \code{collapse} A simplied version of paste. 
#'
#' @param ... Comma separated list of values.
#' @return A string of the values combined.
#' @keywords graphics
#' @author Shane Conway \email{shane.conway@@gmail.com}
#' @examples
#' collapse("a", "b", "c")
collapse <- function(...) paste(c(...), collapse="")

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

field.exists <- function(field, data) {
	if(!esse(data)) FALSE
	if(!missing(data)) if(is.data.frame(data)) { if(any(field %in% colnames(data))) return(TRUE) } else { stop("data should be a data.frame")}
	FALSE
}
