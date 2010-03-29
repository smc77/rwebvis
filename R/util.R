#
# webvis: An R package to create web visualizations.
# author: Shane Conway <shane.conway@gmail.com>
#
# This is released under a BSD license.
#
# Documentation was created using roxygen:
# NAMESPACE.FILE <- FALSE; roxygenize('webvis', roxygen.dir='webvis', copy.package=FALSE, unlink.target=FALSE)
#
###############################################################################


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
	if(!missing(data)) if(is.data.frame(data)) { if(any(field %in% colnames(data))) return(TRUE) } else { stop("data should be a data.frame")}
	FALSE
}
