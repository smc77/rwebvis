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