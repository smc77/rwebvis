#
# webvis: An R package to create web visualizations.
# author: Shane Conway <shane.conway@gmail.com>
#
# demo example similar to http://protovis-js.googlecode.com/svn/trunk/examples/area-stacked.html
#
# This is released under a BSD license.
#
# Documentation was created using roxygen:
# roxygenize('webvis', roxygen.dir='webvis', copy.package=FALSE, unlink.target=FALSE)
#
###############################################################################

d <- sin(seq(0, 9.9, 0.1)) + (0.5 * rnorm(100, 5, 2))
data <- data.frame(x=1:100, a=d, b=2*d, c=3*d)
pv.data(data)

wv <- new.webvis(root=pv.panel(right=10, left=20, bottom=20, top=5, width=400, height=200), width=400, height=200)

wp <- new.webvis(root=pv.panel(data=data, width=NULL, height=NULL))
wp <- wp + pv.area()
wv <- wv + wt

wg <- new.webvis(root=pv.area(wv=wv, data=wheat, height.name="wages", left.name="year", fill.style="hsla(195, 50%, 80%, .75)", scale.min=0, scale.max=100))
wg <- wg + pv.line(line.width=4, stroke.style="lightcoral", anchor="top", bottom.name=NULL, left.name=NULL, fill.style="null")
wg <- wg + pv.line(line.width=1.5, stroke.style="black", anchor="top", bottom.name=NULL, left.name=NULL, fill.style="null")
wv <- wv + wg

wv <- wv + pv.label(left=130, bottom=31, font="italic 10px serif", text="Weekly Wages of a Good Mechanic")

wr <- new.webvis(root=pv.rule(bottom=-0.5))
wr2 <- new.webvis(root=pv.rule(wv=wv, data=data.frame(y=seq(0,100,10)), bottom=0, height.name="y", height.scale="linear.y.y", ymin=0, ymax=100, stroke.style="rgba(255, 255, 255, .2)"))
wr2 <- wr2 + pv.label(anchor="right", text.name="y")
wr <- wr + wr2
wv <- wv + wr

monarch2 <- monarch
monarch2$top <- ifelse(monarch2$commonwealth == 0 & as.numeric(rownames(monarch2)) %% 2 == 0, 15, 10)
monarch2$fill <- ifelse(monarch2$commonwealth == 0, "'#000'", "'#fff'")
monarch2$reign <- (monarch2$end-monarch2$start) * (wv$width/(max(monarch2$end)-min(monarch2$start)))
vm <- new.webvis(root=pv.mark(wv=wv, data=monarch2, type="Bar",  
		pv.param(name="data", value="d"), 
		pv.param(name="height", value=5),
		pv.param(name="strokeStyle", value="#000"),
		pv.param(name="left", data.name="start", scale="linear.start.x"),
		pv.param(name="top", data.name="top"),
		pv.param(name="width", data.name="reign"),
		pv.param(name="fillStyle", data.name="fill")
))
vm <- vm + pv.mark(wv=wv, data=monarch2, type="Label", 
		pv.param(name="font", value="italic 10px serif"), 
		pv.param(name="text", data.name="name"), 
		pv.param(name="textMargin", value=6), 
		pv.param(name="textBaseline", value="top"), 
		anchor="center")
wv2 <- wv + vm

render.webvis(wv=wv2, vis.name="playfairs_wheat")
