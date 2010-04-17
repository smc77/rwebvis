###############################################################################
#
# webvis: An R package to create web visualizations.
# author: Shane Conway <shane.conway@gmail.com>
#
# demo example from http://vis.stanford.edu/protovis/ex/weather.html
#
# This is released under a BSD license.
#
# Documentation was created using roxygen:
# roxygenize('webvis', roxygen.dir='webvis', copy.package=FALSE, unlink.target=FALSE)
#
###############################################################################

h <- 3
w <- 18

vis <- pv.panel(width=200, height=250)
vis + pv.Bar(wv=vis, data=weather, bottom.name=)
#
#var w = 18, h = 3;
#
#var vis = new pv.Panel()
#.width(200)
#.height(250);
#
#/* Record range. */
#		var record = vis.add(pv.Bar)
#.data(weather)
#.bottom(function(d) d.record.low * h)
#.height(function(d) (d.record.high - d.record.low) * h)
#.left(function() this.index * w)
#.width(w - 2)
#.fillStyle("#ccc");
#
#/* Normal range. */
#		record.add(pv.Bar)
#.bottom(function(d) d.normal.low * h)
#.height(function(d) (d.normal.high - d.normal.low) * h)
#.fillStyle("#999");
#
#/* White grid lines. */
#		vis.add(pv.Rule)
#.data([20, 40, 60])
#.bottom(function(d) d * h + 1)
#.left(0).right(20)
#.lineWidth(2).strokeStyle("white")
#.anchor("right").add(pv.Label)
#.text(function(d) d + "\u00b0");
