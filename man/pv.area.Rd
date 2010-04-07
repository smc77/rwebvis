\name{pv.area}
\alias{pv.area}
\title{# line example 1 (using layers)...}
\usage{pv.area(bottom=0, height.name="y", left.name="x", height.scale=paste("linear", height.name, "y", sep = "."), left.scale=paste("linear", left.name, "x", sep =
    "."), scale.min=0, ...)}
\description{# line example 1 (using layers)
wv <- new.webvis(width=150, height=150)
render.webvis(wv + pv.area(wv=wv, data=c(1, 1.2, 1.7, 1.5, .7, .5, .2), bottom=0, height.name="y", left.name="x", height.scale="linear.y.y", left.scale="linear.x.x"))}
\details{wv <- new.webvis(width=150, height=150)
render.webvis(wv + pv.area(wv=wv, data=c(1, 1.2, 1.7, 1.5, .7, .5, .2)))

# line example 2 (need to make sure that it doesn't go over the edge
wv <- new.webvis(width=150, height=150)
wv <- wv + (new.webvis(wv=wv, root=pv.area(wv=wv, data=c(1, 1.2, 1.7, 1.5, .7, .5, .2), height.name="y", left.name="x", height.scale="linear.y.y", left.scale="linear.x.x"))
+ pv.dot())
render.webvis(wv)

# line example 4
wv <- new.webvis(width=150, height=150)
render.webvis(wv + pv.area(wv=wv, data=c(1, 1.2, 1.7, 1.5, .7, .5, .2), top.name="y", left.name="x", top.scale="linear.y.y", left.scale="linear.x.x"))

# line example 5
wv <- new.webvis(width=150, height=150)
render.webvis(wv + pv.area(wv=wv, data=c(1, 1.2, 1.7, 1.5, .7, .5, .2), top.name="y", right.name="x", top.scale="linear.y.y", right.scale="linear.x.x"))

# line example 7
wv <- new.webvis(width=150, height=150)
render.webvis(wv + pv.area(wv=wv, data=c(1, 1.2, 1.7, 1.5, .7, .5, .2), bottom.name="y", left.name="x", bottom.scale="linear.y.y", left.scale="linear.x.x", interpolate="step-after"))

#pv.area(data=c(1, 1.2, 1.7, 1.5, .7, .5, .2), height.name="y", left.name="x", height.scale="linear.y.y", left.scale="linear.x.x", ymin=10, ymax=100, bottom=0, render=TRUE)}
