\name{pv.label}
\alias{pv.label}
\title{wv <- new...}
\usage{pv.label(text, text.name, ...)}
\description{wv <- new.webvis(root=pv.panel(width=300, height=200, left=20, top=20, right=20, bottom=20))
wv <- wv + (new.webvis(wv=wv, root=pv.rule(wv=wv, data=1:10, axis="y", stroke.style="rgba(128,128,128,.2)")) + pv.label(anchor="right", text.name="y"))
wv <- wv + (new.webvis(wv=wv, root=pv.rule(wv=wv, data=1:10, axis="x", stroke.style="rgba(128,128,128,.2)")) + pv.label(anchor="bottom", text.name="x"))
wv <- wv + pv.dot(wv=wv, data=c(1, 1.2, 1.7, 1.5, .7, .5, .2), scale.min=0)
render.webvis(wv)}
