# TODO: Add comment
# 
# Author: TMC
###############################################################################



data=data.frame(x=c(1, 1.2, 1.7, 1.5, .7, .5, .2), y=c(1, 1.2, 1.7, 1.5, .7, .5, .2))
wv <- new.webvis()
wv <- new.panel(wv, width=1000)
#wv <- add.bar(wv, data=c(1, 1.2, 1.7, 1.5, .7, .5, .2))
#wv <- add.line(wv, data=c(1, 1.2, 1.7, 1.5, .7, .5, .2))
#wv <- add.dot(wv, data=c(1, 1.2, 1.7, 1.5, .7, .5, .2))
#wv <- add.wedge(wv, data=c(1, 1.2, 1.7, 1.5, .7, .5, .2))
#wv <- add.wedge(wv, data=c(1, 1.2, 1.7, 1.5, .7, .5, .2), inner.radius=80)
#wv <- add.line(wv, data=data.frame(y=c(1, 1.2, 1.7, 1.5, .7, .5, .2)))
wv <- add.line(wv, data=data)
#wv <- add.bar(wv, data=data)
#wv <- add.area(wv, data=c(1, 1.2, 1.7, 1.5, .7, .5, .2))
#wv2 <- add.dot(wv, data=c(1, 1.2, 1.7, 1.5, .7, .5, .2))
render.webvis(wv)


wv <- new.webvis()
wv <- new.panel(wv, width=800, height=445, right=60, bottom=20)
wv <- add.area(wv, data=wheat, interpolate="step-after", bottom=0, fill.style="#aaa", stroke.style="#999", add.rule=TRUE)
wv <- add.area(wv, data=wheat, interpolate="step-after", bottom=0, fill.style="#aaa", stroke.style="#999", add.rule=TRUE)

#wv <- add.bar(wv, data=c(1, 1.2, 1.7, 1.5, .7, .5, .2))
#wv <- add.line(wv, data=c(1, 1.2, 1.7, 1.5, .7, .5, .2))
#wv <- add.dot(wv, data=c(1, 1.2, 1.7, 1.5, .7, .5, .2))
#wv <- add.wedge(wv, data=c(1, 1.2, 1.7, 1.5, .7, .5, .2))
#wv <- add.wedge(wv, data=c(1, 1.2, 1.7, 1.5, .7, .5, .2), inner.radius=80)
#wv <- add.line(wv, data=c(1, 1.2, 1.7, 1.5, .7, .5, .2))
#wv <- add.bar(wv, data=c(1, 1.2, 1.7, 1.5, .7, .5, .2))
render.webvis(wv)




var w = 860 - 60,
h = 465 - 20,
x = pv.Scale.linear(1565, 1821).range(0, w),
y = pv.Scale.linear(0, 100).range(0, h);

var vis = new pv.Panel()
.width(w)
.height(h)
.right(60)
.bottom(20);

/* Price of The Quarter of Wheat. */
		vis.add(pv.Area)
.data(wheat)
.interpolate("step-after")
.bottom(0)
.height(function(d) y(d.wheat))
.left(function(d) x(d.year))
.fillStyle("#aaa")
.strokeStyle("#999")
.add(pv.Rule);

/* Weekly Wages of a Good Mechanic. */
		vis.add(pv.Area)
.data(wheat.filter(function(d) d.wages))
.left(function(d) x(d.year))
.bottom(0)
.height(function(d) y(d.wages))
.fillStyle("hsla(195, 50%, 80%, .75)")
.anchor("top").add(pv.Line)
.fillStyle(null)
.lineWidth(4)
.strokeStyle("lightcoral")
.add(pv.Line)
.top(function() this.proto.top() + 1.5)
.lineWidth(1.5)
.strokeStyle("black");

vis.add(pv.Label)
.left(130).bottom(31)
.font("italic 10px serif")
.text("Weekly Wages of a Good Mechanic");

/* Y-axis. */
		vis.add(pv.Rule)
.bottom(-.5)
.add(pv.Rule)
.data(pv.range(0, 100, 10))
.bottom(y)
.strokeStyle("rgba(255, 255, 255, .2)")
.anchor("right").add(pv.Label)
.visible(function() !(this.index % 2))
						.text(function(s) s + (s ? "" : " shillings"));

/* X-axis. */
		vis.add(pv.Rule)
.data(pv.range(1560, 1830, 10))
.bottom(0)
.left(x)
.height(-4)
.add(pv.Rule)
.data(pv.range(1600, 1850, 50))
.height(null)
.top(0)
.strokeStyle("rgba(0, 0, 0, .2)")
.anchor("bottom").add(pv.Label)
.textMargin(8);

/* Monarchs. */
		vis.add(pv.Bar)
.data(monarch)
.height(5)
.top(function(d) (!d.commonwealth && (this.index % 2)) ? 15 : 10)
							.fillStyle(function(d) d.commonwealth ? null : "#000")
							.strokeStyle("#000")
.left(function(d) x(d.start))
.width(function(d) x(d.end) - x(d.start))
.anchor("center").add(pv.Label)
.textBaseline("top")
.textMargin(6)
.font("italic 10px serif")
.text(function(d) d.name);


var wheat = [
		{ year: 1565, wheat: 41, wages: 5 },
		{ year: 1570, wheat: 45, wages: 5.05 },
		{ year: 1575, wheat: 42, wages: 5.08 },
		{ year: 1580, wheat: 49, wages: 5.12 },
		{ year: 1585, wheat: 41.5, wages: 5.15 },
		{ year: 1590, wheat: 47, wages: 5.25 },
		{ year: 1595, wheat: 64, wages: 5.54 },
		{ year: 1600, wheat: 27, wages: 5.61 },
		{ year: 1605, wheat: 33, wages: 5.69 },
		{ year: 1610, wheat: 32, wages: 5.78 },
		{ year: 1615, wheat: 33, wages: 5.94 },
		{ year: 1620, wheat: 35, wages: 6.01 },
		{ year: 1625, wheat: 33, wages: 6.12 },
		{ year: 1630, wheat: 45, wages: 6.22 },
		{ year: 1635, wheat: 33, wages: 6.3 },
		{ year: 1640, wheat: 39, wages: 6.37 },
		{ year: 1645, wheat: 53, wages: 6.45 },
		{ year: 1650, wheat: 42, wages: 6.5 },
		{ year: 1655, wheat: 40.5, wages: 6.6 },
		{ year: 1660, wheat: 46.5, wages: 6.75 },
		{ year: 1665, wheat: 32, wages: 6.8 },
		{ year: 1670, wheat: 37, wages: 6.9 },
		{ year: 1675, wheat: 43, wages: 7 },
		{ year: 1680, wheat: 35, wages: 7.3 },
		{ year: 1685, wheat: 27, wages: 7.6 },
		{ year: 1690, wheat: 40, wages: 8 },
		{ year: 1695, wheat: 50, wages: 8.5 },
		{ year: 1700, wheat: 30, wages: 9 },
		{ year: 1705, wheat: 32, wages: 10 },
		{ year: 1710, wheat: 44, wages: 11 },
		{ year: 1715, wheat: 33, wages: 11.75 },
		{ year: 1720, wheat: 29, wages: 12.5 },
		{ year: 1725, wheat: 39, wages: 13 },
		{ year: 1730, wheat: 26, wages: 13.3 },
		{ year: 1735, wheat: 32, wages: 13.6 },
		{ year: 1740, wheat: 27, wages: 14 },
		{ year: 1745, wheat: 27.5, wages: 14.5 },
		{ year: 1750, wheat: 31, wages: 15 },
		{ year: 1755, wheat: 35.5, wages: 15.7 },
		{ year: 1760, wheat: 31, wages: 16.5 },
		{ year: 1765, wheat: 43, wages: 17.6 },
		{ year: 1770, wheat: 47, wages: 18.5 },
		{ year: 1775, wheat: 44, wages: 19.5 },
		{ year: 1780, wheat: 46, wages: 21 },
		{ year: 1785, wheat: 42, wages: 23 },
		{ year: 1790, wheat: 47.5, wages: 25.5 },
		{ year: 1795, wheat: 76, wages: 27.5 },
		{ year: 1800, wheat: 79, wages: 28.5 },
		{ year: 1805, wheat: 81, wages: 29.5 },
		{ year: 1810, wheat: 99, wages: 30 },
		{ year: 1815, wheat: 78 }, // TODO
{ year: 1820, wheat: 54 },
{ year: 1821, wheat: 54 }
];

var monarch = [
		{ name: "Elizabeth", start: 1565, end: 1603 },
		{ name: "James I", start: 1603, end: 1625 },
		{ name: "Charles I", start: 1625, end: 1649 },
		{ name: "Cromwell", start: 1649, end: 1660, commonwealth: true },
		{ name: "Charles II", start: 1660, end: 1685 },
		{ name: "James II", start: 1685, end: 1689 },
		{ name: "W&M", start: 1689, end: 1702 },
		{ name: "Anne", start: 1702, end: 1714 },
		{ name: "George I", start: 1714, end: 1727 },
		{ name: "George II", start: 1727, end: 1760 },
		{ name: "George III", start: 1760, end: 1820 },
		{ name: "George IV", start: 1820, end: 1821 }
];