# TODO: Add comment
# 
# Author: TMC
###############################################################################

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

wheat <- read.csv(con <- textConnection(
				"year, wheat, wages
						1565, 41, 5
						1570, 45, 5.05
						1575, 42, 5.08
						1580, 49, 5.12
						1585, 41.5, 5.15
						1590, 47, 5.25
						1595, 64, 5.54
						1600, 27, 5.61
						1605, 33, 5.69
						1610, 32, 5.78
						1615, 33, 5.94
						1620, 35, 6.01
						1625, 33, 6.12
						1630, 45, 6.22
						1635, 33, 6.3
						1640, 39, 6.37
						1645, 53, 6.45
						1650, 42, 6.5
						1655, 40.5, 6.6
						1660, 46.5, 6.75
						1665, 32, 6.8
						1670, 37, 6.9
						1675, 43, 7
						1680, 35, 7.3
						1685, 27, 7.6
						1690, 40, 8
						1695, 50, 8.5
						1700, 30, 9
						1705, 32, 10
						1710, 44, 11
						1715, 33, 11.75
						1720, 29, 12.5
						1725, 39, 13
						1730, 26, 13.3
						1735, 32, 13.6
						1740, 27, 14
						1745, 27.5, 14.5
						1750, 31, 15
						1755, 35.5, 15.7
						1760, 31, 16.5
						1765, 43, 17.6
						1770, 47, 18.5
						1775, 44, 19.5
						1780, 46, 21
						1785, 42, 23
						1790, 47.5, 25.5
						1795, 76, 27.5
						1800, 79, 28.5
						1805, 81, 29.5
						1810, 99, 30
						1815, 78, 30
						1820, 54, 30
						1821, 54, 30"), header=TRUE)
close(con)
wheat$wages <- as.numeric(wheat$wages)

wv <- new.webvis(root=pv.panel(right=60, top=20, width=800, height=445), width=800, height=445)

wt <- new.webvis(root=pv.area(wv, data=wheat, interpolate="step-after", bottom=0, y.name="wheat", x.name="year", x.padding=0, y.padding=0, ymin=0, ymax=100, fill.style="#aaa", stroke.style="#999"))
wt <- wt + pv.rule()
wv <- wv + wt

wg <- new.webvis(root=pv.area(wv, data=wheat, bottom=0, y.name="wages", x.name="year", x.padding=0, y.padding=0, ymin=0, ymax=100, fill.style="hsla(195, 50%, 80%, .75)"))
wg <- wg + pv.line(line.width=4, stroke.style="lightcoral", anchor="top")
wg <- wg + pv.line(line.width=1.5, stroke.style="black", anchor="top")
wv <- wv + wg

wv <- wv + pv.label(left=130, bottom=31, font="italic 10px serif", text="Weekly Wages of a Good Mechanic")

wr <- new.webvis(root=pv.rule(bottom=-0.5))
wr <- wr + pv.rule(wv=wv, data=data.frame(y=seq(0,100,10)), y.name="y", y.padding=0)
wv <- wv + wr

render.webvis(wv=wv, vis.name="demo2")
