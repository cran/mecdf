bcdf.plot = function (m, mmin=0, mmax=1)
{	n = nrow (m)
	if (n != ncol (m) ) stop ("square matrix required")
	m = (m - mmin) / (mmax - mmin)

	p0 = par (mar=c (1, 0.25, 1, 0.25) )
	plot.new ()
	plot.window (c (-0.75, 0.75), c (0, 1.5) )

	.surfplot.plane.uv ()
	.surfplot.plane.u2 ()
	.surfplot.plane.v2 ()
	.surfplot.grid (m, n)

	par (p0)
}

.surfplot.grid = function (m, n)
{	n2 = n - 1
	incr = 1 / n2
	for (i in 1:n2) for (j in 1:n2)
	{	u1 = (i - 1) * incr
		u2 = u1 + incr
		v1 = (j - 1) * incr
		v2 = v1 + incr
		u = c (u1, u1, u2, u2)
		v = c (v1, v2, v2, v1)
		w = c (m [i, j], m [i, j + 1], m [i + 1, j + 1], m [i + 1, j])
		#not true tangent (tangent is dw / sqrt (2 * du^2), noting du=dv)
		dir = (w [3] - w [1]) / incr
		#scaling to interval (0, 1), tangent=1 -> dir=0.5
		dir = 2 * atan (dir) / pi
		.surfplot.poly (u, v, w, border=rgb (0.08, 0.6, 0.4), col=.colinterp (dir) )
	}
}

.colinterp = function (x)
{	col1 = c (0, 0.4, 0.05)
	col2 = c (0.2, 1, 0.1)
	col = col1 + x * (col2 - col1)
	rgb (col [1], col [2], col [3])
}

.surfplot.plane.uv = function () .surfplot.poly (c (0, 0, 1, 1), c (0, 1, 1, 0), 0)

.surfplot.plane.u2 = function ()
{	.surfplot.poly (1, c (0, 0, 1, 1), c (0, 1, 1, 0) )
	.surfplot.lines (1, c (0, 1), 0.25)
	.surfplot.lines (1, c (0, 1), 0.5)
	.surfplot.lines (1, c (0, 1), 0.75)
}

.surfplot.plane.v2 = function ()
{	.surfplot.poly (c (0, 0, 1, 1), 1, c (0, 1, 1, 0) )
	.surfplot.lines (c (0, 1), 1,  0.25)
	.surfplot.lines (c (0, 1), 1, 0.5)
	.surfplot.lines (c (0, 1), 1, 0.75)
}

.surfplot.poly = function (u, v, w, border="grey70", col="grey95")
{	m = .project (u, v, w)
	polygon (m [,1], m [,2], border=border, col=col)
}

.surfplot.lines = function (u, v, w, col="grey70")
{	m = .project (u, v, w)
	lines (m [,1], m [,2], col=col)
}

#todo, fix this up...
.project = function (u, v, w)
{	x = u * cos (pi / 4) + v * cos (pi * 3 / 4)
	y = u * sin (pi / 4) + v * sin (pi * 3 / 4)
	y = 0.71 * y + 0.5 * w
	cbind (x, y)
}






