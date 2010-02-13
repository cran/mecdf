mecdf = function (x, continuous, expand=0.1, validate=TRUE, project=FALSE)
{	x = cbind (x)
	if (is.null (colnames (x) ) ) colnames (x) = paste ("x", 1:ncol (x), sep="")
	nr = nrow (x)
	nc = ncol (x)
	if (validate)
	{	if (!is.numeric (x) ) stop ("x must be numeric")
		if (!all (is.finite (x) ) ) stop ("all x must be finite")
		for (j in 1:nc) if (length (unique (x [,j]) ) < 2)
			stop ("each variable requires at least 2 distinct realisations")
	}
	if (!is.na (expand) )
	{	nr = nr + 2
		a = b = numeric (nc)
		for (j in 1:nc)
		{	xrng = range (x [,j])
			xf = expand * diff (xrng)
			a [j] = xrng [1] - xf
			b [j] = xrng [2] + xf
		}
		x = rbind (a, x, b)
	}
	if (project) for (j in 1:nc) x [,j] = (order (order (x [,j]) ) - 1) / (nr - 1)
	if (nc == 1) x [] = sort (x)
	if (missing (continuous) ) continuous = (nc == 1)
	m = FUNCTION (.mecdf.main, x, nr, nc)
	e = environment (m)
	if (continuous)
	{	e$Fh = FUNCTION (.mecdf.continuous)
		e$Fst = FUNCTION (.mecdf.vertex)
		environment (e$Fst) = e
	}
	else e$Fh = FUNCTION (.mecdf.step)
	environment (e$Fh) = e
	extend (structure (m, continuous=continuous), "mecdf")
}

#inefficient
.mecdf.main = function (u)
{	if (is.matrix (u) )
	{	n = nrow (u)
		v = numeric (n)
		for (i in 1:n) v [i] = .$Fh (u [i,])
		v
	}
	else
	{	n = length (u)
		if (.$nc == 1 && n > 1)
		{	v = numeric (n)
			for (i in 1:n) v [i] = .$Fh (u [i])
			v
		}
		else .$Fh (u)
	}
}

print.mecdf = function (m, ...)
{	type = if (attr (m, "continuous") ) "continuous" else "step"
	cat ("mecdf:", type, "function\n      ",
		m$nr, "realisations of", m$nc, "random variables\n")
}

plot.mecdf = function (m, ...)
{	p = m (m$x)
	if (m$nc == 1) .uecdf.plot (m, p, attr (m, "continuous"), ...)
	else if (m$nc == 2) .becdf.plot (m, p, ...)
	else stop ("plot.mecdf only supports univariate and bivariate models")
}

.uecdf.plot = function (e, p, continuous, ...)
{	xlab = colnames (e$x)
	ylab = "Fh(x)"
	if (continuous)
		plot (e$x, p, ylim=c (0, 1), yaxs="i", type="l", xlab=xlab, ylab=ylab, ...)
	else
	{	plot (e$x, p, ylim=c (0, 1), yaxs="i", xlab=xlab, ylab=ylab, pch=NA, ...)
		x1 = e$x [-e$nr]
		x2 = e$x [-1]
		p0 = p [-e$nr]
		segments (x1, p0, x2, p0)
		segments (e$x, c (0, p), e$x, c (p, 1) )
	}
}

.becdf.plot = function (e, p, col, lower=FALSE, upper=FALSE, ...)
{	labs = colnames (e$x)
	x1 = e$x [,1]; x2 = e$x [,2]
	if (missing (col) ) col = rgb (0.975, 0.7, 0)
	plot (x1, x2, xlab=labs [1], ylab=labs [2], pch=NA, ...)
	if (lower)
	{	segments (x1, x2, x1 - 2 * diff (range (x1) ), x2, col=col)
		segments (x1, x2, x1, x2 - 2 * diff (range (x2) ), col=col)
	}
	if (upper)
	{	segments (x1, x2, x1 + 2 * diff (range (x1) ), x2, col=col)
		segments (x1, x2, x1, x2 + 2 * diff (range (x2) ), col=col)
	}
	text (x1, x2, round (p, 2) )
}


