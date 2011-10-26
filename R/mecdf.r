mecdf = function (x, continuous=FALSE, ...,
	validate=TRUE, expand=continuous, project=FALSE, expandf=0.1)
{	x = cbind (x)
	nraw = nr = nrow (x)
	nc = ncol (x)
	if (validate)
	{	if (length (list (...) ) > 0)
			stop ("invalid constructor argument")
		if (!is.numeric (x) ) stop ("x must be numeric")
		if (!all (is.finite (x) ) ) stop ("all x must be finite")
		for (j in 1:nc) if (length (unique (x [,j]) ) < 2)
			stop ("each variable requires at least 2 distinct realisations")
		if (nc == 1) x [] = sort (x)
		if (is.null (colnames (x) ) ) colnames (x) = paste ("x", 1:ncol (x), sep="")
		if (is.null (rownames (x) ) ) rownames (x) = 1:nr
	}
	if (expand)
	{	nr = nr + 2
		a = b = numeric (nc)
		for (j in 1:nc)
		{	xrng = range (x [,j])
			xf = expandf * diff (xrng)
			a [j] = xrng [1] - xf
			b [j] = xrng [2] + xf
		}
		x = rbind (a, x, b)
	}
	if (project)
		for (j in 1:nc) x [,j] = (order (order (x [,j]) ) - 1) / (nr - 1)
	Fh = Fst = NULL
	if (nc > 1)
	{	if (continuous)
		{	Fh = .mecdf.continuous
			Fst = .mecdf.vertex
		}
		else Fh = FUNCTION (.mecdf.step)
	}
	else
	{	if (continuous) Fh = .uecdf.continuous
		else Fh =.uecdf.step
	}
	extend (FUNCTION (.mecdf.main), "mecdf", continuous, Fh, Fst, nraw, nr, nc, x)
}

.mecdf.main = function (u)
{	if (.$nc > 1)
	{	if (!is.matrix (u) ) u = rbind (u)
		if (.$nc != ncol (u) )
			stop ("k-variate mecdf requires k-column matrix")
		.mecdf.interpolate (.$Fh, .$Fst, .$nr, .$nc, .$x, u)
	}
	else
	{	if (is.matrix (u) && ncol (u) > 1)
			stop ("univariate mecdf doesn't accept multicolumn matrix")
		.uecdf.interpolate (.$Fh, .$nr, .$x, u)
	}
}

print.mecdf = function (m, ...)
{	variate = if (m$nc == 1) "univariate"
	else if (m$nc == 2) "bivariate"
	else paste (m$nc, "-variate", sep="")
	type = if (m$continuous) "continuous" else "step"
	cat ("mecdf_{", variate, ", ", type, "}\n", sep="")
	print (samp (m$x) )
}

plot.mecdf = function (m, ...)
{	p = m (m$x)
	if (m$nc == 1) .uecdf.plot (m, p, m$continuous, ...)
	else if (m$nc == 2) .becdf.plot (m, p, ...)
	else stop ("s3x_plot.mecdf only supports univariate and bivariate models")
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

.becdf.plot = function (e, p, lines=TRUE, lty=1, col=rgb (0.975, 0.7, 0), ...)
{	labs = colnames (e$x)
	x1 = e$x [,1]; x2 = e$x [,2]
	plot (x1, x2, xlab=labs [1], ylab=labs [2], pch=NA, ...)
	if (lines)
	{	segments (x1, x2, x1 - 2 * diff (range (x1) ), x2, lty=lty, col=col)
		segments (x1, x2, x1, x2 - 2 * diff (range (x2) ), lty=lty, col=col)
	}
	text (x1, x2, round (p, 2) )
}



