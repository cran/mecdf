mecdf = function (x)
{	if (!is.numeric (x) ) stop ("x must be numeric")
	if (!all (is.finite (x) ) ) stop ("all x must be finite")
	x = cbind (x)
	if (is.null (colnames (x) ) ) colnames (x) = paste ("x", 1:ncol (x), sep="")

	nr = nrow (x)
	nc = ncol (x)
	p = numeric (nr)
	for (i in 1:n)
	{	v = numeric ()
		for (j in 1:nc) v [j] = sum (x [,j] <= x [i, j])
		p [i] = min (v)
	}
	p = p / nr

	f = freemethod (.mecdf.evaluate, x, p, nr, nc)
	class (f) = "mecdf"
	f
}

#not vectorised properly
#u is one (multivariate) realisation
.mecdf.evaluate = function (u)
{	v = 0
	for (i in 1:.$nr) if (all (.$x [i,] <= u) && .$p [i] > v) v = .$p [i]
	v
}

print.mecdf = function (m, ...)
{	e = environment (m)
	cat ("mecdf:", e$nr, "realisations,", e$nc, "variables\n")
}

plot.mecdf = function (m, ...)
{	e = environment (m)
	if (e$nc == 1) .uecdf.plot (e, ...)
	else if (e$nc == 2) .becdf.plot (e, ...)
	else stop ("plot.mecdf only supports univariate and bivariate case")
}

.uecdf.plot = function (e, ...) plot (e$x, e$p, ...)

.becdf.plot = function (e, ...)
{	x1 = e$x [,1]; x2 = e$x [,2]
	plot (x1, x2, pch=NA)
	segments (x1, x2, x1 + 2 * diff (range (x1) ), x2, col="red")
	segments (x1, x2, x1, x2 + 2 * diff (range (x2) ), col="red")
	text (x1, x2, round (e$p, 2) )
}

