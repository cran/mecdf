mecdf = function (x)
{	if (!is.numeric (x) ) stop ("x must be numeric")
	if (!all (is.finite (x) ) ) stop ("all x must be finite")
	x = cbind (x)
	if (is.null (colnames (x) ) ) colnames (x) = paste ("x", 1:ncol (x), sep="")

	nr = nrow (x)
	nc = ncol (x)
	f = freemethod (.mecdf.evaluate, x, nr, nc)
	class (f) = "mecdf"
	f
}

.mecdf.fitted = function (f, e)
{	p = numeric (e$nr)
	for (i in 1:e$nr) p [i] = f (e$x [i,])
	p
}

#not vectorised properly
#u is one (multivariate) realisation
.mecdf.evaluate = function (u)
{	k = rep (TRUE, .$nr)
	for (i in 1:.$nc) k = k & (.$x [,i] <= u [i])
	sum (k) / .$nr
}

print.mecdf = function (m, ...)
{	e = environment (m)
	cat ("mecdf:", e$nr, "realisations,", e$nc, "variables\n")
}

plot.mecdf = function (m, ...)
{	e = environment (m)
	p = .mecdf.fitted (m, e)
	if (e$nc == 1) .uecdf.plot (e, p, ...)
	else if (e$nc == 2) .becdf.plot (e, p, ...)
	else stop ("plot.mecdf only supports univariate and bivariate case")
}

.uecdf.plot = function (e, p, ...) plot (e$x, p, ...)

.becdf.plot = function (e, p, lower=FALSE, upper=FALSE, ...)
{	x1 = e$x [,1]; x2 = e$x [,2]
	plot (x1, x2, pch=NA)
	if (lower)
	{	segments (x1, x2, x1 - 2 * diff (range (x1) ), x2, col="red")
		segments (x1, x2, x1, x2 - 2 * diff (range (x2) ), col="red")
	}
	if (upper)
	{	segments (x1, x2, x1 + 2 * diff (range (x1) ), x2, col="red")
		segments (x1, x2, x1, x2 + 2 * diff (range (x2) ), col="red")
	}
	text (x1, x2, round (p, 2) )
}


