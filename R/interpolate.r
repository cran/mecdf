.mecdf.step = function (u)
{	k = rep (TRUE, .$nr)
	for (j in 1:.$nc) k = k & (.$x [,j] <= u [j])
	sum (k) / .$nr
}

.mecdf.vertex = function (u)
{	k = rep (TRUE, .$nr)
	for (j in 1:.$nc) k = k & (.$x [,j] <= u [j])
	(sum (k) - 1) / (.$nr - 1)
}

.mecdf.continuous = function (u)
{	k = rep (NA, .$nc)
	a = b = p = q = numeric (.$nc)
	for (j in 1:.$nc)
	{	i = (.$x [,j] <= u [j])
		n1 = sum (i)
		if (n1 == 0) k [j] = 0
		else if (n1 == .$nr) k [j] = max (.$x [,j])
		else
		{	x1 = .$x [i, j]
			a [j] = max (x1)
			if (u [j] == a [j]) k [j] = a [j]
			else
			{	x2 = .$x [!i, j] 
				b [j] = min (x2)
			}
		}
	}
	if (any (!is.na (k) & k == 0) ) 0
	else if (all (!is.na (k) ) ) .$Fst (k)
	else
	{	p = (u - a) / (b - a)
		q = 1 - p
		nvert = 2^sum (is.na (k) )
		w = 1
		ust = NULL
		vst = numeric (nvert)
		for (j in .$nc:1)
		{	if (is.na (k [j]) )
			{	w = c (q [j] * w, p [j] * w)
				ust1 = cbind (a [j], ust)
				ust2 = cbind (b [j], ust)
				ust = rbind (ust1, ust2)
			}
			else ust = cbind (k [j], ust)
		}
		for (i in 1:nvert) vst [i] = .$Fst (ust [i,])
		sum (w * vst)
	}
}

