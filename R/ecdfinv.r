#no expansion
ecdfinv = function (x, p, sort=TRUE)
{	n = length (x)
	if (sort) x = sort (x)
	k = (n - 1) * p + 1
	if (k < 1) x [1]
	else if (k > n) x [n]
	else
	{	kf = floor (k)
		while (kf > 1 && x [kf] == x [kf + 1]) kf = kf - 1
		if (k == kf)
			x [kf]
		else
		{	kc = ceiling (k)
			while (kc < n && x [kc] == x [kc + 1]) kc = kc + 1
			a = (k - kf) / (kc - kf)
			b = 1 - a
			b * x [kf] + a * x [kc]
		}
	}
}

