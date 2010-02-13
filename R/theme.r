mecdf.theme = function (theme)
{	#for .First.lib
	rgb = grDevices::rgb

	if (theme == "simple") .mecdfcols (fline="black", ffill="white")
	else if (theme == "emerald")
		.mecdfcols (rgb (0.08, 0.6, 0.4), c (0, 0.4, 0.05), c (0.2, 1, 0.1) )
	else if (theme == "blue-sapphire")
		.mecdfcols ("blue", c (0.3, 0, 0.6), c (0, 0, 1) )
	else if (theme == "ruby")
		.mecdfcols ("black", c (0.4, 0, 0), c (1, 0.2, 0.2) )
	else if (theme == "silver")
		.mecdfcols ("grey30", c (0.60, 0.60, 0.62), c (0.95, 0.95, 0.94) )
	else if (theme == "gold")
		.mecdfcols (rgb (0.98, 1, 0.15), c (0.5, 0.55, 0), c (0.98, 1, 0.15) )
	else stop ("unsupported theme")
}

.mecdfcols = function (sline="black", sfill1=c (0.92, 0.92, 0.92), sfill2=sfill,
	fline="grey70", ffill="grey95")
{	options (mecdf.frame=list (line=fline, fill=ffill) )
	options (mecdf.surface = list (line=sline, fill1=sfill1, fill2=sfill2) )
}




