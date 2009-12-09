makeall:
	make makedev
	make clean

makedev:
	make mecdf.pdf

mecdf.pdf: mecdf.tex maia.sty
	pdflatex mecdf.tex

mecdf.tex: mecdf.rnw
	R CMD Sweave mecdf.rnw

clean:
	rm *.tex
	rm *.aux
	rm *.log
	rm tmp*
	rm Rplots*


