default:
	make -f make-ext
	make clean

enhanced:
	make -f make-ext bldcmd=makesweave

init:
	rm -f /tmp/makesweavepipe
	mkfifo /tmp/makesweavepipe
	echo -e "0\n" > /tmp/makesweavestatus
	makesweave -i &

clean:
	rm -f *.tex
	rm -f *.aux
	rm -f *.log
	rm -f tmp*
	rm -f Rplots*

