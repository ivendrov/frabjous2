
all:	frabjous.pdf

CXXFLAGS = -Wall

%.pdf:	%.tex
	-pdflatex $*
	pdflatex $*

.PHONY:	clean 

clean:
	-${RM} *.log *.aux *.out *.nav *.snm *.toc *.tof *.vrb *~

