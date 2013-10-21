
all:	frabjous_ihi.pdf

CXXFLAGS = -Wall

%.pdf:	%.tex
	-pdflatex $*
	pdflatex $*

.PHONY:	clean squeaky

clean:
	-${RM} *.log *.aux *.out *.nav *.snm *.toc *.tof *.vrb *~

squeaky:	clean
	-${RM} sigplan-template-oliver.pdf
