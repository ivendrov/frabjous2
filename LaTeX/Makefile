# change this to conform to your system setup
LATEX=latex
PDFLATEX=pdflatex
BIBTEX=bibtex

TARGET=test.pdf

all:			$(TARGET)

# coercion functions
%.pdf:		%.tex %.bbl
		$(PDFLATEX) $* ; $(PDFLATEX) $*

%.aux:		%.tex
		$(PDFLATEX) $*

%.bbl:		%.aux %.bib
		$(BIBTEX) $*

# remove log files and other fluff
clean:		
		-/bin/rm -rf *.log *.aux *.bbl *.out *.blg *.dvi

# remove everything that can be regenerated
squeaky:	clean
		-/bin/rm *~ $(TARGET)
#               be careful with pdf files: they might be images

