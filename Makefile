meine = $(shell kpsewhich meine.bib)
beispielkorpus = $(shell kpsewhich beispielkorpus.bib)
referatory = $(shell kpsewhich referatory.bib)

LTX = latexmk -pdf -pdflatex="pdflatex " -use-make


all: Beitrag.pdf Beitrag.zip


Beitrag.pdf: Beitrag.tex datengrundlage.tex
	$(LTX) $<

Beitrag.zip: Beitrag.pdf
	zip Beitrag.zip Beitrag.tex Beitrag.bbl \
	Datengrundlage.tex datengrundlage.tex \
	*.sty logos/*

