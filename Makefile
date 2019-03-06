meine = $(shell kpsewhich meine.bib)
beispielkorpus = $(shell kpsewhich beispielkorpus.bib)
referatory = $(shell kpsewhich referatory.bib)

biblio.bib: ${referatory} ${meine} ${beispielkorpus}
	cat ${referatory} ${meine} ${beispielkorpus} > biblio.bib

BeispieleAnnotieren.pdf: BeispieleAnnotieren.Rmd biblio.bib
	./compileRmd.R

all: biblio.bib BeispieleAnnotieren.pdf
