meine = $(shell kpsewhich meine.bib)
beispielkorpus = $(shell kpsewhich beispielkorpus.bib)
referatory = $(shell kpsewhich referatory.bib)

biblio.bib: ${referatory} ${meine} ${beispielkorpus}
	cat ${referatory} ${meine} ${beispielkorpus} > biblio.bib
