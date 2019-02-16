#! /usr/bin/Rscript
library(knitr)
library(rmarkdown)
##script.dir <- dirname(sys.frame(1)$ofile)
file <- "BeispieleAnnotieren.Rmd"
rmarkdown::render(file)
