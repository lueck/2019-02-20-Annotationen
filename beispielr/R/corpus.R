library("tools")
library("readr")
library("dplyr")


## column specification for the csv importer
corpus_cols <- cols(tokenNum = col_integer(),
                    sentenceNum = col_integer(),
                    numInSentence = col_integer(),
                    srcStart = col_skip(),
                    srcEnd = col_skip(),
                    start = col_skip(),
                    end = col_skip(),
                    tokenId = col_skip(),
                    sentenceId = col_skip()
                    )

## Read all csv files from a given directory.
read_csv_corpus <- function(dir, col_types = corpus_cols, bibkey = "sigle") {

    read_csv_corpus_file <- function(filename) {
        read_csv(file.path(dir, filename), col_types = col_types) %>%
            mutate(!!bibkey := file_path_sans_ext(filename)) # dplyr>=0.7
    }

    bind_rows(lapply(list.files(dir, pattern = "*.csv"), read_csv_corpus_file))
}


## manuell    
## Ros1853 <- read_csv("tcf/Ros1853.csv", col_types = corpus_cols) %>%
##     mutate(sigle = "Ros1853")

## KdU1963 <- read_csv("tcf/KdU1963.csv", col_types = corpus_cols) %>%
##     mutate(sigle = "KdU1963")

## corpus <-
##     bind_rows(
##         Ros1853
##       , KdU1963)
