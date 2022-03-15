library("dplyr")
library("magrittr")


## Calculate total number of occurrances.
total <- function(corpus, ...) {
    grouping_vars <- quos(...)
    corpus %>%
        group_by(!!!grouping_vars) %>%
        summarize(total = n())
}

## Calculates the relative frequency of a feature (term frequency).
freq <- function(corpus, ..., col_name = "freq") {
    grouping_vars <- quos(...)
    total <- total(corpus, !!first(grouping_vars))
    freqs <- total(corpus, !!!grouping_vars) %>% rename(n = total)
    left_join(freqs, total) %>%
        mutate(!!col_name := n/total)
}

## Calculates the relative frequency of a feature (term frequency).
freqOFF <- function(corpus, total_grouping_col, ..., col_name = "freq") {
    #grouping_vars <- quos(...)
    total_grouping_col <- as.name(total_grouping_col)
    total <- total(corpus, total_grouping_col)
    freqs <- total(corpus, ...) %>% rename(n = total)
    left_join(freqs, total) %>%
        mutate(!!col_name := n/total)
}

freqOFF <- function(corpus, total_grouping, ..., col_name = "freq") {
    #grouping_vars <- quos(...)
    total_grouping <- as.name(total_grouping)
    x <- dplyr::count(corpus, total_grouping)
    x <- rename(x, n = total)
    y <- dplyr::count(corpus, ...)
    x <- left_join(y, x)
    x <- mutate(x, !!col_name := n/total)
    x
}


## Die Zitierformen der Substantive nach Buch für Rosenkranz, aufsteigend nach Frequenz:
## freq(corpus, sigle, POStag, lemma) %>% filter(POStag == "NN", sigle =="Ros1853") %>% arrange(freq) %>% View()
