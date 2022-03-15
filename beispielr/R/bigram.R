library("dplyr")
library("magrittr")


sentences_with_bigrams <- function(corpus, word1 = "z.", word2 = "B.", ngram_start_col = "markerNum", ngram_length_col = "markerLen") {

    ## First filter rows from corpus with word1 and then contruct a
    ## tibble with word2 and tokenNum = tokenNum_word1 + 1
    bigrams_constr <-
        corpus %>%
        filter(token == word1) %>%
        transmute(token1Num = tokenNum,
                  token1 = token,
                  sigle,
                  token2Num = token1Num+1,
                  token2 = word2)
    ## Make inner join with corpus and then drop all columns except
    ## sentenceNum and document identifier.
    inner_join(corpus, bigrams_constr,
               by = c("token" = "token2",
                      "tokenNum" = "token2Num",
                      "sigle")) %>%
        transmute(sentenceNum, sigle, !!ngram_start_col := token1Num, !!ngram_length_col := as.integer(2))
}


tokens_in_sentences_with_bigrams <- function(corpus, word1 = "z.", word2 = "B.") {
    inner_join(corpus, sentences_with_bigrams(corpus, word1, word2))
}

