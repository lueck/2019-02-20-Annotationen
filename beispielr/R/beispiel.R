library(dplyr)
library(tidyr)
library(magrittr)
library(purrr)
library(stringr)


weights <- tibble(
    w_pos = as.double(3),
    w_lf = as.double(4),
    w_tf = as.double(0),
    w_dmt = as.double(2),
    w_dmc = as.double(6),
    bias = as.double(0),
    sum_is = "summa")


cross_join_by_sentence <- function(.data) {
    inner_join(.data, .data, by = c("sigle", "sentenceNum"))
}


postag_df <- tibble(
    POStag = c("NE", "NN", "VVINF", "VVIZU", "VVPP", "VVFIN", "VMINF", "VAINF"),
    norm_pos   = c(1,     0.8,     0.5,     0.5,    0.5,     0.4,     0.2,     0.1))


normalize_freq_exp <- function(f, total) {
    a <- total
    x <- exp(-a * (f - (1/total)))
    x
}

normalize_freq_off <- function(f, total) {
    a <- 100
    x <- 1 - ((total/100) * f)
    x <- if_else(x <= 0, 0, x)
    x
}

normalize_freq <- function(f, max, pos = 1, c = 0.5) {
    x <- 1 - (c * (f - 1) / (max - 1))
    ##x <- if (pos > 0) x else 0 # FIXME
    x
}

generate_example_features <- function(.data, .crossdata) {
    ## select tokens from sentences with markers
    x <- tokens_in_sentences_with_bigrams(.data)
    ## norm_pos: POS tag scalar value
    x <- left_join(x, postag_df, by = c("POStag")) %>%
        replace_na(list(norm_pos = 0))
    ## lf: lemma frequency
    ## x <- left_join(x,
    ##                freq(.data, sigle, POStag, lemma, col_name = "lf") %>%
    ##                select(sigle, POStag, lemma, total, lf),
    ##                by = c("sigle", "POStag", "lemma"))
    ## x <- mutate(x, norm_lf = normalize_freq(lf, total))    
    x <- left_join(x,
                   count(.data, sigle, POStag, lemma) %>%
                   rename(lf=n),
                   by = c("sigle", "POStag", "lemma"))
    x <- left_join(x,
                   x %>%
                   filter(norm_pos > 0) %>%
                   group_by(sigle) %>%
                   summarize(maxlf = max(lf)),
                   by = c("sigle"))
    x <- mutate(x, norm_lf = normalize_freq(lf, maxlf))#, norm_pos))
    x <- mutate(x, norm_lf = ifelse(norm_pos > 0, norm_lf, 0))
    ## tf: token frequency
    x <- left_join(x,
                   count(.data, sigle, token) %>%
                   rename(tf=n),
                   by = c("sigle", "token"))
    x <- left_join(x,
                   x %>%
                   filter(norm_pos > 0) %>%
                   group_by(sigle) %>%
                   summarize(maxtf = max(tf)),
                   by = c("sigle"))
    x <- mutate(x, norm_tf = normalize_freq(tf, maxtf))#, norm_pos))
    x <- mutate(x, norm_tf = ifelse(norm_pos > 0, norm_tf, 0))
    ## sLen: Sentence length
    x <- left_join(x,
                   group_by(x, sigle, sentenceNum) %>%
                   summarize(sLen = n()) %>%
                   ungroup(),
                   by = c("sigle", "sentenceNum"))
    ## dmt: Distance from marker in tokens
    x <- mutate(x, norm_dmt = 1 - (pmin(abs(tokenNum - markerNum),
                                        abs(tokenNum - (markerNum + markerLen - 1))) / sLen))
    ## cross_join on sigle and tokenNun
    crs <- inner_join(x, x, by = c("sigle", "sentenceNum"))
    ## dmc: Distance from marker in commas
    ## a) first count the commas in each sentence
    x <- left_join(x,
                   filter(x, token == ",") %>%
                   group_by(sigle, sentenceNum) %>%
                   summarize(commas = n()),
                   by = c("sigle", "sentenceNum")) %>%
        ## if there is no comma, set it to 1 (not 0, since we devide by it)
        replace_na(list(commas = as.integer(1)))
    ## b) then count commas between token and marker
    x <- left_join(x,
                   # use cross product to count commas for each row
                   filter(crs,
                          tokenNum.y >= pmin(tokenNum.x, markerNum.x),
                          tokenNum.y <= pmax(markerNum.x, tokenNum.x),
                          token.y == ",")
                   %>% select(tokenNum.x, token.x, sentenceNum, sigle, tokenNum.y, token.y)
                   %>% group_by(sigle, sentenceNum, tokenNum.x)
                   %>% summarize(dmc = n()),
                   by = c("sigle", "sentenceNum", "tokenNum" = "tokenNum.x")) %>%
        replace_na(list(dmc = as.integer(0))) %>% # 0 dmc was NA until now
        ## c) make 0 <= dmc <= 1
        mutate(norm_dmc = 1 - (dmc / commas))
    x
}

other_column_name <- function(col_name, prefix = "w_", other_prefix = "norm_") {
    paste0(other_prefix, substring(col_name, str_length(prefix) + 1))
}

sum_weighted_features <- function(features, weights,
                                  weight_col_prefix = "w_",
                                  norm_col_prefix = "norm_") {
    x <- crossing(features, weights)
    x <- mutate(x, sum = 0) # use sum column as helper column
    cols <- keep(colnames(weights), function(c) startsWith(c, weight_col_prefix))
    for (i in seq_along(cols)) {
        col <- cols[i]
        norm_col <- other_column_name(col, weight_col_prefix, norm_col_prefix)
        x[[col]] <- x[[col]] * x[[norm_col]]
        x$sum <- x$sum + x[[col]]
    }
    x$sum <- x$sum + x$bias # TODO: sigmoid
    x
}

annotate_example_head <- function(features, weights,
                                  weight_col_prefix = "w_",
                                  norm_col_prefix = "norm_") {
    ## sum up weighted features
    x <- sum_weighted_features(features, weights, weight_col_prefix, norm_col_prefix)
    ## annotate marker
    x <- mutate(x, is_marker = if_else(tokenNum %in% c(markerNum, markerNum + 1),
                                       TRUE, FALSE))
    ## set sum of marker tokens to 0
    x <- mutate(x, sum = if_else(is_marker, 0, sum))
    ## get the maximum sum
    x <- left_join(x, group_by(x, sigle, sentenceNum) %>%
                      summarize(is_head = max(sum)))
    ## annotate example head
    x <- mutate(x, is_head = if_else(sum == is_head, TRUE, FALSE))
    x
}
