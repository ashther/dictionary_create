library(dplyr)
data_path <- 'data/Matthew.txt'
stopwords_path <- 'dict/stop_words.utf8'

Rcpp::sourceCpp('entropyCaculateCpp.cpp')

wordsPrepare <- function(data_path, stopwords_path) {
  require(magrittr)
  require(stringi)
  stopwords_cn <- stri_read_lines(stopwords_path) %>% 
    `[`(156:877) %>% 
    stri_c(collapse = '|')
  stopwords_cn <- stri_c(stopwords_cn, '|的|了')
  
  stri_read_lines(data_path) %>%
    stri_c(collapse = '') %>% 
    stri_replace_all('', regex = '[\\W[:punct:][:space:]0-9a-zA-Z]')
    # stri_replace_all('', regex = stopwords_cn)
}

entropyCaculate <- function(vec) {
  vec <- table(vec)
  p <- vec / sum(vec)
  sum(-p * log(p))
}

dicCreate <- function(words) {
  require(data.table)
  require(dplyr)
  require(tibble)
  require(stringi)
  require(stringr)
  n <- nchar(words)
  st <- 1:n
  print(sprintf('segment start on %s', format(Sys.time(), '%M:%OS3')))
  seg_words <- lapply(0:4, function(i){
    stri_sub(words, st, i + st)
  })
  print(sprintf('segment finish on %s', format(Sys.time(), '%M:%OS3')))
  
  print(sprintf('word_free start on %s', format(Sys.time(), '%M:%OS3')))
  word_free <- rbindlist(lapply(unique(seg_words[[1]]), function(x) {
    idx <- which(seg_words[[1]] == x)
    pre_idx <- idx - 1
    suff_idx <- idx + 1
    pre <- seg_words[[1]][ifelse(pre_idx < 1, 1, pre_idx)]
    suff <- seg_words[[1]][ifelse(suff_idx > n, n, suff_idx)]
    data.table(w = x, 
               pre = entropyCaculateCpp(pre), 
               suff = entropyCaculateCpp(suff))
  }))
  print(sprintf('word_free finish on %s', format(Sys.time(), '%M:%OS3')))
  
  print(sprintf('free start on %s', format(Sys.time(), '%M:%OS3')))
  word_freq_dt <- as.data.table(unlist(seg_words))[, .(freq = .N/n), V1]
  colnames(word_freq_dt)[1] <- 'word'
  setkeyv(word_freq_dt, 'word')
  
  word_freq_dt[, w := str_sub(word, 1, 1)]
  word_freq_dt[word_free, pre := i.pre, on = 'w']
  word_freq_dt[, w := str_sub(word, -1, -1)]
  word_freq_dt[word_free, suff := i.suff, on = 'w']
  word_freq_dt[, free := pmin(pre, suff)]
  print(sprintf('free finish on %s', format(Sys.time(), '%M:%OS3')))
  
  print(sprintf('pmi start on %s', format(Sys.time(), '%M:%OS3')))
  pmi <- rep(0, nrow(word_freq_dt))
  names(pmi) <- word_freq_dt$word
  
  test <- word_freq_dt[nchar(word) == 2, word]
  pmi[test] <- word_freq_dt[stri_sub(test, from = 1, length = 1), freq] * 
                word_freq_dt[stri_sub(test, from = -1, length = 1), freq]
  
  test <- word_freq_dt[nchar(word) == 3, word]
  pmi[test] <- pmax(word_freq_dt[stri_sub(test, from = 1, length = 1), freq] * 
                      word_freq_dt[stri_sub(test, from = -1, length = 2), freq], 
                    word_freq_dt[stri_sub(test, from = 1, length = 2), freq] * 
                      word_freq_dt[stri_sub(test, from = -1, length = 1), freq])
  
  test <- word_freq_dt[nchar(word) == 4, word]
  pmi[test] <- pmax(word_freq_dt[stri_sub(test, from = 1, length = 1), freq] * 
                      word_freq_dt[stri_sub(test, from = -1, length = 3), freq], 
                    word_freq_dt[stri_sub(test, from = 1, length = 2), freq] * 
                      word_freq_dt[stri_sub(test, from = -1, length = 2), freq], 
                    word_freq_dt[stri_sub(test, from = 1, length = 3), freq] * 
                      word_freq_dt[stri_sub(test, from = -1, length = 1), freq])
  
  test <- word_freq_dt[nchar(word) == 5, word]
  pmi[test] <- pmax(word_freq_dt[stri_sub(test, from = 1, length = 1), freq] * 
                      word_freq_dt[stri_sub(test, from = -1, length = 4), freq], 
                    word_freq_dt[stri_sub(test, from = 1, length = 2), freq] * 
                      word_freq_dt[stri_sub(test, from = -1, length = 3), freq], 
                    word_freq_dt[stri_sub(test, from = 1, length = 3), freq] * 
                      word_freq_dt[stri_sub(test, from = -1, length = 2), freq], 
                    word_freq_dt[stri_sub(test, from = 1, length = 4), freq] * 
                      word_freq_dt[stri_sub(test, from = -1, length = 1), freq])
  
  pmi <- ifelse(pmi == 0, 0, log(word_freq_dt$freq / pmi))
  print(sprintf('pmi finish on %s', format(Sys.time(), '%M:%OS3')))
  
  pmi_df <- as.data.table(enframe(pmi, 'word', 'pmi'))
  
  word_freq_dt[
    , f := as.numeric(scale(free)) + as.numeric(scale(pmi))
  ][order(-f)]
}

words <- dataGetFromSQLite(
  'e:/work/meta.db', 
  sql = 'select episode, scene, section, word from episode_word where stopWord == 0 and symbol == 0 and word <> "";'
)

words_section <- dataGetFromSQLite(
  'e:/work/meta.db', 
  sql = 'select episode, scene, section, content from episode_section;'
)

words_for_dict <- wordsPrepare('words_section.txt', 'dict/stop_words.utf8')
words_for_dict_len <- nchar(words_for_dict)
words_dict <- dicCreate(words_for_dict)

words_0 <- words_dict %>% 
  filter(free > 0 & pmi > 0 & nchar(word) > 1) %>% 
  mutate(f = as.numeric(scale(pmi)) + as.numeric(scale(free))) %>% 
  filter(f > 0) %>% 
  filter(f > quantile(.$f, 0.8))

words_1 <- dicCreate(stri_sub(words_for_dict, from = 1, 
                                 length = floor(words_for_dict_len/4))) %>% 
  filter(free > 0 & pmi > 0 & nchar(word) > 1) %>% 
  mutate(f = as.numeric(scale(pmi)) + as.numeric(scale(free))) %>% 
  filter(f > 0) %>% 
  filter(f > quantile(.$f, 0.9))
test <- left_join(select(words_1, word, freq), 
                  select(words_0, word, freq), by = 'word')
temp <- (test$freq.x + mean(test$freq.x, na.rm = TRUE)) /
  (test$freq.y + mean(test$freq.y, na.rm = TRUE))
test %>% add_column(temp) %>% arrange(-temp) %>% head(30) %>% as.data.frame()

words_2 <- dicCreate(stri_sub(words_for_dict, from = floor(words_for_dict_len/4) * 1, 
                              length = floor(words_for_dict_len/4))) %>% 
  filter(free > 0 & pmi > 0 & nchar(word) > 1) %>% 
  mutate(f = as.numeric(scale(pmi)) + as.numeric(scale(free))) %>% 
  filter(f > 0) %>% 
  filter(f > quantile(.$f, 0.9))

words_3 <- dicCreate(stri_sub(words_for_dict, from = floor(words_for_dict_len/4) * 2, 
                              length = floor(words_for_dict_len/4))) %>% 
  filter(free > 0 & pmi > 0 & nchar(word) > 1) %>% 
  mutate(f = as.numeric(scale(pmi)) + as.numeric(scale(free))) %>% 
  filter(f > 0) %>% 
  filter(f > quantile(.$f, 0.9))

words_4 <- dicCreate(stri_sub(words_for_dict, from = floor(words_for_dict_len/4) * 3, 
                              length = floor(words_for_dict_len/4))) %>% 
  filter(free > 0 & pmi > 0 & nchar(word) > 1) %>% 
  mutate(f = as.numeric(scale(pmi)) + as.numeric(scale(free))) %>% 
  filter(f > 0) %>% 
  filter(f > quantile(.$f, 0.9))
