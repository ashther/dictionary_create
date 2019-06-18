require(data.table)
require(stringi)
data_path <- 'data/Matthew.txt'
stopwords_path <- 'dict/stop_words.utf8'

Rcpp::sourceCpp('entropyCaculate.cpp')

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

# entropyCaculate <- function(vec) {
#   vec <- table(vec)
#   p <- vec / sum(vec)
#   sum(-p * log(p))
# }

expr_paste <- function(n) {
  left <- seq_len(n - 1)
  right <- rev(left)
  template <- 'word_freq_dt[stri_sub(word_names, from = 1, length = %s), freq] * word_freq_dt[stri_sub(word_names, from = -1, length = %s), freq]'
  paste0('pmax(', paste(sprintf(template, left, right), collapse = ', '), ')')
}

word_find <- function(string_long, max_len = 5L, min_freq = 5e-05) {
  
  # segment long strings into segment
  n <- nchar(string_long)
  st <- 1:n
  seg_words <- lapply(0:(max_len - 1), function(i){
    stri_sub(string_long, st, i + st)
  })
  
  # count words frequency
  word_freq_dt <- as.data.table(unlist(seg_words))[
    , .(freq = .N/n), keyby = V1
  ][freq >= min_freq]
  setnames(word_freq_dt, 'V1', 'word')
  
  # left and right free degree
  word_single <- seg_words[[1]]
  word_single_with_freq <- word_freq_dt[nchar(word) == 1, word]
  word_free <- rbindlist(lapply(word_single_with_freq, function(x) {
    idx <- which(word_single == x)
    pre <- word_single[idx - 1]
    suff <- word_single[idx + 1]

    list(
      w    = x,
      pre  = entropyCaculate(pre),
      suff = entropyCaculate(suff)
    )
  }))
  # word_free <- as.data.table(entropyGet(word_single_with_freq, word_single))
  setkey(word_free, w)
  
  word_freq_dt[, w := stri_sub(word, 1, 1)]
  word_freq_dt[word_free, pre := i.pre, on = 'w']
  word_freq_dt[, w := stri_sub(word, -1, -1)]
  word_freq_dt[word_free, suff := i.suff, on = 'w']
  word_freq_dt[, free := pmin(pre, suff)]
  
  # single word pmi
  pmi <- rep(0, nrow(word_freq_dt))
  names(pmi) <- word_freq_dt$word
  for (i in 2:max_len) {
    word_names <- word_freq_dt[nchar(word) == i, word]
    pmi[word_names] <- eval(parse(text = expr_paste(i)))
  }
  pmi <- ifelse(pmi == 0, 0, log(word_freq_dt$freq / pmi))
  
  # caculate f and sort based on f
  word_freq_dt[
    , f := as.numeric(scale(free)) + as.numeric(scale(pmi))
    ][order(-f)][
      , .(word, f)
    ]
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
