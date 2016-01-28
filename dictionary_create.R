library(jiebaR)
library(dplyr)
library(parallel)
library(tm)

stopwords_path <- 'C:/Program Files/R/R-3.2.2/library/jiebaRD/dict/stop_words.utf8'
stopwords_cn <- readLines(file(stopwords_path, 'r'), encoding = 'utf-8') %>% 
    iconv(from = 'utf-8', to = 'gbk') %>% 
    `[`(119:868) %>% 
    paste(collapse = '|')

words <- readLines(file('Matthew.txt', 'r')) %>%
    paste0(collapse = '') %>%
    removePunctuation() %>%
    removeNumbers() %>%
    stripWhitespace() %>%
    gsub('[a-zA-Z]', '', .) %>% 
    gsub(' ', '', .) %>% 
    gsub(stopwords_cn, '', .)

cl <- makeCluster(4)

# words <- '吃葡萄不吐葡萄皮不吃葡萄倒吐葡萄皮'

n <- nchar(words)

st <- 1:n

clusterExport(cl, 'words')
clusterExport(cl, 'st')

# 用户 系统 流逝 
# 0.11 0.00 5.86 
system.time(seg_words <- parSapply(cl, 0:4, function(i){
    substring(words, st, i + st)
}))

# 用户 系统 流逝 
# 0.11 0.00 5.86 
system.time(word_dic <- table(unlist(seg_words)))
rm(seg_words)
word_dic <- word_dic / n
dic_names <- names(word_dic)

clusterExport(cl, 'dic_names')
invisible(clusterEvalQ(cl, library(magrittr)))
invisible(clusterEvalQ(cl, entropyCaculate <- function(vec) {
    vec <- table(vec)
    p <- vec / sum(vec)
    sum(-p * log(p))
}))

  # 用户   系统   流逝 
  # 0.26   0.00 185.01 
system.time(free <- parSapply(cl, dic_names, function(w){
    pos <- gregexpr(w, words, fixed = FALSE)[[1]]
    pre <- substring(words, pos - 1, pos - 1) %>% entropyCaculate()
    suff <- substring(words, pos + nchar(w), pos + nchar(w)) %>% entropyCaculate()
    return(min(pre, suff))
}))

stopCluster(cl)

# ================================================================

# clusterExport(cl, 'word_dic')
# system.time(pmi <- parSapply(cl, dic_names, function(w){
#     n <- nchar(w)
#     if (n == 1) {
#         return(0) # single word mutual information is 1?
#     } else {
#         
#         left <- substring(w, 1, 1:(n - 1))
#         right <- substring(w, 2:n, n)
#         
#         return(mapply(function(x, y){
#             log(word_dic[w] / (word_dic[x] * word_dic[y]))
#             }, left, right) %>% min())
#     }
# }))

pmi <- word_dic

test <- dic_names[nchar(dic_names) == 2]
pmi[test] <- word_dic[substr(test, 1, 1)] * word_dic[substr(test, 2, 2)]

test <- dic_names[nchar(dic_names) == 3]
pmi[test] <- pmax(word_dic[substring(test, 1, 1)] * word_dic[substring(test, 2, 3)], 
                  word_dic[substring(test, 1, 2)] * word_dic[substring(test, 3, 3)])

test <- dic_names[nchar(dic_names) == 4]
pmi[test] <- pmax(word_dic[substring(test, 1, 1)] * word_dic[substring(test, 2, 4)], 
                  word_dic[substring(test, 1, 2)] * word_dic[substring(test, 3, 4)], 
                  word_dic[substring(test, 1, 3)] * word_dic[substring(test, 4, 4)])

test <- dic_names[nchar(dic_names) == 5]
pmi[test] <- pmax(word_dic[substring(test, 1, 1)] * word_dic[substring(test, 2, 5)], 
                  word_dic[substring(test, 1, 2)] * word_dic[substring(test, 3, 5)], 
                  word_dic[substring(test, 1, 3)] * word_dic[substring(test, 4, 5)], 
                  word_dic[substring(test, 1, 4)] * word_dic[substring(test, 5, 5)])

pmi <- log(word_dic / pmi)

# ================================================================

pmi_df <- data.frame(word = names(pmi), pmi = as.vector(pmi), stringsAsFactors = FALSE)
free_df <- data.frame(word = names(free), free = free, 
                      row.names = NULL, stringsAsFactors = FALSE)
word_df <- inner_join(pmi_df, free_df, by = 'word')

test <- word_df %>% 
    filter(free >= 0.001 & pmi >= 0) %>% 
    arrange(desc(pmi), desc(free))








