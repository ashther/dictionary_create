library(jiebaR)
library(dplyr)
library(parallel)
library(tm)

stopwords_path <- 'C:/Program Files/R/R-3.2.2/library/jiebaRD/dict/stop_words.utf8'
stopwords_cn <- readLines(file(stopwords_path, 'r'), encoding = 'utf-8') %>% 
    iconv(from = 'utf-8', to = 'gbk') %>% 
    `[`(119:868) %>% 
    paste(collapse = '|')

# words <- readLines(file('Matthew.txt', 'r')) %>%
#     paste0(collapse = '') %>%
#     removePunctuation() %>%
#     removeNumbers() %>%
#     stripWhitespace() %>%
#     gsub('[a-zA-Z]', '', .) %>%
#     gsub(' ', '', .) %>%
#     gsub(stopwords_cn, '', .)

dicCreate <- function(words, thr_p = 0.01, thr_f = 0.01) {
    
    cl <- makeCluster(4)
    
    n <- nchar(words)

    st <- 1:n

    clusterExport(cl, 'words', environment())
    clusterExport(cl, 'st', environment())
    
    # seg_words
    print(sprintf('seg_words start on %s', format(Sys.time(), '%M:%OS3')))
    system.time(seg_words <- parSapply(cl, 0:4, function(i){
        substring(words, st, i + st)
    }))
    print(sprintf('seg_words finish on %s', format(Sys.time(), '%M:%OS3')))
    
    # word_dic
    print(sprintf('word_freq start on %s', format(Sys.time(), '%M:%OS3')))
    system.time(word_freq <- table(unlist(seg_words)))
    print(sprintf('word_freq finish on %s', format(Sys.time(), '%M:%OS3')))
    rm(seg_words)
    word_dic <- word_freq / n
    dic_names <- names(word_dic)
    
    clusterExport(cl, 'dic_names', environment())
    invisible(clusterEvalQ(cl, require(magrittr)))
    invisible(clusterEvalQ(cl, entropyCaculate <- function(vec) {
        vec <- table(vec)
        p <- vec / sum(vec)
        sum(-p * log(p))
    }))
    
    print(sprintf('free start on %s', format(Sys.time(), '%M:%OS3')))
    system.time(free <- parSapply(cl, dic_names, function(w){
        pos <- gregexpr(w, words, fixed = FALSE)[[1]]
        pre <- substring(words, pos - 1, pos - 1) %>% entropyCaculate()
        suff <- substring(words, pos + nchar(w), pos + nchar(w)) %>% entropyCaculate()
        return(min(pre, suff))
    }))
    print(sprintf('free finsh on %s', format(Sys.time(), '%M:%OS3')))
    
    stopCluster(cl)
    
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
    
    pmi_df <- data.frame(word = names(pmi), pmi = as.vector(pmi), stringsAsFactors = FALSE)
    free_df <- data.frame(word = names(free), free = free, 
                          row.names = NULL, stringsAsFactors = FALSE)
    word_df <- inner_join(pmi_df, free_df, by = 'word')
    
    test <- word_df %>% 
        filter(free >= thr_f & pmi >= thr_p) %>% 
        mutate(freq = as.vector(word_freq[as.character(word)])) %>% 
        arrange(desc(pmi), desc(free))
    
    return(test)
}

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





words <- readLines(file('199801.txt', 'r'))
words <- words[nchar(words) != 0]
date <- substring(words, 1, 8)
content <- substring(words, 23)
news <- data.frame(list(date = date, content = content), stringsAsFactors = FALSE) %>% 
    group_by(date) %>% 
    summarise(news = paste0(content, collapse = '')) %>% 
    ungroup()
rm(content)

news$news <- news$news %>% 
    removePunctuation() %>% 
    removeNumbers() %>% 
    stripWhitespace() %>% 
    gsub('[a-zA-Z]', '', .) %>% 
    gsub(' ', '', .) %>% 
    gsub(stopwords_cn, '', .)

dateCopmpare <- function(words_1, words_2, n) {
    words_1 <- dicCreate(words_1)
    words_2 <- dicCreate(words_2)
    
    temp <- left_join(words_1, words_2, by = 'word') %>% 
        select(word, f2 = freq.x, f1 = freq.y)
    temp[is.na(temp)] <- 0
    temp <- mutate(temp, f = f1 + f2, prob = f2 / f)
    
    avg_f <- mean(temp$f)
    avg_prob <- mean(temp$prob)
    
    result <- mutate(temp, rating = (f * prob + avg_f * avg_prob) / (f + avg_f)) %>% 
        arrange(desc(rating)) %>% 
        top_n(n, rating)
    
    return(result)
}



























