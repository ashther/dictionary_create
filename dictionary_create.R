library(jiebaR)
library(dplyr)
library(parallel)

entropyCaculate_t <- function(vec) {
    p <- vec / sum(vec)
    sum(-p * log(p))
}

cl <- makeCluster(4)

words <- '吃葡萄不吐葡萄皮不吃葡萄倒吐葡萄皮'

n <- nchar(words)
#words <- strsplit(test, '') %>% unlist() %>% unique()

st <- 1:n

clusterExport(cl, 'words')
clusterExport(cl, 'st')

system.time(seg_words <- parSapply(cl, 0:4, function(i){
    substring(words, st, i + st)
}))

system.time(word_dic <- table(unlist(seg_words)))
rm(seg_words)
word_dic <- word_dic / sum(word_dic)

system.time(pre_words <- parSapply(cl, st, function(i){
    substring(words, i - 1, i - 1)
}))
pre_words <- replicate(5, pre_words)

system.time(suff_words <- parSapply(cl, 0:4, function(i){
    substring(words, st + i + 1, st + i + 1)
}))
# ================================================================





clusterEvalQ(cl, require(magrittr))
clusterExport(cl, 'word_dic')

system.time(test <- parSapply(cl, names(word_dic), function(w){
    left <- 1:(nchar(w) - 1)
    right <- rev(left)
    left <- substring(w, 1, left)
    right <- substring(w, nchar(w) - right + 1, nchar(w))
    
    if (nchar(w) == 1) {
        return(0) # single word mutual information is 1?
    } else {
        return(mapply(function(x, y){
            log(word_dic[w] / (word_dic[x] * word_dic[y]))
            }, left, right) %>% min())
    }
}))

# word_list <- vector('list', length = length(word_dic))
# names(word_list) <- names(word_dic)
# for (i in names(word_list)) {
#     word_list[[i]] <- unname(word_dic[i]) / n
# }
# rm(word_dic)

word_cohesion <- vector('list', length = length(word_dic))
names(word_cohesion) <- names(word_dic)

word_freedom <- vector('list', length = length(word_dic))
names(word_freedom) <- names(word_dic)

pb <- txtProgressBar(max = length(names(word_freedom)), style = 3)
for (i in names(word_freedom)) {
    temp <- gregexpr(i, words)[[1]]
    pre_word <- substring(words, temp - 1, temp - 1) %>% 
        table() %>% entropyCaculate_t()
    suff_word <- substring(words, temp + nchar(i), temp + nchar(i)) %>% 
        table() %>% entropyCaculate_t()
    word_freedom[[i]] <- min(pre_word, suff_word)
    
    left <- 1:(nchar(i) - 1)
    right <- rev(left)
    left <- substring(i, 1, left)
    right <- substring(i, nchar(i) - right + 1, nchar(i))
    
    if (nchar(i) == 1) {
        word_cohesion[[i]] <- 0 # single word mutual information is 1?
    } else {
        word_cohesion[[i]] <- mapply(function(x, y){
            log(word_dic[i] / (word_dic[x] * word_dic[y]))
        }, left, right) %>% min()
    }
    
    setTxtProgressBar(pb, value = which(i == names(word_freedom)))
}
close(pb)


cohesion <- do.call('rbind', word_cohesion)
cohesion <- data.frame(word = rownames(cohesion), cohesion, row.names = NULL) %>% 
    arrange(desc(cohesion))

freedom <- do.call('rbind', word_freedom)
freedom <- data.frame(word = rownames(freedom), freedom, row.names = NULL) %>% 
    arrange(desc(freedom))














