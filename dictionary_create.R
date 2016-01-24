library(jiebaR)
library(dplyr)
library(parallel)

cl <- makeCluster(4)

words <- '吃葡萄不吐葡萄皮不吃葡萄倒吐葡萄皮'

n <- nchar(words)

st <- 1:n

clusterExport(cl, 'words')
clusterExport(cl, 'st')

system.time(seg_words <- parSapply(cl, 0:4, function(i){
    substring(words, st, i + st)
}))

system.time(word_dic <- table(unlist(seg_words)))
rm(seg_words)
word_dic <- word_dic / n
dic_names <- names(word_dic)

clusterExport(cl, 'dic_names')
clusterEvalQ(cl, library(magrittr))
clusterEvalQ(cl, entropyCaculate_t <- function(vec) {
    vec <- table(vec)
    p <- vec / sum(vec)
    sum(-p * log(p))
})

system.time(free <- parSapply(cl, dic_names, function(w){
    pre <- gregexpr(w, words, fixed = TRUE)[[1]] %>% `-`(1) %>% substring(words, ., .) %>% entropyCaculate_t()
    suff <- gregexpr(w, words, fixed = TRUE)[[1]] %>% `+`(nchar(w)) %>% substring(words, ., .) %>% entropyCaculate_t()
    return(min(pre, suff))
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

# ================================================================
cohesion <- do.call('rbind', word_cohesion)
cohesion <- data.frame(word = rownames(cohesion), cohesion, row.names = NULL) %>% 
    arrange(desc(cohesion))

freedom <- do.call('rbind', word_freedom)
freedom <- data.frame(word = rownames(freedom), freedom, row.names = NULL) %>% 
    arrange(desc(freedom))

# stopCluster(cl)












