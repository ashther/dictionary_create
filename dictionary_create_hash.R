library(jiebaR)
library(dplyr)
library(hash)

entropyCaculate <- function(vec) {
    vec <- table(vec)
    p <- vec / sum(vec)
    sum(-p * log(p))
}

words <- '吃葡萄不吐葡萄皮不吃葡萄倒吐葡萄皮'
n <- nchar(words)

st <- 1:n

word_map <- hash()
pre_map <- hash()
suff_map <- hash()
free_map <- hash()
pmi_map <- hash()

pb1 <- txtProgressBar(max = n, style = 3)
for (i in 1:n) {
    for (j in 0:4) {
        term <- substr(words, i, i + j)
        if ((i + j) > n) {
            break
        }
        
        temp_pre <- substr(words, i - 1, i - 1)
        term_pre <- ifelse(identical(temp_pre, ''), ' ', temp_pre)
        
        temp_suff <- substr(words, i + j + 1, i + j + 1)
        term_suff <- ifelse(identical(temp_suff, ''), ' ', temp_suff)
        
        if (has.key(term, word_map)) {
            word_map[[term]] <- word_map[[term]] + 1
        } else {
            word_map[[term]] <- 1
        }
        
        if (has.key(term, pre_map)) {
            pre_map[[term]] <- append(pre_map[[term]], term_pre)
        } else {
            pre_map[[term]] <- term_pre
        }
        
        if (has.key(term, suff_map)) {
            suff_map[[term]] <- append(suff_map[[term]], term_suff)
        } else {
            suff_map[[term]] <- term_suff
        }
    }
    setTxtProgressBar(pb1, value = i)
}
close(pb1)

values(word_map) <- values(word_map) / n


pb2 <- txtProgressBar(max = length(keys(word_map)), style = 3)
for (i in keys(word_map)) {
    free_map[[i]] <- min(entropyCaculate(pre_map[[i]]), 
                         entropyCaculate(suff_map[[i]]))
    
    left <- 1:(nchar(i) - 1)
    right <- rev(left)
    left <- substring(i, 1, left)
    right <- substring(i, nchar(i) - right + 1, nchar(i))
    
    if (nchar(i) == 1) {
        pmi_map[[i]] <- 0 # single word mutual information is 1?
    } else {
        pmi_map[[i]] <- mapply(function(x, y){
            log(word_map[[i]] / (word_map[[x]] * word_map[[y]]))
        }, left, right) %>% min()
    }
    setTxtProgressBar(pb2, value = which(i == keys(word_map)))
}
close(pb2)


free <- data.frame(word = keys(free_map), free = values(free_map), row.names = NULL) %>% 
    filter(free > 0) %>% 
    arrange(desc(free))

pmi <- data.frame(word = keys(pmi_map), pmi = values(pmi_map), row.names = NULL) %>% 
    filter(pmi >= 0) %>% 
    arrange(desc(pmi))

inner_join(pmi, free, by = 'word') %>% arrange(desc(pmi))

# word cohesion   freedom
# 1 吐葡萄皮 2.140066 0.6931472
# 2   吃葡萄 1.446919 0.6931472
# 3     葡萄 1.446919 0.6931472
# 4       不 0.000000 0.6931472











