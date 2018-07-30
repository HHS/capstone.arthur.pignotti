#### wordcount: Function to count words ####
wordcount <- function(str) {
    sapply(gregexpr("\\b\\W+\\b", str, perl=TRUE), function(x) sum(x>0) ) + 1 
}

calcDocSim <- function(dsn) {
    similarity <- dsn %>%
        textstat_simil(margin = "documents",
                       method = "cosine") %>%
        as.dist() %>%
        tidy() %>%
        arrange(desc(distance))
    
    names(similarity) <- c("doc1", "doc2", "distance")
    
    return(similarity)
}

createTFIDF <- function(df, group, var, threshold) {
    count <- df %>%
        group_by(!! group) %>%
        count(!! group, !! var, sort = TRUE) %>%
        ungroup()
    
    total <- count %>% 
        group_by(!! group) %>% 
        summarize(total = sum(n))
    
    combine <- left_join(count, total) %>%
        filter(total > !! threshold)
    
    ret <- combine %>%
        bind_tf_idf(!! word, !! group, n)
    
    return(ret)
}

createTFIDF <- function(df, group, var) {
    group <- enquo(group)
    var <- enquo(var)
    
    count <- df %>%
        group_by(!! group) %>%
        count(!! var, sort = TRUE) %>%
        ungroup()
    
    total <- count %>% 
        group_by(!! group) %>% 
        summarize(total = sum(n))
    
    combine <- left_join(count, total)
    
    ret <- combine %>%
        bind_tf_idf(!! var, !! group, n)
    
    return(ret)
}

elementMax <- function(list){
    maxEl = 0
    for (i in 1:length(list)){
        if (length(list[[i]]) > maxEl) {
            maxEl <- length(list[[i]])
        }
    }
    return(maxEl)
}