#####################
# Initial Setup     #
#####################
.libPaths( c("C:/R/Packages", .libPaths()) ) #add extra library location
setwd("C:/Users/P6BQ/Desktop/capstone.arthur.pignotti") #local location of github repo



############################
# Create Corpus Using Tidy #
############################

commentsDf <- read.csv("Data/commentsDf.csv", stringsAsFactors = FALSE)

library(dplyr)
library(stringr)
library(ggplot2)
library(tidytext)
library(tidyverse)

data(stop_words)
test <- commentsDf %>%
    group_by(Document.ID) %>%
    mutate(linenumber = row_number()) %>%
    ungroup()


comment.words <- test %>%
    unnest_tokens(word, Text)

comment.words <- comment.words %>%
    anti_join(stop_words)

cms_stop <- data.frame(word = c("cms","medicare","ma", "plan", "care", "beneficiaries", "advantage", "proposed", "rule", "health", "plans", "md", "baltimore", "seema", "verma", "washington", "dc", "boulevardbaltimore"), stringsAsFactors = FALSE)

comment.words <- comment.words %>%
    anti_join(cms_stop)

#######################
# Spell Check Testing #
#######################
library(hunspell)
comment.words <- comment.words %>%
    filter(!(str_detect(word, regex("^http"))),
           !(str_detect(word, regex("^www"))))

spell.test <- hunspell_find(comment.words$word)
spell.suggest <- hunspell_su(comment.words$word)
spell.count <- as.data.frame(matrix(unlist(spell.test), byrow = TRUE))
spell.suggest <- hunspell_su(spell.count$V1)

spell.count %>%
    count(V1, sort = TRUE) %>%
    filter(n <= 30 & n >= 25) %>%
    #summarise(sum(n))
    mutate(V1 = reorder(V1, n)) %>%
    ggplot(aes(V1, n)) +
    geom_col() +
    xlab(NULL) +
    coord_flip()

misspelled <- spell.count %>%
    count(V1, sort = TRUE) %>%
    mutate(V1 = reorder(V1, n))

misspell.find <- comment.words %>%
    filter(str_detect(word, regex("affordabilityimplementation")))

comment.words$word <- str_replace_all(comment.words$word, "ahcancal", "ahca/ncal")
comment.words$word <- str_replace_all(comment.words$word, regex("[a-zA-Z].[a-zA-Z]"), "measure level")

write.csv(misspelled, file = "Data/misspelled.csv", row.names = FALSE)
write.csv(misspell.find, file = "Data/misspellTest.csv", row.names = FALSE)


#######################
# TF-IDF Testing      #
#######################
word_count <- test1 %>%
    count(Document.ID, word, sort = TRUE) %>%
    ungroup()

word_total <- word_count %>% 
    group_by(Document.ID) %>% 
    summarize(total = sum(n))

words <- left_join(word_count, word_total)

words_tf_idf <- words %>%
    bind_tf_idf(word, Document.ID, n)

words_tf_idf %>%
    filter(total > 8000) %>%
    arrange(desc(tf_idf)) %>%
    mutate(word = factor(word, levels = rev(unique(word)))) %>% 
    group_by(Document.ID) %>% 
    top_n(15) %>% 
    ungroup %>%
    ggplot(aes(word, tf_idf, fill = Document.ID)) +
    geom_col(show.legend = FALSE) +
    labs(x = NULL, y = "tf-idf") +
    facet_wrap(~Document.ID, ncol = 2, scales = "free") +
    coord_flip()

library(tm)
words_dtm <- cast_dtm(words_tf_idf, document = Document.ID, term = word, value = tf_idf)


#######################
# Bigram Testing      #
#######################
bigram_count <- test %>%
    unnest_tokens(bigram, Text, token = "ngrams", n = 2) %>%
    separate(bigram, c("word1", "word2"), sep = " ") %>%
    filter(!word1 %in% stop_words$word) %>%
    filter(!word2 %in% stop_words$word) %>% 
    count(Document.ID, word1, word2, sort = TRUE) %>%
    unite(bigram, word1, word2, sep = " ")

bigram_total <- bigram_count %>% 
    group_by(bigram) %>% 
    summarize(total = sum(n)) %>%
    arrange(desc(total))

bigram_total <- bigram_count %>% 
    group_by(Document.ID) %>% 
    summarize(total = sum(n))

bigrams <- left_join(bigram_count, bigram_total)

bigram_tf_idf <- bigrams %>%
    bind_tf_idf(bigram, Document.ID, n) %>%
    arrange(desc(tf_idf))

bigram_tf_idf %>%
    filter(total > 7000) %>%
    arrange(desc(tf_idf)) %>%
    mutate(bigram = factor(bigram, levels = rev(unique(bigram))))%>% 
    group_by(Document.ID) %>% 
    top_n(15) %>% 
    ungroup %>%
    ggplot(aes(bigram, tf_idf, fill = Document.ID)) +
    geom_col(show.legend = FALSE) +
    labs(x = NULL, y = "tf-idf") +
    facet_wrap(~Document.ID, ncol = 2, scales = "free") +
    coord_flip()


####################
# Network Graph    #
####################

library(igraph)
library(ggraph)
set.seed(2017)

bigram_graph <- test %>%
    unnest_tokens(bigram, Text, token = "ngrams", n = 2) %>%
    separate(bigram, c("word1", "word2"), sep = " ") %>%
    filter(!word1 %in% stop_words$word) %>%
    filter(!word2 %in% stop_words$word) %>% 
    count(word1, word2, sort = TRUE) %>% 
    filter(n > 1000) %>%
    graph_from_data_frame()

a <- grid::arrow(type = "closed", length = unit(.1, "inches"))

ggraph(bigram_graph, layout = "fr") +
    geom_edge_link(aes(edge_alpha = n), show.legend = FALSE,
                   arrow = a, end_cap = circle(.01, 'inches')) +
    geom_node_point(color = "lightblue", size = 3) +
    geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
    theme_void()


##########################
# Pairwaise Correlations #
##########################

library(widyr)

word_cors <- test1 %>%
    group_by(word) %>%
    filter(n() >= 100) %>%
    pairwise_cor(word, Document.ID, sort = TRUE)


word_cors %>%
    filter(item1 %in% c("drug", "dir", "pharmacy", "pbm")) %>%
    group_by(item1) %>%
    top_n(10) %>%
    ungroup() %>%
    mutate(item2 = reorder(item2, correlation)) %>%
    ggplot(aes(item2, correlation)) +
    geom_bar(stat = "identity") +
    facet_wrap(~ item1, scales = "free") +
    coord_flip()

##############################
# Sentiment Analysis Testing #
##############################
nrc_joy <- get_sentiments("nrc") %>% 
    filter(sentiment == "joy")

test_sentiment <- test1 %>%
    inner_join(get_sentiments("bing")) %>%
    count(Document.ID, Page.Number, sentiment) %>%
    spread(sentiment, n, fill = 0) %>%
    mutate(sentiment = positive - negative) %>%
    filter(abs(sentiment)>200)

ggplot(test_sentiment, aes(index, sentiment, fill = Document.ID)) +
    geom_col(show.legend = FALSE) +
    facet_wrap(~Document.ID, ncol = 2, scales = "free_x")

test_sentiment <- test1 %>%
    inner_join(get_sentiments("bing")) %>%
    count(sentiment) %>%
    spread(sentiment, n, fill = 0) %>%
    mutate(sentiment = positive - negative)

ggplot(jane_austen_sentiment, aes(Page.Number, sentiment)) +
    geom_col(show.legend = FALSE) 

bing_word_counts <- test1 %>%
    inner_join(get_sentiments("bing")) %>%
    count(word, sentiment, sort = TRUE) %>%
    ungroup()

bing_word_counts %>%
    group_by(sentiment) %>%
    top_n(10) %>%
    ungroup() %>%
    mutate(word = reorder(word, n)) %>%
    ggplot(aes(word, n, fill = sentiment)) +
    geom_col(show.legend = FALSE) +
    facet_wrap(~sentiment, scales = "free_y") +
    labs(y = "Contribution to sentiment",
         x = NULL) +
    coord_flip()
