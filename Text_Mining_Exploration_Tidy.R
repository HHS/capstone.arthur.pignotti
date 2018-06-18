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
library(tidyr)

data(stop_words)
test <- commentsDf %>%
    group_by(Document.ID) %>%
    mutate(linenumber = row_number()) %>%
    ungroup()


test1 <- test %>%
    unnest_tokens(word, Text)

test1 <- test1 %>%
    anti_join(stop_words)

cms_stop <- data.frame(word = c("cms","medicare","ma", "plan", "care", "beneficiaries", "advantage", "proposed", "rule", "health", "plans"), stringsAsFactors = FALSE)

test1 <- test1 %>%
    anti_join(cms_stop)

#######################
# Spell Check Testing #
#######################
library(hunspell)
spell.test <- hunspell_find(test1$word)
str(spell.test)
spell.test1 <- unique(spell.test)
spell.count <- as.data.frame(spell.test1)

test1 %>%
    count(word, sort = TRUE) %>%
    filter(n > 4000) %>%
    mutate(word = reorder(word, n)) %>%
    ggplot(aes(word, n)) +
    geom_col() +
    xlab(NULL) +
    coord_flip()

test1 %>%
    group_by(Type, word) %>%
    summarise(counts = n()) %>%
    filter(Type == "Health Plan", counts > 250) %>%
    mutate(word = reorder(word, counts)) %>%
    ggplot(aes(word, counts)) +
    geom_col() +
    xlab(NULL) +
    coord_flip()


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

testing <- words_tf_idf %>%
    filter(total > 8000) %>%
    count(Document.ID)

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
