############################
# Create Corpus Using Tidy #
############################

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



nrc_joy <- get_sentiments("nrc") %>% 
    filter(sentiment == "joy")

test_sentiment <- test1 %>%
    inner_join(get_sentiments("bing")) %>%
    count(Document.ID, index = Page.Number %/% 80, sentiment) %>%
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

ggplot(jane_austen_sentiment, aes(index, sentiment)) +
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
