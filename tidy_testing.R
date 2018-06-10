install.packages("gutenbergr", lib="C:/R/Packages")
library(janeaustenr)
library(dplyr)
library(stringr)
data(stop_words)

original_books <- austen_books() %>%
    group_by(book) %>%
    mutate(linenumber = row_number(),
           chapter = cumsum(str_detect(text, regex("^chapter [\\divxlc]",
                                                   ignore_case = TRUE)))) %>%
    ungroup()

original_books

library(tidytext)
tidy_books <- original_books %>%
    unnest_tokens(word, text)

tidy_books

tidy_books <- tidy_books %>%
    anti_join(stop_words)

test <- commentsDf %>%
    group_by(doc_id) %>%
    mutate(linenumber = row_number()) %>%
    ungroup()

test1 <- test %>%
    unnest_tokens(word, text)

test1 <- test1 %>%
    anti_join(stop_words)

library(dplyr)
tidy_books %>%
    dplyr::count(word, sort = TRUE) 

library(ggplot2)

tidy_books %>%
    count(word, sort = TRUE) %>%
    filter(n > 600) %>%
    mutate(word = reorder(word, n)) %>%
    ggplot(aes(word, n)) +
    geom_col() +
    xlab(NULL) +
    coord_flip()

test1 %>%
    count(word, sort = TRUE) %>%
    filter(n > 600) %>%
    mutate(word = reorder(word, n)) %>%
    ggplot(aes(word, n)) +
    geom_col() +
    xlab(NULL) +
    coord_flip()

nrc_joy <- get_sentiments("nrc") %>% 
    filter(sentiment == "joy")

library(tidyr)

jane_austen_sentiment <- tidy_books %>%
    inner_join(get_sentiments("bing")) %>%
    count(book, index = linenumber %/% 80, sentiment) %>%
    spread(sentiment, n, fill = 0) %>%
    mutate(sentiment = positive - negative)

ggplot(jane_austen_sentiment, aes(index, sentiment, fill = book)) +
    geom_col(show.legend = FALSE) +
    facet_wrap(~book, ncol = 2, scales = "free_x")

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


library(gutenbergr)
tmp <- gutenberg_metadata
