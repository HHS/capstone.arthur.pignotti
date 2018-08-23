#### Analysis of Scoring ####








#### Sentiment Analysis Testing ####

nrc_joy <- get_sentiments("nrc") %>% 
    filter(sentiment == "joy")

comment.unigrams <- commentsDf %>%
    unnest_tokens(word, Text) %>%
    rowid_to_column("index") %>%
    filter(!(str_detect(word, regex("^http"))),
           !(str_detect(word, regex("^www")))) %>% 
    anti_join(stop_words) %>%
    anti_join(cms_stop) %>%
    anti_join(bigram.index, by = c("index" = "index1")) %>%
    anti_join(bigram.index, by = c("index" = "index2")) %>%
    mutate(word = wordStem(word))

test_sentiment <- comment.unigrams %>%
    inner_join(get_sentiments("bing")) %>%
    filter(Page.Number != 0) %>%
    count(Document.ID, Page.Number, sentiment) %>%
    spread(sentiment, n, fill = 0) %>%
    mutate(sentiment = positive - negative) %>%
    #filter(abs(sentiment)>10) %>%
    rowid_to_column("index")

ggplot(test_sentiment, aes(index, sentiment, fill = Document.ID)) +
    geom_col(show.legend = FALSE)

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
