#### Initial Setup ####
.libPaths( c("C:/R/Packages", .libPaths()) ) #add extra library location
setwd("C:/Users/P6BQ/Desktop/capstone.arthur.pignotti") #local location of github repo

baseStart = 66

#Load libraries
library(tidyverse)
library(readxl)
library(tidytext)
library(tm)
library(SnowballC)
library(topicmodels)

baselineDoc <- read_excel("Data/AN_Part2 - V2.xlsx", sheet = 1)
commentsDf <- read.csv("Data/commentsDf.csv", stringsAsFactors = FALSE)
colnames(baselineDoc) <- make.names(colnames(baselineDoc))

#Load stop words
data(stop_words)

#Load CMS-specific stop words
cms_stop <- read_csv("Data/cms_stop_words.csv")

# Remove non-alpha-numeric characters
baselineDoc$Text <- gsub("[^A-z0-9 ]", "", baselineDoc$Text)


baselineDoc_section <- baselineDoc %>%
    group_by(Document.ID) %>%
    filter(Text != "",
           Paragraph.Number >= baseStart) %>%
    mutate(section = ifelse(str_detect(Text, regex("^section [A-Z]", ignore_case = TRUE)), Text, NA)) %>%
#    mutate(section = cumsum(str_detect(Text, regex("^section [A-Z]", ignore_case = TRUE)))) %>%
    filter(str_count(Text, regex("[A-z]", ignore_case = TRUE))/str_count(Text, regex("[A-z0-9]", ignore_case = TRUE)) > .5) %>% # Removes lines that are 50% or more numbers
    fill(section) %>%
    filter(!is.na(section))

baselineDoc_section <- baselineDoc_section %>%
    filter(Paragraph.Number < 626 | Paragraph.Number > 695)


bigram <- baselineDoc_section %>%
    select(Document.ID, Text) %>%
    union_all(select(commentsDf, Document.ID, Text)) %>%
    ungroup() %>%
    unnest_tokens(bigram, Text, token = "ngrams", n = 2) %>%
    separate(bigram, c("word1", "word2"), sep = " ") %>%
    anti_join(stop_words, by = c("word1" = "word")) %>%
    anti_join(stop_words, by = c("word2" = "word")) %>%
    anti_join(cms_stop, by = c("word1" = "word")) %>%
    anti_join(cms_stop, by = c("word2" = "word")) %>%
    mutate(word1 = wordStem(word1),
           word2 = wordStem(word2)) %>%
    unite(word, word1, word2, sep = " ") %>%
    select(-Document.ID) %>%
    count(word, sort = TRUE) %>%
    filter(n/length(unique(commentsDf$Comment.ID)) > .08)

bigram_index <- baselineDoc_section %>%
    unnest_tokens(bigram, Text, token = "ngrams", n = 2) %>%
    rowid_to_column("index1") %>%
    inner_join(bigram, by = c("bigram" = "word")) %>%
    mutate(index2 = index1 + 1) %>%
    select(Document.ID, bigram, index1, index2)

baseline_unigram <- baselineDoc_section %>%
    unnest_tokens(word, Text) %>%
    rowid_to_column("index") %>%
    filter(!(str_detect(word, regex("^http"))),
           !(str_detect(word, regex("^www")))) %>%
    anti_join(stop_words) %>%
    anti_join(cms_stop) %>%
    anti_join(bigram_index, by = c("index" = "index1")) %>%
    anti_join(bigram_index, by = c("index" = "index2")) %>%
    mutate(word = wordStem(word))

baseline_bigram <- baselineDoc_section %>%
    unnest_tokens(bigram, Text, token = "ngrams", n = 2) %>%
    rowid_to_column("index") %>%
    separate(bigram, c("word1", "word2"), sep = " ") %>%
    anti_join(stop_words, by = c("word1" = "word")) %>%
    anti_join(stop_words, by = c("word2" = "word")) %>%
    anti_join(cms_stop, by = c("word1" = "word")) %>%
    anti_join(cms_stop, by = c("word2" = "word")) %>%
    mutate(word1 = wordStem(word1),
           word2 = wordStem(word2)) %>%
    unite(word, word1, word2, sep = " ") %>%
    filter(word %in% bigram$word)

baseline <- union_all(baseline_unigram, baseline_bigram)

base_count <- baseline %>%
    group_by(section) %>%
    count(section, word, sort = TRUE) %>%
    ungroup()

base_total <- base_count %>% 
    group_by(section) %>% 
    summarize(total = sum(n))

base <- left_join(base_count, base_total) %>%
    filter(total > 5)

base_tf_idf <- base %>%
    bind_tf_idf(word, section, n)

base_dtm <- cast_dtm(base_tf_idf, section, word, n)

length(unique(base$section))

base_lda <- LDA(base_dtm,
                k = 70,
                control = list(seed = 1234))

save(base_lda, file = "Models/lda_test.rda")

load("Models/lda_test.rda")

base_topics_terms <- tidy(base_lda, matrix = "beta")
base_doc_topics <- tidy(base_lda, matrix = "gamma")

write.csv(base_topics_terms, file="Models/modelingTopicsTermsBeta.csv", row.names = FALSE)
write.csv(base_doc_topics, file="Models/modelingDocsTopicsGamma.csv", row.names = FALSE)

docTopicTerms <- base_topics_terms %>%
    inner_join(base_doc_topics, by = c("topic" = "topic")) %>%
    group_by(document, term) %>%
    summarise(final_beta = sum(beta)) %>%
    ungroup() %>%
    group_by(document) %>%
    top_n(20, final_beta) %>%
    ungroup()

write.csv(docTopicTerms, file = "termTesting.csv", row.names = FALSE)

#### Create Topic Map ####
topic_map1 <- base_doc_topics %>%
    group_by(document) %>%
    filter(gamma == max(gamma))

topic_map2 <- base_doc_topics %>%
    group_by(topic) %>%
    filter(gamma == max(gamma))

topic_map <- union_all(topic_map1, topic_map2) %>%
    distinct()

write.csv(topic_map, file = "Models/topic_map.csv", row.names = FALSE)

#### Testing ####

test <- base_doc_topics %>%
    group_by(document) %>%
    filter(gamma == max(gamma))

count <- test %>%
    filter(gamma < .75) %>%
    arrange(desc(gamma))

multi <- base_doc_topics %>%
    filter(gamma > .01) %>%
    group_by(topic) %>%
    count() %>%
    filter(n > 1)

test1 <- base_doc_topics %>%
    group_by(topic) %>%
    filter(gamma == max(gamma))
write.csv(test1, file="modelingtesttopics.csv", row.names = FALSE)


model_top_terms <- base_topics %>%
    group_by(topic) %>%
    top_n(10, beta) %>%
    ungroup() %>%
    arrange(topic, -beta) %>%
    inner_join(topic_map)

write.csv(model_top_terms, file="Data/modelingtopterms.csv", row.names = FALSE)

model_top_terms %>%
    filter(topic %in% c(1,2,3,4,5,6)) %>%
    mutate(term = reorder(term, beta)) %>%
    ggplot(aes(term, beta, fill = factor(topic))) +
    geom_col(show.legend = FALSE) +
    facet_wrap(~ document, scales = "free") +
    coord_flip()

beta_spread <- base_topics %>%
    mutate(topic = paste0("topic", topic)) %>%
    spread(topic, beta) %>%
    filter(topic1 > .001 | topic2 > .001) %>%
    mutate(log_ratio = log2(topic2 / topic1)) %>%
    arrange(term, abs(log_ratio))
