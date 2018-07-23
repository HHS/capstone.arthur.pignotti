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
colnames(baselineDoc) <- make.names(colnames(baselineDoc))

#Load stop words
data(stop_words)

#Load CMS-specific stop words
cms_stop <- read_csv("Data/cms_stop_words.csv")

# Remove non-alpha-numeric characters
baselineDoc$Text <- gsub("[^A-z0-9 ]", "", baselineDoc$Text)

baseline <- baselineDoc %>%
    group_by(Document.ID) %>%
    filter(Text != "",
           Paragraph.Number >= baseStart) %>%
    mutate(section = ifelse(str_detect(Text, regex("^section [A-Z]", ignore_case = TRUE)), Text, NA)) %>%
#    mutate(section = cumsum(str_detect(Text, regex("^section [A-Z]", ignore_case = TRUE)))) %>%
    filter(str_count(Text, regex("[A-z]", ignore_case = TRUE))/str_count(Text, regex("[A-z0-9]", ignore_case = TRUE)) > .5) %>% # Removes lines that are 50% or more numbers
    fill(section) %>%
    filter(!is.na(section)) %>%
    unnest_tokens(word, Text) %>%
    anti_join(stop_words) %>%
    anti_join(cms_stop) %>%
    mutate(word = wordStem(word))

baseline <- baseline %>%
    filter(Paragraph.Number < 626 | Paragraph.Number > 695)

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

base_lda <- LDA(base_dtm,
                k = length(unique(base$section)),
                control = list(seed = 1234))

save(base_lda, file = "Models/lda_test.rda")

base_topics <- tidy(base_lda, matrix = "beta")
base_doc_topics <- tidy(base_lda, matrix = "gamma")

write.csv(base_doc_topics, file="modelingtest.csv")

#### Create Topic Map ####
topic_map1 <- base_doc_topics %>%
    group_by(document) %>%
    filter(gamma == max(gamma))

topic_map2 <- base_doc_topics %>%
    group_by(topic) %>%
    filter(gamma == max(gamma))

topic_map <- rbind(topic_map1, topic_map2) %>%
    distinct() %>%
    select(-gamma)

write.csv(topic_map, file = "Models/topic_map.csv", row.names = FALSE)

#### Testing ####

test <- base_doc_topics %>%
    group_by(document) %>%
    filter(gamma == max(gamma))

count <- test %>%
    filter(gamma < .75) %>%
    arrange(desc(gamma))

multi <- base_doc_topics %>%
    filter(gamma > .001) %>%
    group_by(topic) %>%
    count() %>%
    filter(n > 1)




test1 <- base_doc_topics %>%
    group_by(topic) %>%
    filter(gamma == max(gamma))
write.csv(test1, file="modelingtesttopics.csv")


model_top_terms <- base_topics %>%
    group_by(topic) %>%
    top_n(10, beta) %>%
    ungroup() %>%
    arrange(topic, -beta)


write.csv(model_top_terms, file="Data/modelingtopterms.csv")


model_top_terms %>%
    mutate(term = reorder(term, beta)) %>%
    ggplot(aes(term, beta, fill = factor(topic))) +
    geom_col(show.legend = FALSE) +
    facet_wrap(~ topic, scales = "free") +
    coord_flip()

beta_spread <- base_topics %>%
    mutate(topic = paste0("topic", topic)) %>%
    spread(topic, beta) %>%
    filter(topic1 > .001 | topic2 > .001) %>%
    mutate(log_ratio = log2(topic2 / topic1)) %>%
    arrange(term, abs(log_ratio))
