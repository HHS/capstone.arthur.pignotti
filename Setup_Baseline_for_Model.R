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

base_topics <- tidy(base_lda, matrix = "beta")



train.topics <- topics(base_lda)

test.topics <- posterior(base_lda, base_dtm)
test <- apply(test.topics$topics, 1, which.max)




model_top_terms <- base_topics %>%
    group_by(topic) %>%
    top_n(10, beta) %>%
    ungroup() %>%
    arrange(topic, -beta)

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
