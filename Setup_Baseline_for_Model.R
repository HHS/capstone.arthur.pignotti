#### Initial Setup ####

.libPaths( c("C:/R/Packages", .libPaths()) ) #add extra library location
setwd("C:/Users/P6BQ/Desktop/capstone.arthur.pignotti") #local location of github repo

#Load libraries
library(tidyverse)
library(readxl)
library(tidytext)
library(tm)
library(SnowballC)

baselineDoc <- read_excel("Data/AN_Part2.xlsx", sheet = 1)
colnames(baselineDoc) <- make.names(colnames(baselineDoc))

data(stop_words)

testing <- baselineDoc %>%
    group_by(Document.ID) %>%
    filter(Text != "",
           Paragraph.Number > 65) %>%
    mutate(section = ifelse(str_detect(Text, regex("^section [A-Z]", ignore_case = TRUE)), Text, NA)) %>%
    fill(section)

testing <- baselineDoc %>%
    group_by(Document.ID) %>%
    filter(Text != "",
           Paragraph.Number > 65) %>%
    mutate(section = cumsum(str_detect(Text, regex("^section [A-Z]", ignore_case = TRUE))))
write.csv(testing, file = "Data/sections_test.csv")

baseline <- baselineDoc %>%
    group_by(Document.ID) %>%
    filter(Text != "",
           Paragraph.Number > 65) %>%
    mutate(section = cumsum(str_detect(Text, regex("^section [A-Z]", ignore_case = TRUE)))) %>%
    unnest_tokens(word, Text) %>%
    anti_join(stop_words) %>%
    mutate(word = wordStem(word))

base_count <- baseline %>%
    group_by(section) %>%
    count(section, word, sort = TRUE) %>%
    ungroup()

base_total <- base_count %>% 
    group_by(section) %>% 
    summarize(total = sum(n))

base <- left_join(base_count, base_total)

base_tf_idf <- base %>%
    bind_tf_idf(word, section, n)

#base_dtm <- cast_dtm(base_tf_idf, section, word, n)
#getTransformations()
#base_dtm <- tm_map(base_dtm, stemDocument)

base_tf_idf %>%
    filter(section %in% 9:12) %>%
    arrange(desc(tf_idf)) %>%
    mutate(word = factor(word, levels = rev(unique(word)))) %>% 
    group_by(section) %>% 
    top_n(15) %>% 
    ungroup %>%
    ggplot(aes(word, tf_idf, fill = section)) +
    geom_col(show.legend = FALSE) +
    labs(x = NULL, y = "tf-idf") +
    facet_wrap(~section, ncol = 2, scales = "free") +
    coord_flip()


############################
# Create Corpus Using Tidy #
############################

commentsDf <- read.csv("Data/commentsDf.csv", stringsAsFactors = FALSE)
