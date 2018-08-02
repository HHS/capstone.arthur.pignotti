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
library(quanteda)

baselineDoc <- read_excel("Data/AN_Part2 - V3.xlsx", sheet = 1)
#commentsDf <- read_csv("Data/commentsDf.csv")
colnames(baselineDoc) <- make.names(colnames(baselineDoc))

#Load stop words
data(stop_words)

#Load CMS-specific stop words
cms_stop <- read_csv("Data/cms_stop_words.csv")

# Remove non-alpha-numeric characters
baselineDoc$Text <- gsub("[^A-z0-9 ]", "", baselineDoc$Text)

# Add section column and removes lines paragraphs that are mostly numbers, which are generally parts of tables
baselineDoc_section <- baselineDoc %>%
    group_by(Document.ID) %>%
    filter(Text != "",
           Paragraph.Number >= baseStart) %>%
    mutate(section = ifelse(str_detect(Text, regex("^section [A-Z]", ignore_case = TRUE)), Text, ifelse(str_detect(Text, regex("^attachment [A-Z]", ignore_case = TRUE)), Text, NA))) %>%
    filter(str_count(Text, regex("[A-z]", ignore_case = TRUE))/str_count(Text, regex("[A-z0-9]", ignore_case = TRUE)) > .5) %>% # Removes lines that are 50% or more numbers
    fill(section) %>%
    filter(!is.na(section)) %>%
    filter(str_detect(section, regex("^section [A-Z]", ignore_case = TRUE)))

# Remove table of contents for call letter
baselineDoc_section <- baselineDoc_section %>%
    filter(Paragraph.Number < 514 | Paragraph.Number > 695)

# Find sections w, probably not a real section
section.count <- baselineDoc_section %>%
    unnest_tokens(word, Text) %>%
    group_by(section) %>%
    count() %>%
    filter(n < 10)

# Filter out sections with less than 10 words
baselineDoc_section <- filter(baselineDoc_section, !section %in% section.count$section)

word.count <- baselineDoc_section %>%
    unnest_tokens(word, Text) %>%
    count()

#### Build to find unbalanced sections ####
section.word.count <- baselineDoc_section %>%
    unnest_tokens(word, Text) %>%
    count(section, sort = TRUE)

bigram.baseline <- baselineDoc_section %>%
    select(section, Document.ID, Text) %>%
    ungroup() %>%
    unnest_tokens(bigram, Text, token = "ngrams", n = 2) %>%
    separate(bigram, c("word1", "word2"), sep = " ") %>%
    anti_join(stop_words, by = c("word1" = "word")) %>%
    anti_join(stop_words, by = c("word2" = "word")) %>%
    anti_join(cms_stop, by = c("word1" = "word")) %>%
    anti_join(cms_stop, by = c("word2" = "word")) %>%
    mutate(word1 = wordStem(word1),
           word2 = wordStem(word2)) %>%
    unite(word, word1, word2, sep = " ")

bigram.section.count <- bigram.baseline %>%
    count(section, word, sort = TRUE)

bigram.section.total <- bigram.section.count %>% 
    group_by(section) %>% 
    summarize(total = sum(n))

bigram.count <- bigram.baseline %>%
    count(word, sort = TRUE)

bigram <- left_join(bigram.section.count, bigram.section.total)

bigram.tf_idf <- bigram %>%
    bind_tf_idf(word, section, n) %>%
    arrange(desc(tf_idf))

bigram.list <- union(select(filter(bigram.count, n > 10), word),
                     select(filter(bigram.tf_idf, n > 5 & tf_idf > .1), word)) %>%
    distinct() %>%
    arrange(word)

write.csv(bigram.list, file = "Data/bigram_list2.csv", row.names = FALSE)

bigram.index <- baselineDoc_section %>%
    unnest_tokens(bigram, Text, token = "ngrams", n = 2) %>%
    rowid_to_column("index1") %>%
    inner_join(bigram.list, by = c("bigram" = "word")) %>%
    mutate(index2 = index1 + 1) %>%
    select(Document.ID, bigram, index1, index2)

baseline.unigram <- baselineDoc_section %>%
    unnest_tokens(word, Text) %>%
    rowid_to_column("index") %>%
    filter(!(str_detect(word, regex("^http"))),
           !(str_detect(word, regex("^www")))) %>%
    anti_join(stop_words) %>%
    anti_join(cms_stop) %>%
    anti_join(bigram.index, by = c("index" = "index1")) %>%
    anti_join(bigram.index, by = c("index" = "index2")) %>%
    mutate(word = wordStem(word))

baseline.bigram <- baselineDoc_section %>%
    unnest_tokens(bigram, Text, token = "ngrams", n = 2) %>%
    separate(bigram, c("word1", "word2"), sep = " ") %>%
    mutate(word1 = wordStem(word1),
           word2 = wordStem(word2)) %>%
    unite(word, word1, word2, sep = " ") %>%
    filter(word %in% bigram.list$word)

baseline <- union_all(baseline.unigram, baseline.bigram)

baseline.count <- baseline %>%
    count(section, word, sort = TRUE)

baseline.total <- baseline.count %>% 
    group_by(section) %>% 
    summarize(total = sum(n))

base <- left_join(baseline.count, baseline.total)

baseline.tf_idf <- base %>%
    bind_tf_idf(word, section, n)

baseline.dtm <- cast_dtm(baseline.tf_idf, section, word, n)

length(unique(base$section))

base_lda <- LDA(baseline.dtm,
                k = 80,
                control = list(seed = 1234))

save(base_lda, file = "Models/lda_test.rda")

base_ctm <- CTM(baseline.dtm,
                k = 80,
                control = list(seed = 1234))

save(base_ctm, file = "Models/ctm_test.rda")

load("Models/lda_test.rda")

model.topic.term <- tidy(base_lda, matrix = "beta")
model.doc.topic <- tidy(base_lda, matrix = "gamma")
model.doc.term <- inner_join(model.topic.term, model.doc.topic, by = c("topic" = "topic")) %>%
    mutate(score = beta * gamma) %>%
    select(document, term, score) %>%
    group_by(document, term) %>%
    summarise(score = sum(score)/sum(gamma)) %>%
    arrange(desc(score))

write.csv(model.doc.term, file="Models/modelingTopicsTermsBeta.csv", row.names = FALSE)
write.csv(model.doc.topic, file="Models/modelingDocsTopicsGamma.csv", row.names = FALSE)

doc.topic.terms <- model.topic.term %>%
    inner_join(model.doc.topic, by = c("topic" = "topic")) %>%
    group_by(document, term) %>%
    summarise(final_beta = sum(beta*gamma)/sum(gamma)) %>%
    ungroup() %>%
    group_by(document) %>%
    top_n(200, final_beta) %>%
    ungroup()

write.csv(doc.topic.terms, file = "termTesting.csv", row.names = FALSE)

#### Create Word Clouds of Basline Sections ####

library(wordcloud)
library(RColorBrewer)

pal2 <- brewer.pal(8,"Dark2")
for (i in 1:length(unique(doc.topic.terms$document))) {
    doc <- unique(doc.topic.terms$document)[i]
    subset <- doc.topic.terms %>%
        filter(document == doc)
    png(paste0("clouds/", doc,".png"), width=1280,height=800)
    wordcloud(subset$term, 
              subset$final_beta, 
              scale=c(8,.3),
              colors=pal2)
    dev.off()
}


#### Create Topic Map ####
topic.map1 <- model.doc.topic %>%
    group_by(document) %>%
    filter(gamma == max(gamma))

topic.map2 <- model.doc.topic %>%
    group_by(topic) %>%
    filter(gamma == max(gamma))

topic.map <- union_all(topic.map1, topic.map2) %>%
    distinct()

write.csv(topic.map, file = "Models/topic_map.csv", row.names = FALSE)

#### Test similarity of sections ####
baseline.dfm <- cast_dfm(baseline.count,
                         document = section,
                         term = word,
                         value = n)

baseline.similarity <- textstat_simil(baseline.dfm,
                                      margin = "documents",
                                      method = "cosine") %>%
    as.dist() %>%
    tidy() %>%
    arrange(desc(distance))

names(baseline.similarity) <- c("doc1", "doc2", "distance")

hist(baseline.similarity$distance)

write.csv(baseline.similarity,
          file = "Models/base_similarity_scores.csv",
          row.names = FALSE)

#### Graphs of Similar Sections ####

test <- baseline %>%
    filter(section %in% c("Section D  Medicare Part D Benefit Parameters Annual Adjustments for Defined Standard Benefit in 2019", "Section H  Enhanced Medication Therapy Management MTM Model")) %>%
    count(section, word, sort = TRUE) %>%
    group_by(section) %>%
    top_n(20)


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