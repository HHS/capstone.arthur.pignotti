#### Initial Setup ####
.libPaths( c("C:/R/Packages", .libPaths()) ) #add extra library location
setwd("C:/Users/P6BQ/Desktop/capstone.arthur.pignotti") #local location of github repo

#baseStart = 66

# Load libraries
library(tidyverse)
library(readxl)
library(tidytext)
library(tm)
library(SnowballC)
library(topicmodels)
library(quanteda)

# Load baseline document for building model
baselineDoc <- read_excel("Data/AN - V5.xlsx", sheet = 1)
colnames(baselineDoc) <- make.names(colnames(baselineDoc))

# Load stop words
data(stop_words)

# Load CMS-specific stop words
cms_stop <- read_csv("Data/cms_stop_words.csv")

# Remove non-alpha-numeric characters
#baselineDoc$Text <- gsub("[^A-z0-9 ]", " ", baselineDoc$Text)

# Add section column and removes lines paragraphs that are mostly numbers, which are generally parts of tables
baselineDoc_section <- baselineDoc %>%
    group_by(Document.ID) %>%
    filter(Text != "") %>%
    #filter(Paragraph.Number >= baseStart) %>%
    mutate(section = ifelse(str_detect(Text, regex("^section [A-Z]", ignore_case = TRUE)), Text, ifelse(str_detect(Text, regex("^attachment [A-Z]", ignore_case = TRUE)), Text, NA))) %>%
    mutate(Text = str_replace_all(Text, "-", " ")) %>%
    filter(str_count(Text, regex("[A-z]", ignore_case = TRUE))/str_count(Text, regex("[A-z0-9]", ignore_case = TRUE)) > .5) %>% # Removes lines that are 50% or more numbers
    fill(section) %>%
    filter(!is.na(section)) %>%
    filter(str_detect(section, regex("^section [A-Z]", ignore_case = TRUE)))

# Remove table of contents for call letter
baselineDoc_section <- baselineDoc_section %>%
    filter(Paragraph.Number < 593 | Paragraph.Number > 774)

# Find sections with less than 10 words, probably not real sections
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

write.csv(section.word.count, file = "Models/section_word_count.csv", row.names = FALSE)

#### Find important bigrams from the baseline document ####
bigram.baseline <- baselineDoc_section %>%
    select(section, Document.ID, Text) %>%
    ungroup() %>%
    unnest_tokens(bigram,
                  Text,
                  token = "ngrams",
                  n = 2) %>%
    separate(bigram,
             c("word1", "word2"),
             sep = " ") %>%
    anti_join(stop_words,
              by = c("word1" = "word")) %>%
    anti_join(stop_words,
              by = c("word2" = "word")) %>%
    anti_join(cms_stop,
              by = c("word1" = "word")) %>%
    anti_join(cms_stop,
              by = c("word2" = "word")) %>%
    mutate(word1 = wordStem(word1),
           word2 = wordStem(word2)) %>%
    unite(word, word1, word2, sep = " ")

bigram.section.count <- bigram.baseline %>%
    count(section, word, sort = TRUE)

bigram.section.total <- bigram.section.count %>% 
    group_by(section) %>% 
    summarize(total = sum(n))

bigram.count <- bigram.baseline %>%
    count(word,
          sort = TRUE)

bigram <- left_join(bigram.section.count,
                    bigram.section.total)

bigram.tf_idf <- bigram %>%
    bind_tf_idf(word,
                section,
                n) %>%
    arrange(desc(tf_idf)) %>%
    select(-n) %>%
    inner_join(bigram.count, by = c("word" = "word"))

bigram.list <- union(select(filter(bigram.count, n > 10), word),
                     select(filter(bigram.tf_idf, n > 5 & tf_idf > .1), word)) %>%
    distinct() %>%
    arrange(word)

write.csv(bigram.list, file = "Data/bigram_list2.csv", row.names = FALSE)

#### Integrate Bigrams ####
bigram.index <- baselineDoc_section %>%
    unnest_tokens(bigram,
                  Text,
                  token = "ngrams",
                  n = 2,
                  collapse = FALSE) %>%
    rowid_to_column("index") %>%
    separate(bigram,
             c("word1", "word2"),
             sep = " ") %>%
    mutate(word1 = wordStem(word1),
           word2 = wordStem(word2))
    
bigram.last.row <- bigram.index %>%
    group_by(Paragraph.Number) %>%
    filter(row_number() == n()) %>%
    ungroup() %>%
    mutate(word = word2,
           index = index + 1) %>%
    filter(!is.na(word)) %>%
    select(-c(word1, word2))
    
bigram.index <- bigram.index %>%
    union_all(bigram.last.row) %>%
    unite(word,
          word1,
          word2,
          sep = " ") %>%
    arrange(Paragraph.Number,
            index) %>%
    rowid_to_column("index1") %>%
    select(-index) %>%
    inner_join(bigram.list,
               by = c("word" = "word")) %>%
    mutate(index2 = index1 + 1,
           index = index1) 
#%>%
#    select(Document.ID, bigram, index1, index2)

#write.csv(bigram.index, file = "bigram_index.csv", row.names = FALSE)

baseline.unigram <- baselineDoc_section %>%
    unnest_tokens(word,
                  Text) %>%
    rowid_to_column("index") %>%
    filter(!(str_detect(word, regex("^http"))),
           !(str_detect(word, regex("^www")))) %>%
    anti_join(stop_words) %>%
    anti_join(cms_stop) %>%
    anti_join(bigram.index,
              by = c("index" = "index1")) %>%
    anti_join(bigram.index,
              by = c("index" = "index2")) %>%
    mutate(word = wordStem(word))

#write.csv(baseline.unigram, file = "unigram_index.csv", row.names = FALSE)

baseline <- baseline.unigram %>%
    union_all(bigram.index) %>%
    select(-c(index1, index2)) %>%
    arrange(Paragraph.Number)

# Similarity to Baseline Document Threshold
baseSimThres = .95

# Load comment data
commentsDf <- read.csv("Data/commentsDf.csv",
                       stringsAsFactors = FALSE) # dataset of comments
bigram.list <- read.csv("Data/bigram_list2.csv",
                        stringsAsFactors = FALSE) # load list of relevant bigrams

commentsDf$Text <- gsub("[^A-z0-9 ]", " ", commentsDf$Text)

#### Remove Comment Attachment that are Uploads of the Baseline Document ####
baseSim <- read.csv("Data/Similarity_to_Baseline.csv")
simRemoves <- baseSim %>%
    filter(distance > baseSimThres) %>%
    select(doc2) %>%
    rename(doc = doc2)

commentsDf <- commentsDf %>%
    filter(!(Document.ID %in% simRemoves$doc))

#### Create Corpus Using Tidy ####
data(stop_words) # load common English stop words
cms_stop <- read_csv("Data/cms_stop_words.csv") # load CMS-specific stop words

## Unnest tokens, integrate bigrams, and remove stop words ##
# Unnnest bigram tokens
bigram.index <- commentsDf %>%
    unnest_tokens(bigram,
                  Text,
                  token = "ngrams",
                  n = 2) %>% # unnest bigram tokens
    rowid_to_column("index") %>% # add index column
    separate(bigram,
             c("word1", "word2"),
             sep = " ") %>% # separate bigram into two words for stemming 
    mutate(word1 = wordStem(word1),
           word2 = wordStem(word2)) # stem separated bigrams

# Fix bigram unnesting so it matches word unnesting. Bigram unnesting will not unnest the final word of a cell into the first word of the next cell or by itself so the indexing between bigrams and words will not naturally match. So, we are unnesting the second word from the final bigram from each Document.ID-Page.Number combo to create a separate row with the final word by itself 
bigram.last.row <- bigram.index %>%
    group_by(Document.ID,
             Page.Number) %>% # group by Document.ID and Page.Number
    filter(row_number() == n()) %>% # filters for the last row of each group
    ungroup() %>%
    mutate(word = word2, # transform the bigram into the final word
           index = index + 1) %>% # increment the index so it arrange at the end of the Document.ID-Page.Number group when reintegrated
    filter(!is.na(word)) %>% # remove NA that show up when unnesting text with less than two words
    select(-c(word1, word2)) # 

bigram.index <- bigram.index %>%
    unite(word,
          word1,
          word2,
          sep = " ") %>% # recombine stemmed bigrams
    union_all(bigram.last.row) %>% # combine the original unnested bigram and the single word fix
    arrange(Document.ID,
            Page.Number,
            index) %>% # re
    rowid_to_column("index1") %>%
    select(-index) %>%
    inner_join(bigram.list,
               by = c("word" = "word")) %>%
    mutate(index2 = index1 + 1,
           index = index1)

comment.words <- commentsDf %>%
    unnest_tokens(word,
                  Text) %>%
    rowid_to_column("index") %>%
    filter(!(str_detect(word, regex("^http"))),
           !(str_detect(word, regex("^www")))) %>% 
    anti_join(stop_words) %>%
    anti_join(cms_stop) %>%
    anti_join(bigram.index,
              by = c("index" = "index1")) %>%
    anti_join(bigram.index,
              by = c("index" = "index2")) %>%
    mutate(word = wordStem(word))

comments <- comment.words %>%
    union_all(bigram.index) %>%
    select(-c(index1, index2))

combined <- baseline %>%
    ungroup() %>%
    select(Document.ID = section, word) %>%
    union_all(comments %>%
                  mutate(Document.ID = paste0(Document.ID,"-",Page.Number)) %>%
                  select(Document.ID, word))


combined.count <- combined %>%
    count(Document.ID,
          word,
          sort = TRUE)

combined.dtm <- combined.count %>%
    cast_dtm(word,
             Document.ID,
             n)


#### Train topic model ####
combined_lda <- LDA(combined.dtm,
                    k = 86,
                    control = list(seed = 1234))

#save(base_lda, file = "Models/lda_test.rda")

#base_ctm <- CTM(baseline.dtm,
#                k = 80,
#                control = list(seed = 1234))

#save(base_ctm, file = "Models/ctm_test.rda")

#load("Models/lda_test.rda")


model.topic.term <- tidy(combined_lda, matrix = "beta")
model.doc.topic <- tidy(combined_lda, matrix = "gamma")
model.doc.topic.sections <- model.doc.topic %>%
    filter(str_detect(document, "^Section"))

model.topic.top <- model.doc.topic.sections %>%
    group_by(topic) %>%
    arrange(-gamma) %>%
    top_n(1)

model.document.top <- model.doc.topic.sections %>%
    group_by(document) %>%
    arrange(-gamma) %>%
    top_n(1)

model.topic.3 <- model.doc.topic %>%
    filter(topic == 3) %>%
    arrange(-gamma)


model.doc.term <- model.topic.term %>%
    inner_join(model.doc.topic,
               by = c("topic" = "topic")) %>%
    mutate(score = beta * gamma) %>%
    select(document, term, score, gamma) %>%
    group_by(document,
             term) %>%
    summarise(score = sum(score)/sum(gamma)) %>%
    arrange(desc(score))


#write.csv(model.doc.term, file="Models/modelingTopicsTermsBeta.csv", row.names = FALSE)
#write.csv(model.doc.topic, file="Models/modelingDocsTopicsGamma.csv", row.names = FALSE)

doc.topic.terms <- model.topic.term %>%
    inner_join(model.doc.topic,
               by = c("topic" = "topic")) %>%
    group_by(document,
             term) %>%
    summarise(final_beta = sum(beta*gamma)/sum(gamma)) %>%
    ungroup() %>%
    mutate(final_beta = if_else(final_beta < .001,
                           0,
                           final_beta)) %>% # Scores below .1% are changed to zero to remove scientific notation
    arrange(document, -final_beta)



#### Create Word Clouds of Basline Sections ####
library(wordcloud)
library(RColorBrewer)

pal2 <- brewer.pal(8,"Dark2")
for (i in 1:length(unique(doc.topic.terms$document))) {
    doc <- unique(doc.topic.terms$document)[i]
    
    subset <- doc.topic.terms %>%
        filter(document == doc) %>%
        arrange(-final_beta) %>%
        slice(1:50)
        
    png(paste0("clouds/", doc,".png"),
        width = 1280,
        height = 800)
    
    wordcloud(subset$term, 
              subset$final_beta, 
              scale = c(8,.2),
              colors = pal2)
    
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