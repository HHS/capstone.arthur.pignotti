#Placeholder for loading html files
mycorpus <- Corpus(DirSource(comLoc, pattern = "html"))
myhtmlfiles <- list.files(path = comLoc, pattern = "html",  full.names = TRUE) #get list of PDFs


#fix tm install issue
slam_url <- "https://cran.r-project.org/src/contrib/Archive/slam/slam_0.1-37.tar.gz"
install_url(slam_url)

#### Convert PDFs ####

attachLoc <- "C:/Data/Comments/CMS-2017-0163/FDMS/files"
pdfCnt <- length(list.files(path = "files/", pattern = "pdf")) #number of PDFs
docCnt <- length(list.files(path = "files/", pattern = "doc")) #number of Docs




#### Convert to DFM and Calculate Similarity Matrices ####
words_dfm <- cast_dfm(comment.count, document = Document.ID, term = word, value = n)
words_dfm_tf_idf <- cast_dfm(comment.tf_idf, document = Document.ID, term = word, value = tf_idf)


save(comment.tf_idf, file = "Models/words_tf_idf.rda")
save(words_dfm, file = "Models/words_dfm.rda")


pdfCnt <- length(list.files(path = "files/", pattern = "pdf",  full.names = TRUE)) #get list of PDFs
mycorpus <- Corpus(DirSource(attachLoc, pattern = "pdf"))


# Clean up?
nrow(attachList)
write.csv(attachList, file="test.csv")
attachUrl = "https://www.regulations.gov/contentStreamer?documentId=CMS-2017-0163-1203&disposition=attachment&attachmentNumber=1"
test <- HEAD(attachUrl)
file <- substr(tmp, 23, nchar(tmp)-1)
download.file(attachUrl, paste("files/",  attachList[comment, "documentId"], "-", doc, " - " , file, sep = ""), mode="wb")
#testUrl = paste("https://www.regulations.gov/contentStreamer?documentId=", attachList[1, "documentId"], "&disposition=attachment&attachmentNumber=", 1, sep ="")
#test <- getURI(testUrl,header=TRUE,verbose=TRUE)


#library(readxl)
#library(rvest)
#library(scrapeR)
#library(tidyverse)



#### Bigram Testing ####

bigram_count <- commentsDf %>%
    unnest_tokens(bigram, Text, token = "ngrams", n = 2) %>%
    separate(bigram, c("word1", "word2"), sep = " ") %>%
    filter(!word1 %in% stop_words$word) %>%
    filter(!word2 %in% stop_words$word) %>% 
    count(Document.ID, word1, word2, sort = TRUE) %>%
    unite(bigram, word1, word2, sep = " ")

bigram_phrase_count <- commentsDf %>%
    unnest_tokens(bigram, Text, token = "ngrams", n = 2) %>%
    separate(bigram, c("word1", "word2"), sep = " ") %>%
    filter(!word1 %in% stop_words$word) %>%
    filter(!word2 %in% stop_words$word) %>% 
    count(word1, word2, sort = TRUE) %>%
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



#### Network Graph ####


library(igraph)
library(ggraph)
set.seed(2017)

bigram_graph <- commentsDf %>%
    unnest_tokens(bigram, Text, token = "ngrams", n = 2) %>%
    separate(bigram, c("word1", "word2"), sep = " ") %>%
    filter(!word1 %in% stop_words$word) %>%
    filter(!word2 %in% stop_words$word) %>% 
    count(word1, word2, sort = TRUE) %>% 
    filter(n > 1000) %>%
    graph_from_data_frame()

sim_graph <- similarity %>%
    filter(distance > .9) %>%
    graph_from_data_frame()


a <- grid::arrow(type = "closed", length = unit(.01, "inches"))

ggraph(sim_graph, layout = "fr") +
    geom_edge_link(aes(edge_alpha = n), show.legend = FALSE,
                   arrow = a, end_cap = circle(.01, 'inches')) +
    geom_node_point(color = "lightblue", size = 1) +
    geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
    theme_void()




#### Pairwaise Correlations ####


library(widyr)

word_cors <- test1 %>%
    group_by(word) %>%
    filter(n() >= 100) %>%
    pairwise_cor(word, Document.ID, sort = TRUE)


word_cors %>%
    filter(item1 %in% c("drug", "dir", "pharmacy", "pbm")) %>%
    group_by(item1) %>%
    top_n(10) %>%
    ungroup() %>%
    mutate(item2 = reorder(item2, correlation)) %>%
    ggplot(aes(item2, correlation)) +
    geom_bar(stat = "identity") +
    facet_wrap(~ item1, scales = "free") +
    coord_flip()
