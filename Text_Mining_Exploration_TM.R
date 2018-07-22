##########################
# Create Corpus Using TM #
##########################


mycorpus = Corpus(VectorSource(comments$commentText)) 
commentsDf <- data.frame(paste(comments$documentId,"-0",sep=""), comments$commentText, stringsAsFactors = FALSE)
colnames(commentsDf) <- c("doc_id","text")
mycorpus = Corpus(DataframeSource(commentsDf)) 




auths <- paste0('Author',seq(nrow(commentsDf)))
i <- 0
mycorpus = tm_map(mycorpus, function(x) {
    i <<- i +1
    meta(x, "Author") <- m[i,2]
    x
})


########################
# Clean Corpus         #
########################
mycorpus <- tm_map(mycorpus, stripWhitespace)
mycorpus <- tm_map(mycorpus, content_transformer(tolower))
mycorpus <- tm_map(mycorpus, removeWords, stopwords("english"))


#html test
webpage <- read_html(myhtmlfiles[1])
test <- html_table(html_nodes(webpage, "table"))


