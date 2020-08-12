library(wordcloud2)
library(tm)

word.cloud = function(file.name, col){
  w = get.words.for.cloud(file.name, col)
  make.word.cloud(w)
}

get.words.for.cloud = function(file.name, col){
  umfrage8 = read.csv(file.name, header = TRUE, encoding = "UTF-8", stringsAsFactors = FALSE)
  words = umfrage8[which(umfrage8[col] != ""),]
  words = words[col]
  words[which(words == "Masturbieren"),1] = "Masturbation"
  words[which(words == "Zitronentarte baken"),1] = "Zitronentarte backen"
  words[which(words == "Daten struturieren"),1] = "Daten strukturieren"
  words[which(words == "Daniel Jung"),1] = "daniel-jung"
  words[which(words == "Darts Scheibe ins Zimmer hängen"),1] = "Dartsscheibe"
  words[which(words == "The IT Crowd auf Netflix"),1] = "Netflix"
  words
}

make.word.cloud = function(words_data){
  docs = Corpus(VectorSource(words_data))
  docs = tm_map(docs, removePunctuation, preserve_intra_word_contractions = TRUE, preserve_intra_word_dashes = TRUE)
  docs = tm_map(docs, stripWhitespace)
  docs <- tm_map(docs, content_transformer(tolower))
  docs <- tm_map(docs, removeWords, stopwords("german"))
  docs <- tm_map(docs, removeWords, c("betrifft", "dass", "mal", "ccoole", "ans", "the", "amp", "erstmal", "allgemein", "einfach", "vielleicht", "genug", "auseinander"))
  dtm <- TermDocumentMatrix(docs) 
  matrix <- as.matrix(dtm) 
  words <- sort(rowSums(matrix),decreasing=TRUE) 
  df <- data.frame(word = names(words),freq=words)
  set.seed(1234) # for reproducibility 
  wordcloud2(data=df, size = 0.5, color = 'random-dark')#, shape = 'pentagon')
}