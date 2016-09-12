library(stringr)
library(tm)
library(wordcloud)
library(httr)
library(rjson)
library(plyr)
library(urltools)
library(twitteR)

clean_input = function(sentence) #TODO figure out why this doesn't work
{
  sentence <- gsub('[[:punct:]]', '', sentence)
  sentence <- gsub('[[:cntrl:]]', '', sentence)
  sentence <- gsub('\\d+', '', sentence)
  #remove emoji
  sentence <- iconv(sentence, 'UTF-8', 'ASCII')
  # remove retweet entities
  sentence <- gsub("(RT|via)((?:\\b\\W*@\\w+)+)", '', sentence)
  # remove at people
  sentence <- gsub("@\\w+", '', sentence)
  sentence <- gsub("http\\w+", '', sentence)
  sentence <- gsub('http\\S+\\s*', '', sentence)
  sentence <- gsub('[^0-9A-Za-z[[:space:]]]', '', sentence)
  # and convert to lower case:
  sentence <- tolower(sentence)
  return(sentence)
}

score.sentiment = function(sentences, pos.words, neg.words, .progress='none')
{
  
  require(plyr)
  require(stringr)
  scores = laply(sentences, function(sentence, pos.words, neg.words) {
    
    sentence <- clean_input(sentence)
    word.list = str_split(sentence, '\\s+')
    words = unlist(word.list)
    # compare our words to the dictionaries of positive & negative terms
    pos.matches = match(words, pos.words)
    neg.matches = match(words, neg.words)
    pos.matches = !is.na(pos.matches)
    neg.matches = !is.na(neg.matches)
    score = sum(pos.matches) - sum(neg.matches)
    return(score)
  }, pos.words, neg.words, .progress=.progress )
  
  scores.df = data.frame(score=scores, text=sentences)
  
  return(scores.df)
}

score.alchemy_sentiment = function(sentences, target)
{
  
  require(plyr)
  require(stringr)
  require(urltools)
  require(httr)
  
  scores = laply(sentences, function(sentence, target) {
    sentence <- clean_input(sentence)
    query <- url_encode(sentence)
    target <- gsub('#', '', target)
    #perform a GET request on the URL, with two headers and store in a resp variable
    #resp <- GET("https://loudelement-free-natural-language-processing-service.p.mashape.com/nlp-text/?text=Friends+and+fellow+sailors+mourned+double+Olympic+medalist+Andrew+%22Bart%22+Simpson+after+the+shocking+news+that+he+had+died+in+San+Francisco+Bay+while+training+for+the+America's+Cup.", add_headers("X-Mashape-Key" = sentimentKey,"Accept" = "application/json"))
    endpoint <- "https://alchemy.p.mashape.com/text/TextGetTargetedSentiment?outputMode=json&target="
    apiCall <- paste(endpoint, target, '&text=', query, sep = '')
    apiCall
    resp <-GET(apiCall, add_headers("X-Mashape-Key" = sentimentKey, "Accept" = "application/json"))
    warn_for_status(resp)
    json<-httr::content(resp, type = "application/json")
    score <- json$docSentiment$score
    if (is.null(score)) {
      score <- 0
    } else {
      score <- as.numeric(score)
    }
    return(score)
    
  }, target)
  
  alchemyscores.df = data.frame(score=scores, text=sentences)
  
  return(alchemyscores.df)
}



# Wordcloud  --------------------------------------------------------------
build_wordcloud_matrix = function(df, searchTerm, minFreq = 1){
  require(tm)
  require(wordcloud)
  # build a corpus, which is a collection of text documents
  # VectorSource specifies that the source is character vectors.
  df$text <- lapply(df$text, clean_input)
  myCorpus <- Corpus(VectorSource(df$text))
  # remove stopwords
  myStopwords <- c(stopwords('SMART'), "available", "via")
  myCorpus <- tm_map(myCorpus, removeWords, myStopwords)
  myDTM <- TermDocumentMatrix(myCorpus,
                              control = list(minWordLength = 1))
  findFreqTerms(myDTM, lowfreq = minFreq)
  findAssocs(myDTM, searchTerm, 0.2)
  m <- as.matrix(myDTM)
  v <- sort(rowSums(m), decreasing = TRUE)
  myNames <- names(v)
  k <- which(names(v)==searchTerm)
  myNames[k] <- searchTerm
  d <- data.frame(word=myNames, freq=v)
  return(d)
}