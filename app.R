library(RColorBrewer)
library(base64enc)
library(httr)
library(rjson)
library(plyr)
library(stringr)
library(urltools)
library(twitteR)
library(ggplot2)
library(shiny)
library(tm)
library(wordcloud)

#initialize environment variables
sentimentKey <- '8khaViAJdqmsh0UcO5cKITVbC8CCp1lGWRMjsntxdWIPzBjaKI'
consumerKey <- 'MUwukLPqlXp7pROuci6NGHiIo'
consumerSecret <- 'yCucMwTsjMsXFrrKDnHTyawxgKFzzRMGy3UeEyzGGvlQXzIHAV'
tokenKey <- '4095349632-uOfSHrUmPzbuTYDRkASHfUuJBy0YxCesY6QtWbe'
tokenSecret <- 'FS55U9PqOIBTCo3mbV3bHli7aEXMJEGmGjPlpfrifGeTL'
setup_twitter_oauth(consumerKey, consumerSecret, tokenKey, tokenSecret)
pos <- scan('res/positive-words.txt', what='character', comment.char=';')
neg <- scan('res/negative-words.txt', what='character', comment.char=';')


score.sentiment = function(sentences, pos.words, neg.words, .progress='none')
{
  
  require(plyr)
  require(stringr)
  scores = laply(sentences, function(sentence, pos.words, neg.words) {
    
    # clean up sentences with R's regex-driven global substitute, gsub():
    sentence = gsub('[[:punct:]]', '', sentence)
    sentence = gsub('[[:cntrl:]]', '', sentence)
    sentence = gsub('\\d+', '', sentence)
    # remove retweet entities
    sentence = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", sentence)
    # remove at people
    sentence = gsub("@\\w+", "", sentence)
    sentence = gsub("http\\w+", "", sentence)
    #sentence = gsub('[^0-9A-Za-z]', '', sentence)
    # and convert to lower case:
    sentence = tolower(sentence)
    # split into words. str_split is in the stringr package
    word.list = str_split(sentence, '\\s+')
    # sometimes a list() is one level of hierarchy too much
    words = unlist(word.list)
    # compare our words to the dictionaries of positive & negative terms
    pos.matches = match(words, pos.words)
    neg.matches = match(words, neg.words)
    # match() returns the position of the matched term or NA
    # we just want a TRUE/FALSE:
    pos.matches = !is.na(pos.matches)
    neg.matches = !is.na(neg.matches)
    # and conveniently enough, TRUE/FALSE will be treated as 1/0 by sum():
    score = sum(pos.matches) - sum(neg.matches)
    return(score)
  }, pos.words, neg.words, .progress=.progress )
  
  scores.df = data.frame(score=scores, text=sentences)
  
  return(scores.df)
}

AlchemyScore.AlchemySentiment = function(sentences, target)
{
  
  require(plyr)
  require(stringr)
  require(urltools)
  require(httr)
  
  scores = laply(sentences, function(sentence, target) {
    # clean up sentences with R's regex-driven global substitute, gsub():
    sentence = gsub('[[:punct:]]', '', sentence)
    sentence = gsub('[[:cntrl:]]', '', sentence)
    sentence = gsub('\\d+', '', sentence)
    # remove retweet entities
    sentence = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", sentence)
    # remove at people
    sentence = gsub("@\\w+", "", sentence)
    sentence <- gsub('http\\S+\\s*', '', sentence)
    # and convert to lower case:
    sentence = tolower(sentence)
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
  
  Alchemyscores.df = data.frame(score=scores, text=sentences)
  
  return(Alchemyscores.df)
}

CleanInput = function(sentence) #TODO figure out why this doesn't work
{
  sentence = gsub('[[:punct:]]', '', sentence)
  sentence = gsub('[[:cntrl:]]', '', sentence)
  sentence = gsub('\\d+', '', sentence)
  # remove retweet entities
  sentence = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", sentence)
  # remove at people
  sentence = gsub("@\\w+", "", sentence)
  sentence = gsub("http\\w+", "", sentence)
  sentence <- gsub('http\\S+\\s*', '', sentence)
  sentence = gsub('[^0-9A-Za-z]', '', sentence)
  # and convert to lower case:
  sentence = tolower(sentence)
  return(sentence)
}

# Wordcloud  --------------------------------------------------------------
BuildWordcloudMatrix = function(df, searchTerm, minFreq){
require(tm)
require(wordcloud)
# build a corpus, which is a collection of text documents
# VectorSource specifies that the source is character vectors.
df$text = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", df$text)
df$text = gsub("@\\w+", "", df$text)
df$text = gsub("[[:punct:]]", "", df$text)
df$text = gsub("[[:digit:]]", "", df$text)
df$text = gsub("[ \t]{2,}", "", df$text)
df$text = gsub("^\\s+|\\s+$", "", df$text)
df$text = gsub("http\\w+", "", df$text)
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
#return(m)
}

ui <- fluidPage(
  fluidRow(
  titlePanel("Twitter Analysis"),
  sidebarLayout(
    # Sidebar with a slider and selection inputs
    sidebarPanel(
      hr(),
      sliderInput(inputId = "nTweets", 
                  label = "Choose a number of tweets to gather", 
                  value = 10, min = 1, max = 100),
      textInput(inputId = "term", label = "Enter a hashtag or term to search:", value = "Canada"),
      sliderInput("freq",
                  "Minimum Word Frequency:",
                  min = 1,  max = 50, value = 1),
      sliderInput("max",
                  "Maximum Word Frequency:",
                  min = 1,  max = 300,  value = 100),
      actionButton(inputId = "go", 
                   label = "Go/Refresh")
    ),
  mainPanel(
  plotOutput("WordCloud"))),
  fluidRow(
    tabsetPanel(              
      tabPanel(title = "Positive/Negative Word Usage",
               plotOutput("LHist")
      ),
      tabPanel(title = "Sentiment Score",
               plotOutput("ADens")
      )
    ))
))

server<- shinyServer(function(input, output) {
  
  #token <- get("oauth_token", twitteR:::oauth_cache)
  #token$cache
  
  title <- eventReactive(input$go, {
    paste("sentiment on", input$term, sep=" ")
  })
  tweets <- eventReactive(input$go, {
    searchTwitter(input$term, n=input$nTweets, lang="en")
  })
  df <- eventReactive(input$go, {twListToDF(tweets())})
  orderedTweets <- eventReactive(input$go, {df()[, order(names(df()))]})
  terms <- reactive({BuildWordcloudMatrix(orderedTweets(), input$term, input$freq)})
  
  Tweets.text <- eventReactive(input$go, {
    laply(tweets(), function(t)t$getText())
  })
  analysis <- eventReactive(input$go, {
    score.sentiment(Tweets.text(), pos, neg)
  })
  AlchemyAnalysis <- eventReactive(input$go, {
    AlchemyScore.AlchemySentiment(Tweets.text(), input$term)
  })
  
  output$LHist <- renderPlot({
    #histogram
    condBlue <- analysis()$score == 0
    condGreen <- analysis()$score >= 1
    condRed <- analysis()$score <= -1
    
    ggplot(analysis(), aes(x=score)) +
      geom_histogram(data=subset(analysis(),condBlue==TRUE), binwidth=1, fill="blue") +
      geom_histogram(data=subset(analysis(),condGreen==TRUE), binwidth=1, fill="green") +
      geom_histogram(data=subset(analysis(),condRed==TRUE), binwidth=1, fill="red") +
      scale_x_continuous(breaks=analysis()$score+0.5, labels=analysis()$score)+
    ggtitle(paste("Lexical analysis'", title()))
  })
  output$ADens <- renderPlot({
    if(input$go==0) return(NULL)
    withProgress(message = 'Calculation in progress',
                 detail = 'This may take a while...', value = 0, {
                   for (i in 1:input$nTweets) {
                     incProgress(1/input$nTweets)
                     Sys.sleep(0.25)
                   }
                 })
    ggplot(AlchemyAnalysis(), aes(x=score)) + geom_density() + ggtitle(paste("Alchemy analysis'", title()))    
  })
  
  output$WordCloud <- renderPlot({
    v <- terms()
    wordcloud(v$word, v$freq, scale=c(4,0.7),
                  min.freq = input$freq, max.words=input$max,
                  colors=brewer.pal(8, "Dark2"))
    
  })
  
#  output$tweets <- renderText(({orderedTweets()}))
#  output$dataTable <- renderDataTable({terms()})
})
shinyApp(ui = ui, server = server)
