library(RColorBrewer)
library(base64enc)
library(ggplot2)
library(shiny)
library(twitteR)

#config.R needs to have the twitter and IBM API keys
#twitter: consumerKey, consumerSecret, tokenKey, tokenSecret
#alchemy (IBM): sentimentKey
source("config.R")
source("helper.R")

setup_twitter_oauth(consumerKey, consumerSecret, tokenKey, tokenSecret)
pos <- scan('res/positive-words.txt', what='character', comment.char=';')
neg <- scan('res/negative-words.txt', what='character', comment.char=';')

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
    paste("results for", input$term, sep=" ")
  })
  tweets <- eventReactive(input$go, {
    searchTwitter(input$term, n=input$nTweets, lang="en")
  })
  tweetsDf <- reactive({twListToDF(tweets())})
  orderedTweets <- reactive({tweetsDf()[, order(names(tweetsDf()))]})
  terms <- reactive({build_wordcloud_matrix(orderedTweets(), input$term, input$freq)})
  
  analysis <- eventReactive(input$go, {
    score.sentiment(orderedTweets()$text, pos, neg)
  })
  AlchemyAnalysis <- eventReactive(input$go, {
    score.alchemy_sentiment(orderedTweets()$text, input$term)
  })
  
  output$LHist <- renderPlot({
    #histogram
    condBlue <- analysis()$score == 0
    condGreen <- analysis()$score >= 1
    condRed <- analysis()$score <= -1
    
    ggplot(analysis(), aes(x=score)) +
      geom_histogram(data=subset(analysis(),condBlue==TRUE), binwidth=1, fill="#3498DB") +
      geom_histogram(data=subset(analysis(),condGreen==TRUE), binwidth=1, fill="#2ECC71") +
      geom_histogram(data=subset(analysis(),condRed==TRUE), binwidth=1, fill="#D91E18") +
      scale_x_continuous(breaks=analysis()$score+0.5, labels=analysis()$score)+
      ggtitle(paste("Lexical analysis'", title())) +
      labs(x = "+/- word differential", y = "count") +
      theme_bw() + theme(panel.grid.major = element_blank(),
                         axis.line = element_line(colour = "black"))
      
      
  })
  output$ADens <- renderPlot({
    if(input$go==0) return(NULL)
    withProgress(message = 'Calculation in progress',
                 detail = 'This may take a while...', value = 0, {
                   for (i in 1:input$nTweets) {
                     incProgress(1/input$nTweets)
                   }
                 })
    ggplot(AlchemyAnalysis(), aes(x=score)) + geom_density(fill="#DCC6E0") + ggtitle(paste("Alchemy analysis'", title())) +
      labs(x = "Sentiment Score", y = "Density") + theme_light()
  })
  
  output$WordCloud <- renderPlot({
    v <- terms()
    wordcloud(v$word, v$freq, scale=c(4,0.7),
                  min.freq = input$freq, max.words=input$max,
                  colors=brewer.pal(8, "Dark2"))
    
  })
  
})
shinyApp(ui = ui, server = server)
