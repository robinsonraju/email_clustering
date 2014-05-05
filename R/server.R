
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
# 
# http://www.rstudio.com/shiny/
#

library(shiny)

shinyServer(function(input, output) {
  # 1. Classification
  output$spam <- renderPlot({
    spam_stat <- c(spam.count, ham.count)
    pct <- round(spam_stat/sum(spam_stat)*100)
    lbls <- paste(c("Spam", "Non-Spam"), pct) 
    lbls <- paste(lbls,"%",sep="") 
    pie(spam_stat,labels = lbls, col=rainbow(length(lbls)),main="Spam Stats") 
  })  

  # 2. Clustering
  # wss
  output$wss <- renderPlot({
    plot(1:20, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")
  }) 

  # clusters
  output$cluster <- renderPlot({
    cluster.sizes <- c(rh.archive.mails.kmeans$clusinfo[,1])
    names(cluster.sizes) = c(1:15)
    barplot(cluster.sizes)
  }) 
  
  # 3. Data Mining
  # Top authors
  output$topauth <- renderPlot({
    par(mar=c(5,8,4,2)) # increase y-axis margin.
    par(las=2) # make label text perpendicular to axis
    barplot(top.auth,  
            horiz=TRUE, 
            names.arg=names(top.auth), 
            cex.names=.5)
  }) 
  
  # Top topics
  output$toptopics <- renderPlot({
    par(mar=c(5,8,4,2)) # increase y-axis margin.
    par(las=2) # make label text perpendicular to axis
    barplot(big.topics,  
            horiz=TRUE, 
            names.arg=names(big.topics), 
            cex.names=.5)
  }) 
  
  # Frequent terms
  output$freqterms <- renderPlot({
    pal2=brewer.pal(8,"Dark2")
    wordcloud(freq.terms,freq.terms.freq, scale=c(2,.2),min.freq=3,max.words=Inf, random.order=FALSE, rot.per=.15, colors=pal2)
  })
})

