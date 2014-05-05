# Functions
if (VERBOSE)
  print("Loading fuction - classify")
classify.feedback <- function(msg, training.df, prior = 0.5, c = 1e-6)
{
  
  msg.corpus <- Corpus(VectorSource(msg))
  msg.corpus <- tm_map(msg.corpus, tolower)
  msg.corpus <- tm_map(msg.corpus, removeWords, stopwords("english"))
  msg.corpus <- tm_map(msg.corpus, removePunctuation)
  msg.tdm <- TermDocumentMatrix(msg.corpus)
  msg.freq <- rowSums(as.matrix(msg.tdm))
  
  # Find intersections of words
  msg.match <- intersect(names(msg.freq), training.df$term)
  
  # Now, we just perform the naive Bayes calculation
  if(length(msg.match) < 1)
  {
    return(prior * c ^ (length(msg.freq)))
  }
  else
  {
    match.probs <- training.df$occurrence[match(msg.match, training.df$term)]
    return(prior * prod(match.probs) * c ^ (length(msg.freq) - length(msg.match)))
  }
}

if (VERBOSE)
  print("Loading fuction - GeoService")
##Function to get geo Co-ordinates based on IP Address
GeoService<- function(x) {
  URL_IP <- paste("http://www.telize.com/geoip/",x, sep = "")
  api_return <- fromJSON( URL_IP, method = "C", unexpected.escape = "error" )
  lat<-api_return$latitude
  lon<-api_return$longitude
  country<-api_return$country_code
  region<-api_return$region
  city<-api_return$city
  
  return(paste(lat, lon,country,region,city, sep = ";"))
}

if (VERBOSE)
  print("Loading fuction - fn")
fn=function(datasheet){
  reuters <- Corpus(VectorSource(datasheet$Feedback))
  reuters <- tm_map(reuters, tolower)
  ## Remove Stopwords
  reuters <- tm_map(reuters, removeWords, stopwords("english"))
  ## Remove Punctuations
  reuters <- tm_map(reuters, removePunctuation)
  ## Remove Numbers
  ## Stemming
  reuters <- tm_map(reuters, stemDocument)
  ## Eliminating Extra White Spaces
  reuters <- tm_map(reuters, stripWhitespace)
  
  verbatims <- unlist(reuters)
  bag <- tolower(as.character(unlist(strsplit(verbatims, " "))))
  
  df <- data.frame(table(bag))
  df=df[!df$bag=="",]
  df$prop <- df$Freq/nrow(df)
  df <- df[order(-df$Freq),]
  as.character(head(df$bag,n=2))
}

classify.email <- function(msg.freq, training.df, prior=0.5, c=1e-6) {
  # Find intersections of words
  msg.match <- intersect(msg.freq, training.df$term)

  # perform the naive Bayes calculation
  if(length(msg.match) < 1)
  {
    return(prior * c ^ (length(msg.freq)))
  }
  else
  {
    match.probs <- training.df$occurrence[match(msg.match, training.df$term)]
    return(prior  * c ^ (length(msg.freq) - length(msg.match)))
  }
}


