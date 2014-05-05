#
# scrape.R - 
# scrapes twitter 
#

if (VERBOSE)
  print("Searching tweets and saving to disk")

require(twitteR)

if (VERBOSE)
  print("RR tweets")
rr.tweets = searchTwitter('@robinsonraju', n=1500)
save(rr.tweets, file=file.path(dataDir, 'rr.tweets.RData' ), ascii=T)

if (VERBOSE)
  print("PC tweets")
pc.tweets = searchTwitter('@paulocoelho', n=1500)
save(pc.tweets, file=file.path(dataDir, 'pc.tweets.RData' ), ascii=T)
