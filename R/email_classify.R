if (VERBOSE)
  print("Calculating probability of Spam")
pr.spam <- sapply(rh.archive.df$term, function(x) classify.email(x, training.df = rr.spam.df))

if (VERBOSE)
  print("Calculating probability of Ham")
pr.ham <- sapply(rh.archive.df$term, function(x) classify.email(x, training.df = rh.good.df))

rh.archive.mails.res <- ifelse(pr.spam > pr.ham, TRUE, FALSE)
totalMessages <- length(rh.archive.mails.res)

ham.count <- 0
spam.count <- 0
for(i in 1:length(rh.archive.mails.res)){
  if(rh.archive.mails.res[i]){
    spam.count<-spam.count+1
  }
  else{
    ham.count<-ham.count+1
  }
}

if (VERBOSE){
  print(paste("Total messages : ", totalMessages))
  print(paste("Number of spam messages : ", spam.count))
  print(paste("Number of non spam messages : ", ham.count))
}

if (VERBOSE)
  print("Done with classifying emails to Spam/Ham")

