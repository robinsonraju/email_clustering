MBOX_TRANSFORM_NOT_DONE <- FALSE
if (VERBOSE)
  print("Checking for required libraries")
require(tm.plugin.mail)
require(tm)

if (VERBOSE)
  print("Step 1 : Creating the Corpus")
if (MBOX_TRANSFORM_NOT_DONE){
	print("Converting the mbox file into a folder full of separate text emails")
	convert_mbox_eml(gzfile("data/rr-spam.mbox"),"data/rr-spam/")
}

if (VERBOSE)
  print("Creating Corpus object from spam folder")
rr.spam.mails<-Corpus(DirSource("data/rr-spam/"),list(reader=readMail))
rr.spam.mails.count <- length(rr.spam.mails)

if (VERBOSE)
  print(paste("Number of mails : ", rr.spam.mails.count))

if (VERBOSE)
  print("Detecting threads")
rr.spam.mails.thread<-threads(rr.spam.mails)

if (VERBOSE)
  print("Removing citations, i.e. lines beginning with >")
rr.spam.mails<-tm_map(rr.spam.mails,removeCitation)

if (VERBOSE)
  print("Removing signatures, i.e. anything below '--'")
rr.spam.mails<-tm_map(rr.spam.mails,removeSignature)

if (VERBOSE)
  print("Step 2 : Pre-processing")

if (VERBOSE)
  print("Converting the emails to plain text")
rr.spam.mails<-tm_map(rr.spam.mails,as.PlainTextDocument)

if (VERBOSE)
  print("Converting to lowercase")
rr.spam.mails<-tm_map(rr.spam.mails,tolower)

if (VERBOSE)
  print("Removing common words. stop words, numbers, punctuation")
for(i in 1:rr.spam.mails.count){rr.spam.mails[[i]]<-removeWords(rr.spam.mails[[i]],stopwords("en"))}
for(i in 1:rr.spam.mails.count){rr.spam.mails[[i]]<-removeNumbers(rr.spam.mails[[i]])}
for(i in 1:rr.spam.mails.count){rr.spam.mails[[i]]<-removePunctuation(rr.spam.mails[[i]])}

if (VERBOSE)
  print("Step 3 : Evaluation")
  
if (VERBOSE)
  print("Creating document term matrix")
rr.spam.mails.tdm<-DocumentTermMatrix(rr.spam.mails)
rr.spam.matrix <- as.matrix(rr.spam.mails.tdm)
rr.spam.counts <- rowSums(rr.spam.matrix)

if (VERBOSE)
  print("Creating term frequency table")
rr.spam.df <- data.frame(cbind(names(rr.spam.counts),
                            as.numeric(rr.spam.counts)),
                      stringsAsFactors = FALSE)
names(rr.spam.df) <- c("term", "frequency")
rr.spam.df$frequency <- as.numeric(rr.spam.df$frequency)

if (VERBOSE)
  print("Calculating density and occurrence scores")
rr.spam.occurrence <- sapply(1:nrow(rr.spam.matrix),
                          function(i)
                          {
                            length(which(rr.spam.matrix[i, ] > 0)) / ncol(rr.spam.matrix)
                          })
rr.spam.density <- rr.spam.df$frequency / sum(rr.spam.df$frequency)

if (VERBOSE)
  print("Updating term frequency table with density and occurrence")
rr.spam.df <- transform(rr.spam.df,
                     density = rr.spam.density,
                     occurrence = rr.spam.occurrence)

if (VERBOSE)
  print("Done with reading and training spam data")