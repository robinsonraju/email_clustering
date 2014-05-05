MBOX_TRANSFORM_NOT_DONE <- FALSE
if (VERBOSE)
  print("Checking for required libraries")
require(tm.plugin.mail)
require(tm)

if (VERBOSE)
  print("Step 1 : Creating the Corpus")
if (MBOX_TRANSFORM_NOT_DONE){
  print("Converting the mbox file into a folder full of separate text emails")
  convert_mbox_eml(gzfile("data/2014-March.txt.gz"),"data/rh-good-mail/")
}

if (VERBOSE)
  print("Creating Corpus object from email folder")
rh.good.mails<-Corpus(DirSource("data/rh-good-mail/"),list(reader=readMail))
rh.good.mails.count <- length(rh.good.mails)

if (VERBOSE)
  print(paste("Number of mails : ", rh.good.mails.count))

if (VERBOSE)
  print("Detecting threads")
rh.good.mails.thread<-threads(rh.good.mails)

if (VERBOSE)
  print("Removing citations, i.e. lines beginning with >")
rh.good.mails<-tm_map(rh.good.mails,removeCitation)

if (VERBOSE)
  print("Removing signatures, i.e. anything below '--'")
rh.good.mails<-tm_map(rh.good.mails,removeSignature)

if (VERBOSE)
  print("Step 2 : Pre-processing")

if (VERBOSE)
  print("Converting the emails to plain text")
rh.good.mails<-tm_map(rh.good.mails,as.PlainTextDocument)

if (VERBOSE)
  print("Converting to lowercase")
rh.good.mails<-tm_map(rh.good.mails,tolower)

if (VERBOSE)
  print("Removing common words. stop words, numbers, punctuation")
for(i in 1:rh.good.mails.count){rh.good.mails[[i]]<-removeWords(rh.good.mails[[i]],stopwords("en"))}
for(i in 1:rh.good.mails.count){rh.good.mails[[i]]<-removeNumbers(rh.good.mails[[i]])}
for(i in 1:rh.good.mails.count){rh.good.mails[[i]]<-removePunctuation(rh.good.mails[[i]])}

if (VERBOSE)
  print("Step 3 : Evaluation")

if (VERBOSE)
  print("Creating document term matrix")
rh.good.mails.tdm<-DocumentTermMatrix(rh.good.mails)
rh.good.matrix <- as.matrix(rh.good.mails.tdm)
rh.good.counts <- rowSums(rh.good.matrix)

if (VERBOSE)
  print("Creating term frequency table")
rh.good.df <- data.frame(cbind(names(rh.good.counts),
                               as.numeric(rh.good.counts)),
                         stringsAsFactors = FALSE)
names(rh.good.df) <- c("term", "frequency")
rh.good.df$frequency <- as.numeric(rh.good.df$frequency)

if (VERBOSE)
  print("Calculating density and occurrence scores")
rh.good.occurrence <- sapply(1:nrow(rh.good.matrix),
                             function(i)
                             {
                               length(which(rh.good.matrix[i, ] > 0)) / ncol(rh.good.matrix)
                             })
rh.good.density <- rh.good.df$frequency / sum(rh.good.df$frequency)

if (VERBOSE)
  print("Updating term frequency table with density and occurrence")
rh.good.df <- transform(rh.good.df,
                        density = rh.good.density,
                        occurrence = rh.good.occurrence)

if (VERBOSE)
  print("Done with reading and training good email data")