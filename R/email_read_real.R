MBOX_TRANSFORM_NOT_DONE <- FALSE
if (VERBOSE)
  print("Checking for required libraries")
require(tm.plugin.mail)
require(tm)

if (VERBOSE)
  print("Step 1 : Creating the Corpus")
if (MBOX_TRANSFORM_NOT_DONE){
  print("Converting the mbox file into a folder full of separate text emails")
  convert_mbox_eml(gzfile("data/2014-March.txt.gz"),"data/rh-archive-2014Mar/")
}

if (VERBOSE)
  print("Creating Corpus object from email folder")
rh.archive.mails<-Corpus(DirSource("data/rh-archive-2014Mar/"),list(reader=readMail))
rh.archive.mails.count <- length(rh.archive.mails)

if (VERBOSE)
  print(paste("Number of mails : ", rh.archive.mails.count))

if (VERBOSE)
  print("Detecting threads")
rh.archive.mails.thread<-threads(rh.archive.mails)

if (VERBOSE)
  print("Removing citations, i.e. lines beginning with >")
rh.archive.mails<-tm_map(rh.archive.mails,removeCitation)

if (VERBOSE)
  print("Removing signatures, i.e. anything below '--'")
rh.archive.mails<-tm_map(rh.archive.mails,removeSignature)

if (VERBOSE)
  print("Step 2 : Pre-processing")

if (VERBOSE)
  print("Converting the emails to plain text")
rh.archive.mails<-tm_map(rh.archive.mails,as.PlainTextDocument)

if (VERBOSE)
  print("Converting to lowercase")
rh.archive.mails<-tm_map(rh.archive.mails,tolower)

if (VERBOSE)
  print("Removing common words. stop words, numbers, punctuation")
for(i in 1:rh.archive.mails.count){rh.archive.mails[[i]]<-removeWords(rh.archive.mails[[i]],stopwords("en"))}
for(i in 1:rh.archive.mails.count){rh.archive.mails[[i]]<-removeNumbers(rh.archive.mails[[i]])}
for(i in 1:rh.archive.mails.count){rh.archive.mails[[i]]<-removePunctuation(rh.archive.mails[[i]])}

if (VERBOSE)
  print("Step 3 : Evaluation")

if (VERBOSE)
  print("Creating document term matrix")
rh.archive.mails.tdm<-DocumentTermMatrix(rh.archive.mails)
rh.archive.matrix <- as.matrix(rh.archive.mails.tdm)
rh.archive.counts <- rowSums(rh.archive.matrix)

if (VERBOSE)
  print("Creating term frequency table")
rh.archive.df <- data.frame(cbind(names(rh.archive.counts),
                               as.numeric(rh.archive.counts)),
                         stringsAsFactors = FALSE)
names(rh.archive.df) <- c("term", "frequency")
rh.archive.df$frequency <- as.numeric(rh.archive.df$frequency)

if (VERBOSE)
  print("Calculating density and occurrence scores")
rh.archive.occurrence <- sapply(1:nrow(rh.archive.matrix),
                             function(i)
                             {
                               length(which(rh.archive.matrix[i, ] > 0)) / ncol(rh.archive.matrix)
                             })
rh.archive.density <- rh.archive.df$frequency / sum(rh.archive.df$frequency)

if (VERBOSE)
  print("Updating term frequency table with density and occurrence")
rh.archive.df <- transform(rh.archive.df,
                        density = rh.archive.density,
                        occurrence = rh.archive.occurrence)

if (VERBOSE)
  print("Done with reading and training email data")