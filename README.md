email_clustering
================

Objective : 
To group emails into meaningful clusters

Process: 
(Sample for spam mails. Others are similar) 

> install.packages("tm.plugin.mail")
> library(tm.plugin.mail)
> library(tm)

Creating the corpus – Getting Spam mail
----------------------------------------
1.	Emails from gmail can be obtained in mbox format by downloading it from Google. 
Access it from https://www.google.com/settings/takeout/downloads 

2.	Convert the mbox file into separate text emails.  This will create a separate text file for each email and place them in a folder of your choice in the R working directory.
Go to 'data' folder and run the command - 
> convert_mbox_eml(gzfile("data/rr-spam.mbox"),"data/rr-spam/")
number of files that I see is 153. It starts with 001 and goes till 153.

3.	Create the corpus. 
> rr_spam_mails<-Corpus(DirSource("data/rr-spam/"),list(reader=readMail))
> rr_spam_mails
Returns "A corpus with 153 text documents"
The corpus should contain the same number of documents as emails in the directory you created.  
You can examine the first email with:

> rr_spam_mails[[1]]
> meta(rr_spam_mails[[1]])

4.	Detect threads.  This is a one-pass algorithm for extracting thread information.  It returns a thread ID and a thread depth (how many emails in the thread).  Note that since it is a one-pass algorithm, it does not detect replies that appear in the list before the base emails (and will mark these with ID “NA” and depth 2).
> rr_spam_mails.thread<-threads(rr_spam_mails)
> rr_spam_mails.thread


Preprocessing
--------------
5.	Remove citations, i.e. lines beginning with >.
> rr_spam_mails<-tm_map(rr_spam_mails,removeCitation)
> rr_spam_mails[[1]]

Remove signatures, i.e. anything below “-- “ (dash dash blank, the official signature start mark) and any other characters of your choice.  Note: this will have some trouble with more than 2 dashes.
> rr_spam_mails<-tm_map(rr_spam_mails,removeSignature)
	(or  > rr_spam_mails[[1]]<-removeSignature(rr_spam_mails[[1]],marks="^[+]-*[+]$"))

Initial Exploration 
--------------------
6.	Determine the most active writers.  First extract all of the authors and then normalize multiple entries (collapse those with multiple lines into one line).
> authors<-lapply(rr_spam_mails,Author)
> authors<-sapply(authors,paste,collapse=' ')

Sort by number of emails authored and look at the top 10.
> sort(table(authors), decreasing=T)[1:10]

7.	Do the same thing with “Heading” for another way to look at thread/topic information.
> headings<-lapply(rr_spam_mails,Heading)
> headings<-sapply(headings,paste,collapse=' ')
> sort(table(headings),decreasing=T)[1:10]

> big.topics<-names(sort(table(headings),decreasing=T)[1:10])
> big.topics
> unique(sapply(rr_spam_mails[headings==big.topics[1]],Author))
> unique(sapply(rr_spam_mails[headings==big.topics[2]],Author))

8.	Determine how many of the emails discuss a certain term.  Perform a search of the emails for a specific term, say “problem,” to determine (estimate) how many of the emails are dealing with problems in R.
> p.filter<-tm_filter(rr_spam_mails, FUN = function(x) any(grep("eat", x)))
> p.filter

for this corpus, 85 of the emails contain the term “eat”?

Determine the 10 most active authors for the term “eat”.
> p.authors<-lapply(p.filter,Author)
> p.authors<-(sapply(p.authors,paste,collapse=' '))
> sort(table(p.authors),decreasing=T)[1:10]

Count-based Evaluation
-----------------------
The emails need to be in plain text format in order to create the document term matrix.  It will also be useful to remove common words and punctuation before creating the document term matrix.

9.	Convert the emails to plain text.  Show what this does to the metadata.
> rr_spam_mails<-tm_map(rr_spam_mails,as.PlainTextDocument)
> meta(rr_spam_mails[[1]])

Convert to lowercase.
> rr_spam_mails<-tm_map(rr_spam_mails,tolower)
> rr_spam_mails[[1]]
10.	Remove common words.
Remove stop words
> mail_count <- length(rr_spam_mails)
> for(i in 1:mail_count){rr_spam_mails[[i]]<-removeWords(rr_spam_mails[[i]],stopwords("en"))}
Remove numbers.
> for(i in 1:mail_count){rr_spam_mails[[i]]<-removeNumbers(rr_spam_mails[[i]])}
Remove punctuation.
> for(i in 1:mail_count){rr_spam_mails[[i]]<-removePunctuation(rr_spam_mails[[i]])}
> rr_spam_mails[[1]]

11.	Create the document term matrix.
> rr_spam_mails.tdm<-DocumentTermMatrix(rr_spam_mails)
> rr_spam_mails.tdm


rr_spam.matrix <- as.matrix(rr_spam_mails.tdm)
rr_spam.counts <- rowSums(rr_spam.matrix)

rr_spam.df <- data.frame(cbind(names(rr_spam.counts),
                            as.numeric(rr_spam.counts)),
                      stringsAsFactors = FALSE)

names(rr_spam.df) <- c("term", "frequency")
rr_spam.df$frequency <- as.numeric(rr_spam.df$frequency)
rr_spam.occurrence <- sapply(1:nrow(rr_spam.matrix),
                          function(i)
                          {
                            length(which(rr_spam.matrix[i, ] > 0)) / ncol(rr_spam.matrix)
                          })
rr_spam.density <- rr_spam.df$frequency / sum(rr_spam.df$frequency)

rr_spam.df <- transform(rr_spam.df,
                     density = rr_spam.density,
                     occurrence = rr_spam.occurrence)
                      