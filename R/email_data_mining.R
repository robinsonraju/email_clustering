if (VERBOSE)
  print("Finding top authors from all emails")
authors<-lapply(rh.archive.mails, Author)
authors<-sapply(authors, paste, collapse=' ')
top.auth <- sort(table(authors), decreasing=T)[1:10]

if (VERBOSE)
  print("Finding top topics from all emails")
headings<-lapply(rh.archive.mails, Heading)
headings<-sapply(headings, paste, collapse=' ')
sort(table(headings), decreasing=T)[1:10]
big.topics<-sort(table(headings), decreasing=T)[1:10]

if (VERBOSE)
  print("Finding frequent words")
freq.terms <- findFreqTerms(rh.archive.mails.tdm, 200)
freq.terms.freq <- sample(1:1000, length(freq.terms), replace=T) # TODO - temporary solve

# Save the data to file
save(list = ls(all=TRUE), file=file.path(outputDir, 'Message_Clustering.Rdata' ))
load(file.path(outputDir,'Message_Clustering.Rdata'), .GlobalEnv)


if (VERBOSE)
    print("Done with data mininig and saving data")
