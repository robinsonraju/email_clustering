require(cluster)

# Cluster using k-means and determine the trend for k
if (VERBOSE)
  print("Calculating Within cluster sum of squares for different values of k")
wss <- (nrow(rh.archive.mails.tdm)-1) * sum(apply(rh.archive.mails.tdm,2,var))
for (i in 2:20) wss[i] <- sum(kmeans(rh.archive.mails.tdm, centers=i)$withinss)

if (VERBOSE)
  print("Clustering using kmeans")
rh.archive.mails.kmeans<-pam(rh.archive.mails.tdm, 15)

if (VERBOSE)
  print("Done with clustering")