library("NLP")
library("tm")
library("topicmodels")
#### Taks 2 

## generate the term frequency matrix
ds3 <- read.csv(file="task1.csv", header = T, sep=",")
vec<-ds3[,ncol(ds3)]
ds3vec <- VectorSource(vec)
ds3corpus <- Corpus(ds3vec)
ds3dtm <- DocumentTermMatrix(ds3corpus)
# help tf*idf to select feature
ds3rst <- removeSparseTerms(ds3dtm,sparse=0.95)

#### 1 Feature selection utilizing tf*idf
ds4 <- as.data.frame(inspect(ds3rst))
colds4 <- ncol(ds4)
rowds4 <- nrow(ds4)
resulttfidf <- matrix(0,rowds4,colds4) 

##write.csv(ds4, "ds4.csv", row.names = F)

##calculate tf*idf

for (e in 1:colds4){
  for (f in 1:rowds4){
    ### calculate TF
    # Frequency of term t in document d/ max frequency of t in any doc d
    tf <- ds4[f,e] / max(ds4[,e])
    ### calculate DF
    # Number of documents d in collection containing term t
    ### calculate idf
    # log |D|/(df(t,D)) 
    df <- length(which(ds4[,e] != 0))
    idf <- log2(rowds4 / df)
    ### calculate tf*idf
    tfidf <- tf *  idf
    resulttfidf[f,e] <- tfidf
  }
  print(e)
}

## make the table of tfidf of terms
ds5 <- as.data.frame(resulttfidf)
colnames(ds5) <- colnames(ds4)

write.csv(ds5, "tfidf.csv", row.names = F)

##### make the binary data
for (h in 1:nrow(ds5)){
  replaceno<-c(which(ds5[h,]!=0))
  replacecell<-ds5[h,replaceno]
  dslength <- length(replacecell)
  if (dslength != 0) {
    ds5mean<- sum(replacecell)/dslength
      for (r in replaceno){
        if (ds5[h,r] < ds5mean){
          ds5[h,r] <- 0
        } else {
          ds5[h,r] <- 1
        }
      }
  }
  print(h)
}


write.csv(ds5, "task2tfidf.csv", row.names = F)

summary(ds5)

#### reduce the terms again.
sumds5<- apply(ds5, 2, sum)
dstfidf <- ds5[,-which(sumds5 <= mean(sumds5))]

write.csv(dstfidf, "task2SelectedFeatures.csv", row.names = F)
read.csv(file="task2SelectedFeatures.csv", header = T, sep=",")


#### 2 LDA feature selection
ds3 <- read.csv(file="task1.csv", header = T, sep=",")
vec<-ds3[,ncol(ds3)]
ds3vec <- VectorSource(vec)
ds3corpus <- Corpus(ds3vec)
ds3dtm <- DocumentTermMatrix(ds3corpus)

## implement the lda. 
lda <- LDA(ds3dtm, control = list(alpha = 0.1), k = 10, method = "Gibbs")

## select top 10 terms of each topic.
term <- terms(lda,15)
termtop <- c(term[,1:10])
uniqueterm <- unique(termtop)


## create table to record the topic model result.
topics <- topics(lda)
termname <- uniqueterm
dslda <- as.data.frame(matrix(0, length(topics), length(termname)))
colnames(dslda) <- termname

## generate the connection between documents and topic models.
for (l in 1: length(topics)) {
  rowtopic<- term[,topics[l]]
  termmark<-match(rowtopic,uniqueterm)
  dslda[l,termmark]<-1
}
ncol(dslda)
write.csv(dslda, "task2lda.csv", row.names = F)


##### 3.combine the tf*idf with the lda features

dslda2 <- read.csv(file="task2lda.csv", header = T, sep=",")
dssf <- read.csv(file="task2SelectedFeatures.csv", header = T, sep=",")

colnames(dslda2)
colnames(dssf)
## find the columns of both data frames which terms are matched.
m <- match(colnames(dssf), colnames(dslda2))
repeatterm<-c()
colnumber<-c()
for (no in 1:length(dssf)){
  if (!is.na(m[no])){
    repeatterm <- c(repeatterm,m[no])
    colnumber <- c(colnumber,no)
  }
}
##  combine
dscombine <- cbind(dssf, dslda2[,-repeatterm])
##  fulfil the repeated cells.
for (r in 1:length(colnumber)){
  for (s in 1:nrow(dssf)){
    if (dslda2[s,repeatterm[r]] || dssf[s,colnumber[r]]){
      dscombine[s,colnumber[r]] <- 1
    }
  }
  print(r)
}

write.csv(dscombine, "task2LDA&SelectedFeatures.csv", row.names = F)
