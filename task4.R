ds6<- read.csv(file="task3LDA2.csv", header = T, sep=",")
ds6hcl<-ds6[,-c(1:11)]
distan <- dist(ds6hcl, method = "euclidean") # distance matrix
fit <- hclust(distan, method="average") 
plot(fit, hang=-1) # display dendogram
groups <- cutree(fit, k=10) # cut tree into 10 clusters
# draw dendogram with red borders around the 10 clusters 
h <- rect.hclust(fit, k=10, border="red")
hcltab<-table(groups,ds6[,ncol(ds6)])

## find the clusters and the accuracy
temptable<-hcltab
cornum<-0
colnames<-c("correctIntance","totalIntance","accuracyOfeachCluster")
result<-matrix(0, 10,3,dimnames=list(1:10,colnames))
for(h in 1:10) {
  maxnnum<-max(temptable)
  cornum<-cornum+maxnnum
  if (maxnnum == 0){
    break
  } else {
    numloc<-which(temptable[,]==maxnnum)
    result[h,1]<-maxnnum
    coltemp<-numloc%/%10+1
    rowtemp<-numloc%%10
    result[h,2]<-sum(temptable[rowtemp,])
    result[h,3]<-maxnnum/sum(temptable[rowtemp,]) ##calculate the accuracy of each clusters
    rownames(hcltab)[rowtemp]<-colnames(temptable)[coltemp]
    temptable[,coltemp]<-0
    temptable[rowtemp,]<-0
  }
}
rownames(hcltab)
accuracy<-cornum/sum(hcltab,na.rm=TRUE)
data.frame(rownames(hcltab))
accuracy
result
