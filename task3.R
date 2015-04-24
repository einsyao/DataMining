library("e1071")
library("randomForest")

##### focus on the 10 most populous classes

ds7<- read.csv(file="task1.csv", header = T, sep=",")
ds81 <- read.csv(file="task2lda.csv", header = T, sep=",")
ds82 <- read.csv(file="task2LDA&SelectedFeatures.csv")
targettopic <- c("topic.earn", "topic.acq", "topic.money.fx", "topic.grain", "topic.crude", "topic.trade", "topic.interest", "topic.ship", "topic.wheat", "topic.corn")
ncoltopic <- c()
for (numCol in 1:length(targettopic)){
  ncoltopic <- c(ncoltopic, which(colnames(ds7) == targettopic[numCol]))
}

## process two different version
##ds8<-ds81
ds8<-ds82

ds9 <- cbind(ds7[,c(3,ncoltopic)], ds8)
ds9 <- ds9[-which(apply(ds9[,2:11], 1, sum) == 0),] 
ds9[,"class"] <- NA

### create 'class' based on the 10 given classes.
ds10 <- c()
addrow<-c()
for (y in 1:nrow(ds9)) {
  for (z in 2:11){
    if (ds9[y,z]==1){
      addrow<-ds9[y,]
      addrow[2:11]<-0
      addrow[z] <- 1
      addrow[ncol(ds9)]<-targettopic[z-1]
      ds10<- rbind(ds10,addrow)
    }    
  }
  print(y)
}

nrow(ds10) 
#write.csv(ds10, "task3LDA1.csv", row.names = F)
write.csv(ds10,"task3LDA2.csv",row.names = F)


### load data

#AI <- read.csv(file="task3LDA1.csv", header = T, sep=",")
 AI<-read.csv(file="task3LDA2.csv",header = T,sep=",")


AI <- AI[,-c(1:11)]

# 10-fold 
AI <- AI[sample(nrow(AI),nrow(AI)),]

####Define the tables
rownames<-c("TP","FN","FP","Recall","Precision","Accuracy","F-Measure")
colnames<-c("topic.earn","topic.acq","topic.money.fx","topic.grain","topic.crude","topic.trade","topic.interest","topic.ship", "topic.wheat", "topic.corn")
colnamesrc<-c("topic.earn","topic.acq","topic.money.fx","topic.grain","topic.crude","topic.trade","topic.interest","topic.ship", "topic.wheat", "topic.corn","R-macro","R-micro")
colnamespr<-c("topic.earn","topic.acq","topic.money.fx","topic.grain","topic.crude","topic.trade","topic.interest","topic.ship", "topic.wheat", "topic.corn","P-macro","P-micro")
table <- matrix(0,7,10,dimnames=list(rownames,colnames))
rownamesR<-c(1:10)
recall<- matrix(0,10,12,dimnames=list(rownamesR,colnamesrc))
precision <- matrix(0,10,12,dimnames=list(rownamesR,colnamespr))
accuracy <- matrix(0,10,10,dimnames=list(rownamesR,colnames))
fMeasure <- matrix(0,10,10,dimnames=list(rownamesR,colnames))


### run the classifation in 10 folds
temp = 0
for(i in 1:10) {
  domain <-((i-1)*999+1):(i*999)
  pre<- AI[domain,]
  tra<- AI[-domain,]
  
  r <- svm(class ~., data = tra,kernel ="linear") 
  #   r <- naiveBayes(class ~., data = tra)
  #       r <- svm(class ~., data = tra) 
 #  r <- randomForest(class~., data = tra) 
  
  p <- predict(r,pre[,-ncol(pre)])
  t <- table(p, pre[,ncol(pre)])
  temp <-t+temp
  stp <- 0
  for(j in 1:10){
    #TP
    tp <- t[j,j]
    table[1,j]<-tp+table[1,j]
    stp <-stp + tp
    #FN
    fn <- sum(t[j,], na.rm = TRUE) - tp
    table[2,j] <- table[2,j] +fn
    #FP
    fp <- sum(t[,j], na.rm = TRUE) - tp
    table[3,j] <- table[3,j] + fp
    #recall
    recall[i,j] <- tp/(tp+fn)
    recall[i,11] <- recall[i,j]+recall[i,11]
    recall[i,12] <- recall[i,12] +tp+fn
    #Precision
    precision[i,j] <- tp/(tp+fp)
    precision[i,11] <- precision[i,j]+precision[i,11]
    precision[i,12]<- precision[i,12]+tp+fp
    #accuracy
    accuracy[i,j] <-tp/999
    #fMeasure
    fMeasure[i,j] <- 2*recall[i,j]*precision[i,j]/(recall[i,j]+precision[i,j])
  }
  recall[i,11] <- recall[i,11]/10
  recall[i,12] <- stp/recall[i,12]
  precision[i,11] <- precision[i,11]/10
  precision[i,12] <- stp/precision[i,12]
}

for(i in 1:10){
  table[4,i] <- table[1,i]/(table[1,i]+table[2,i])
  table[5,i] <- table[1,i]/(table[1,i]+table[3,i])
  table[6,i] <- table[1,i]/9990
  table[7,i] <- 2*table[4,i]*table[5,i]/(table[4,i]+table[5,i])
}
cat("\nThe final Confusion Matrix\n")
print(temp)
cat("\nValue of recall in each fold\n")
print(recall)
cat("\nValue of Precision in each fold \n")
print(precision)
cat("\nValue of accuracy in each fold \n")
print(accuracy)
cat("\nValue of F-Measure in each fold \n")
print(fMeasure)
cat("\nThe final performance measures \n")
print(table)
aveAccuracy <- sum(accuracy[1:10,1:10], na.rm = TRUE)/10
print(aveAccuracy)

###### run the train data set and predict the test data.

#ai <- read.csv(file="task3LDA1.csv", header = T, sep=",")
ai <-read.csv(file="task3LDA2.csv",header = T,sep=",")


aitrain<-ai[which(ai$purpose=="train"),]
aitest <- ai[which(ai$purpose=="test"),]
aitrain <- aitrain[,-c(1:11)]
aitest <- aitest[,-c(1:11)]

r <- svm(class ~., data = aitrain,kernel ="linear") 
#     r <- naiveBayes(class ~., data = aitrain)
#       r <- svm(class ~., data = aitrain) 
#r <- randomForest(class~., data = aitrain)
p <- predict(r,aitest[,-ncol(aitest)])
t <- table(p, aitest[,ncol(aitest)])

rownames<-c("TP","FN","FP","Recall","Precision","Accuracy")
colnames<-c("topic.earn","topic.acq","topic.money.fx","topic.grain","topic.crude","topic.trade","topic.interest","topic.ship", "topic.wheat", "topic.corn")
tableai <- matrix(0,6,10,dimnames=list(rownames,colnames))

colnames2<-c("macrorecall", "microrecall","macroprecision","microprecision","averageAccuracy")
result<-matrix(0,1,5,dimnames=list("result",colnames2))

for (A in 1:10){
  tableai[1,A] <- t[A,A] #tp
  tableai[2,A] <- (sum(t[A,])-t[A,A]) #fn
  tableai[3,A] <- (sum(t[,A])-t[A,A]) #fp
  tableai[4,A] <- tableai[1,A]/(tableai[1,A]+tableai[2,A]) #recall
  tableai[5,A] <- tableai[1,A]/(tableai[1,A]+tableai[3,A]) #precision
  tableai[6,A] <- t[A,A]/sum(t[A,],na.rm=TRUE) #accuracy
}
tableai
result[1,1] <- sum(tableai[4,],na.rm=TRUE)/10
result[1,3] <- sum(tableai[5,])/10
result[1,2] <- sum(tableai[1,])/(sum(tableai[1,],na.rm=TRUE)+sum(tableai[2,],na.rm=TRUE))
result[1,4] <- sum(tableai[1,])/(sum(tableai[1,],na.rm=TRUE)+sum(tableai[3,],na.rm=TRUE))
result[1,5] <- sum(tableai[1,1:10])/sum(data.frame(t)[3],na.rm=TRUE)

result
