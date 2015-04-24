library("e1071")
library("koRpus")
library("NLP")
library("openNLP")
library("tau")
library("tm")
library("randomForest")
install.packages("http://datacube.wu.ac.at/src/contrib/openNLPmodels.en_1.5-1.tar.gz", repos = NULL, type = "source")
ds<- read.csv(file="reutersCSV.csv", head = T, sep=",")

###data preprocessing

## 1.data clean.

# delete the rows whose all values of topics are zero.

temp <- c()
for (i in 1:nrow(ds)){
  if (sum(ds[i,4:138])==0)
    temp<- c(temp,i)
} 
ds_row<-ds[-temp,]

# delete the columns whose all values of topics are zero.

temp1 <- c()
for (i in 4:138){
  if (sum(ds_row[,i])==0)
    temp1 <- c(temp1,i)
}
ds_col<- ds_row[,-temp1]

# delete the not used rows.
ds1<-ds_col[-c(which(ds_col$purpose=="not-used")),]

write.csv(ds1, "datacleaning.csv", row.names = F)

# ds2<- read.csv(file="datacleaning.csv", head = T, sep=",")

## 2. bag of words approach
ds2<-ds1

# Preparation
## add one column to record the combination of title and text
ds2[, "titleText"] <- NA
## add one column to record the result of bag of words approach
ds2[, "processedTitleText"] <- NA

####annotator
sentTokenAnnotator <- Maxent_Sent_Token_Annotator()
wordTokenAnnotator <- Maxent_Word_Token_Annotator()
posTagAnnotator <- Maxent_POS_Tag_Annotator()

personEntityAnnotator <- Maxent_Entity_Annotator(kind = "person")
locationEntityAnnotator <- Maxent_Entity_Annotator(kind = "location")
organizationEntityAnnotator <- Maxent_Entity_Annotator(kind = "organization")
dateEntityAnnotator <- Maxent_Entity_Annotator(kind = "date")
moneyEntityAnnotator <- Maxent_Entity_Annotator(kind = "money")
percentageEntityAnnotator <- Maxent_Entity_Annotator(kind = "percentage")


entities <- function(doc, kind) {
  s <- doc$content
  a <- annotations(doc)[[1]]
  if(hasArg(kind)) {
    k <- sapply(a$features, `[[`, "kind")
    s[a[k == kind]]
  } else {
    s[a[a$type == "entity"]]
  }
}

###combine the title and text
for(v in 1:nrow(ds2)){
  combinetext <- as.String(c(as.String(ds2[v,122]), as.String(ds2[v,123])))
  ds2[v,124] <- gsub(",", "", combinetext)
  print(v)
}

#### bag of words approach
  for (j in 1:nrow(ds2)) {
    ##check the cell
    if (ds2[j,124] != ""){
      # 2.1 Tokenize: divide into words
      text<-as.String(ds2[j,124])
      text<-gsub("/", " ", text)# replace "/" by blanks
      dstokenize <- scan_tokenizer(text)
      
      # 2.2 Tweet normalisation
      #these documents are not tweets. So this step can be ignored.
      
      # 2.3 Remove punctuation, remove links or replace with link placehloder.
      dsremove <- removePunctuation(dstokenize, preserve_intra_word_dashes = TRUE)
      
      # 2.4 Run POS tagger(part of speech)
      dspos <- treetag(dsremove, treetagger="manual", format="obj", TT.tknz = FALSE , lang="en", TT.options = list(path="./TreeTagger", preset="en"))
      
      # 2.5. Perform lemmatisation and/or stemming
      dslemma <- dspos@TT.res$lemma
      dstoken <- dspos@TT.res$token
      for (k in 1: length(dslemma)){
        if (dslemma[k] == "<unknown>"|dslemma[k] == "@card@")
          dslemma[k] <- dstoken[k]
      }
      
      # 2.6 Remove stop words
      sentence <- paste(dslemma, collapse = " ")
      ann <- annotate(sentence, list(sentTokenAnnotator,wordTokenAnnotator))
      ann2 <- annotate(sentence,posTagAnnotator,ann)
      
      ann3 <- subset(ann2, type == "word")
      pos <- sapply(ann3$features, '[[', "POS")
      temp2 <- c()
      p <- 0
      for (o in 1:length(dslemma)){
        if (pos[o] == "PRP" | pos[o] == "IN" | pos[o] == "TO" | pos[o] == "MD" | pos[o] == "DT" | pos[o] == "CC" | pos[o] == "WDT" | pos[o] == "VBP" | pos[o] == "PRP$") {
          temp2 <- c(temp2, o)
          p <- 1
        }          
      }
      if (length(temp2) != 0){
        dslemma2 <- dslemma[-temp2]
      }else{
        dslemma2 <- dslemma
      }
          
      sentence2 <- paste(dslemma2, collapse = " ")
      
      # 2.7 Run Named Entity Recogniser
      sentence2 <-as.String(sentence2)
      ann4 <- annotate(sentence2,list(sentTokenAnnotator,wordTokenAnnotator,personEntityAnnotator,locationEntityAnnotator,organizationEntityAnnotator,dateEntityAnnotator,moneyEntityAnnotator,percentageEntityAnnotator))
      ### check the sentence if it has entities or not
      if (ann4$type[nrow(as.data.frame(ann4))] == "entity"){
        noSen<-0
        noWor<-0
        noEnt<-0
        for(q in 1:length(ann4$type)){
          if (ann4$type[q]=="sentence")
            noSen <- noSen+1
          else if (ann4$type[q]=="word")
            noWor <- noWor +1
          else 
            noEnt <- noEnt +1
        }
        a<-noSen+noWor+1
        b<-length(ann4$type)
        ent <- ann4[a:b]
        ent1 <- data.frame(ent)
        ent2 <- ent1[order(ent1[,3]),]
        c<-noSen+1
        d<-noSen+noWor
        wor <- ann4[c:d]
        wor1 <- data.frame(wor)
        
        
    ### find the location of words and figure out how many words can be replaced.
        for (l in 1:noEnt){
          for (m in 1:noWor){
            if (wor1[m,3] == ent2[l,3]) {
              dslemma2[m] <- as.character(ent2[l,5][[1]])
              if (wor1[m,4] != ent2[l,4]){
                for (n in 1:10){
                  dslemma2[m+n] <- as.character(ent2[l,5][[1]])
                  if (wor1[m+n,4]==ent2[l,4])
                    break
                
                }
              }
            }
          }
        }
        result <- paste(dslemma2, collapse = " ")
      } else {
        result<-sentence2
      }
    ### replace the numbers by num.
      numchecker<- gsub("[[:digit:]]+","num",result)
      ds2[j,125]<-numchecker
    }
    print(j)
  }

write.csv(ds2, "task1.csv", row.names = F)


