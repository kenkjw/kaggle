


require(MASS)
require(data.table)
require(tm)

entropy = function(buys) {
  freqs <- table(buys)/length(buys)
  -sum(freqs * log2(freqs))
}


train = fread('/home/mikeskim/Desktop/kaggle/telstra/data/train.csv',data.table=F)
test = fread('/home/mikeskim/Desktop/kaggle/telstra/data/test.csv',data.table=F)

test$fault_severity=-1

both = rbind(train,test)


feat3 = fread('/home/mikeskim/Desktop/kaggle/telstra/data/event_type.csv')

feat4 = merge(feat3,both,by="id",allow.cartesian = T)

feat4$event_type = sapply(feat4$event_type, function(x) unlist(strsplit(x,' '))[2])
feat4$location = sapply(feat4$location, function(x) unlist(strsplit(x,' '))[2])

feat4$prod1 = paste(feat4$event_type,feat4$location,sep='')

feats4 <- feat4[ ,.(
  alltext = paste0(prod1,collapse=' ')
), id]


feats = data.frame(feats4)
tmpid = feats$id

DTM = function(corpus){
  dtm = DocumentTermMatrix(corpus,control=list(tolower=F,removePunctuation=F,removeNumbers=F,stopwords=F, wordLengths=c(1,Inf),
                                               stemming=F))#,weighting=function(x) weightTfIdf(x,normalize=T)))
  return (dtm)
}

#I don't have to make feats2 into a data.frame? wow!

all_text = Corpus(VectorSource(feats$alltext))

gc()

dtm = DTM(all_text)
dim(dtm)
dtm <- removeSparseTerms(dtm, 0.992)#795
dim(dtm)
df_q = as.data.frame(as.matrix(dtm))
colnames(df_q) = paste("q_",colnames(df_q),sep="")
df_q$id = tmpid



write.csv(df_q, "/home/mikeskim/Desktop/kaggle/telstra/features/feats80tm.csv", quote=FALSE, row.names = FALSE)


