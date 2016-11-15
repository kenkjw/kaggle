require(tm)
require(data.table)

train = fread('/home/mikeskim/Desktop/kaggle/telstra/data/train.csv',data.table=F)
test = fread('/home/mikeskim/Desktop/kaggle/telstra/data/test.csv',data.table=F)

test$fault_severity=-1

both = rbind(train,test)


feat1 = fread('/home/mikeskim/Desktop/kaggle/telstra/data/event_type.csv')
feat2 = fread('/home/mikeskim/Desktop/kaggle/telstra/data/severity_type.csv')
feat3 = fread('/home/mikeskim/Desktop/kaggle/telstra/data/resource_type.csv')
feat4 = fread('/home/mikeskim/Desktop/kaggle/telstra/data/log_feature.csv')

feats1 <- feat1[ ,.(
  V1 = paste0(sub(' ','_',event_type),collapse=' ')
), id]

feats2 <- feat2[ ,.(
  f2 = paste0(sub(' ','_',severity_type),collapse=' ')
), id]

feats3 <- feat3[ ,.(
  f3 = paste0(sub(' ','_',resource_type),collapse=' ')
), id]

feats4 <- feat4[ ,.(
  f4 = paste0( rep(sub(' ','_',log_feature),volume), collapse=' ')
), id]

feats = cbind(feats1,feats2$f2,feats3$f3,feats4$f4)
F1 = nchar(feats$V1)
#F2 = nchar(feats$V2)
F3 = nchar(feats$V3)
F4 = nchar(feats$V4)


feats$alltext = paste(feats$V1,feats$V2,feats$V3,feats$V4,sep=' ')
feats$V1=NULL; feats$V2=NULL; feats$V3=NULL; feats$V4=NULL

feats = data.frame(feats)


DTM = function(corpus){
  dtm = DocumentTermMatrix(corpus,control=list(tolower=F,removePunctuation=F,removeNumbers=F,stopwords=F, wordLengths=c(1,Inf),
                                               stemming=F))#,weighting=function(x) weightTfIdf(x,normalize=T)))
  return (dtm)
}

#I don't have to make feats2 into a data.frame? wow!

all_text = Corpus(VectorSource(feats$alltext))

gc()

dtm = DTM(all_text)
dtm <- removeSparseTerms(dtm, 0.9999999)#795
dim(dtm)
df_q = as.data.frame(as.matrix(dtm))
colnames(df_q) = paste("q_",colnames(df_q),sep="")

df_q = cbind(feats$id, df_q, F1,F3,F4)

write.csv(df_q, "/home/mikeskim/Desktop/kaggle/telstra/features/feats4x.csv", quote=FALSE, row.names = FALSE)


