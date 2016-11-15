require(tm)
require(data.table)

Mean = function(x) {
  mean(as.numeric(x))
}

Mode <- function(x) {
  ux <- unique(x)
  tmpux = ux[which.max(tabulate(match(x, ux)))]
  as.numeric(unlist(strsplit(tmpux,' '))[2])
}

Mode1 <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]

}

train = fread('/home/mikeskim/Desktop/kaggle/telstra/data/train.csv',data.table=F)
test = fread('/home/mikeskim/Desktop/kaggle/telstra/data/test.csv',data.table=F)

test$fault_severity=-1

both = rbind(train,test)


#feat1 = fread('/home/mikeskim/Desktop/kaggle/telstra/data/event_type.csv')
feat2 = fread('/home/mikeskim/Desktop/kaggle/telstra/data/severity_type.csv',data.table=F)#this join is 1-1
#feat3 = fread('/home/mikeskim/Desktop/kaggle/telstra/data/resource_type.csv')
#feat4 = fread('/home/mikeskim/Desktop/kaggle/telstra/data/log_feature.csv')

feat2 = merge(both,feat2,by="id",all.x=T)
feat2 = data.table(feat2)

tmp5 <- feat2[ ,.(
  stype = paste0(sort(severity_type),collapse=' ')
), location]

tmpL = tmp5$location


DTM = function(corpus){
  dtm = DocumentTermMatrix(corpus,control=list(tolower=F,removePunctuation=F,removeNumbers=F,stopwords=F, wordLengths=c(1,Inf),
                                               stemming=F))#,weighting=function(x) weightTfIdf(x,normalize=T)))
  return (dtm)
}

#I don't have to make feats2 into a data.frame? wow!

all_text = Corpus(VectorSource(tmp5$stype))

gc()

dtm = DTM(all_text)
dtm <- removeSparseTerms(dtm, 0.9999999)#795, now 1921
dim(dtm)
df_q = as.data.frame(as.matrix(dtm))
colnames(df_q) = paste("q_",colnames(df_q),sep="")

df_q = cbind(tmpL, df_q)
df_q$q_severity_type=NULL

write.csv(df_q, "/home/mikeskim/Desktop/kaggle/telstra/features/feats28.csv", quote=FALSE, row.names = FALSE)





