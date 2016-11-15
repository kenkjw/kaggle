


require(MASS)
require(data.table)
require(moments)

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
feat4$prod1 = paste(feat4$event_type,feat4$location)


feat4 = feat4[ , `:=`( COUNT = .N ) , by = prod1]

feat4 <- feat4[ ,.(
  logn1a = fitdistr(COUNT,"log-normal")$estimate[1], #tried max,median, mad (worse), iqr, skewness, entropy
  logn2a = fitdistr(COUNT,"log-normal")$estimate[2]
), id]

feat4[is.na(feat4)]=-1
#feat4 = feat4[,c(1,6),with=F]


write.csv(feat4, "/home/mikeskim/Desktop/kaggle/telstra/features/feats80.csv", quote=FALSE, row.names = FALSE)