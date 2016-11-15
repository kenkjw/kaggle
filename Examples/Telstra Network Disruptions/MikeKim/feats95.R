


require(MASS)
require(data.table)
require(tm)



train = fread('/home/mikeskim/Desktop/kaggle/telstra/data/train.csv',data.table=F)
test = fread('/home/mikeskim/Desktop/kaggle/telstra/data/test.csv',data.table=F)

test$fault_severity=-1

both = rbind(train,test)
both$fault_severity=NULL

feat3 = fread('/home/mikeskim/Desktop/kaggle/telstra/data/log_feature.csv')

both = data.table(merge(both,feat3,by='id',all.x=T))

both2 <- both[ ,.(
 volume = mean(volume)#tried sum
), location]
both$volume=NULL
both = merge(both,both2,by="location",all.x=T)

both2 <- both[ ,.(
  meanv = mean(volume)
), id]


both2 = data.frame(both2)
nrow(both2)
write.csv(both2, "/home/mikeskim/Desktop/kaggle/telstra/features/feats95.csv", quote=FALSE, row.names = FALSE)


