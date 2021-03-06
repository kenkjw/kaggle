


require(MASS)
require(data.table)

train = fread('/home/mikeskim/Desktop/kaggle/telstra/data/train.csv',data.table=F)
test = fread('/home/mikeskim/Desktop/kaggle/telstra/data/test.csv',data.table=F)

test$fault_severity=-1

both = rbind(train,test)


feat3 = fread('/home/mikeskim/Desktop/kaggle/telstra/data/log_feature.csv')

feat4 = merge(feat3,both,by="id",allow.cartesian = T)
feat4$prod1 = paste(feat4$log_feature,feat4$location)


feat4 = feat4[ , `:=`( COUNT = .N ) , by = prod1]

feat4 <- feat4[ ,.(
  f4 = mean(COUNT)
), id]

#feat4 = feat4[,c(1,6),with=F]


write.csv(feat4, "/home/mikeskim/Desktop/kaggle/telstra/features/feats74.csv", quote=FALSE, row.names = FALSE)