require(MASS)
require(data.table)

train = fread('/home/mikeskim/Desktop/kaggle/telstra/data/train.csv',data.table=F)
test = fread('/home/mikeskim/Desktop/kaggle/telstra/data/test.csv',data.table=F)

test$fault_severity=-1

both = rbind(train,test)



feat4 = fread('/home/mikeskim/Desktop/kaggle/telstra/data/log_feature.csv')


feat4 = feat4[ , `:=`( COUNT = .N ) , by = log_feature ]


feat4 <- feat4[ ,.(
  vv1 = sum(COUNT)
), id]




write.csv(feat4, "/home/mikeskim/Desktop/kaggle/telstra/features/feats61.csv", quote=FALSE, row.names = FALSE)


