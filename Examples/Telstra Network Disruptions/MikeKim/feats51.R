require(MASS)
require(data.table)

train = fread('/home/mikeskim/Desktop/kaggle/telstra/data/train.csv',data.table=F)
test = fread('/home/mikeskim/Desktop/kaggle/telstra/data/test.csv',data.table=F)

test$fault_severity=-1

both = rbind(train,test)


feat1 = fread('/home/mikeskim/Desktop/kaggle/telstra/data/event_type.csv')
feat2 = fread('/home/mikeskim/Desktop/kaggle/telstra/data/severity_type.csv')
feat3 = fread('/home/mikeskim/Desktop/kaggle/telstra/data/resource_type.csv')
feat4 = fread('/home/mikeskim/Desktop/kaggle/telstra/data/log_feature.csv')

feat4 = merge(feat4,feat2,by="id")
feat4$log_feature = unlist(lapply(strsplit(feat4$log_feature,' '),function(x) x[2]))
feat4$severity_type = unlist(lapply(strsplit(feat4$severity_type,' '),function(x) x[2]))

feat4$cross = as.numeric(feat4$severity_type)*as.numeric(feat4$volume)

tmp6 <- feat4[ ,.(
  a1 = fitdistr(cross,'geometric')$estimate[1]
), id]

tmp6[is.na(tmp6)]=-1


write.csv(tmp6, "/home/mikeskim/Desktop/kaggle/telstra/features/feats51.csv", quote=FALSE, row.names = FALSE)


