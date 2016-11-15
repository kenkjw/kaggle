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

feats1 <- feat1[ ,.(
  V1 = paste0(sort(sub('event_type ','',event_type)),collapse=' ')
), id]

feats2 <- feat2[ ,.(
  V1 = paste0(sort(sub('severity_type ','',severity_type)),collapse=' ')
), id]

feats3 <- feat3[ ,.(
  V1 = paste0(sort(sub('resource_type ','',resource_type)),collapse=' ')
), id]

feats4 <- feat4[ ,.(
  V1 = paste0( sort(rep(sub('feature ','',log_feature),volume)), collapse=' ')
), id]

feats1 = feats1[ , `:=`( COUNT = .N ) , by = V1 ]
feats2 = feats2[ , `:=`( COUNT = .N ) , by = V1 ]
feats3 = feats3[ , `:=`( COUNT = .N ) , by = V1 ]
feats4 = feats4[ , `:=`( COUNT = .N ) , by = V1 ]

#DT[ , `:=`( COUNT = .N , IDX = 1:.N ) , by = VAL ]

feats= cbind(feats1,feats2,feats3,feats4)
feats = data.frame(feats)
feats = feats[,c(1,3,6,9,12)]

write.csv(feats, "/home/mikeskim/Desktop/kaggle/telstra/features/feats58.csv", quote=FALSE, row.names = FALSE)


