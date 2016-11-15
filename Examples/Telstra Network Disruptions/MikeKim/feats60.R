require(MASS)
require(data.table)

train = fread('/home/mikeskim/Desktop/kaggle/telstra/data/train.csv',data.table=F)
test = fread('/home/mikeskim/Desktop/kaggle/telstra/data/test.csv',data.table=F)

test$fault_severity=-1

both = rbind(train,test)



feat4 = fread('/home/mikeskim/Desktop/kaggle/telstra/data/log_feature.csv')

f1 <- feat4[ ,.(
  V1 = paste0(sort(sub('feature ','',log_feature)),collapse=' ')
), id]

f2 <- feat4[ ,.(
  V1 = paste0(sort(volume),collapse=' ')
), id]

f3 <- feat4[ ,.(
  V1 = paste0( paste0(sub('feature ','',log_feature),volume), collapse=' ')
), id]



feats1 = f1[ , `:=`( COUNT = .N ) , by = V1 ]
feats2 = f2[ , `:=`( COUNT = .N ) , by = V1 ]
feats3 = f3[ , `:=`( COUNT = .N ) , by = V1 ]


#DT[ , `:=`( COUNT = .N , IDX = 1:.N ) , by = VAL ]

feats= cbind(feats1,feats2,feats3)
feats = data.frame(feats)
feats = feats[,c(1,3,6,9)]


write.csv(feats, "/home/mikeskim/Desktop/kaggle/telstra/features/feats60.csv", quote=FALSE, row.names = FALSE)


