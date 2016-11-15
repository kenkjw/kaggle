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
  V1 = paste0(sub('event_type ','',event_type),collapse=' ')
), id]

feats2 <- feat2[ ,.(
  f2 = paste0(sub('severity_type ','',severity_type),collapse=' ')
), id]

feats3 <- feat3[ ,.(
  f3 = paste0(sub('resource_type ','',resource_type),collapse=' ')
), id]

feats4 <- feat4[ ,.(
  f4 = paste0( rep(sub('feature ','',log_feature),volume), collapse=' ')
), id]

feats = cbind(feats1,feats2$f2,feats3$f3,feats4$f4)
feats$alltext = paste(feats$V1,feats$V2,feats$V3,sep=' ')


tmp6 <- feats[ ,.(
  a1 = fitdistr(as.numeric(unlist(strsplit(alltext,' '))),'geometric')$estimate[1],
  a2 = fitdistr(as.numeric(unlist(strsplit(alltext,' '))),'exponential')$estimate[1],
  a3 = fitdistr(as.numeric(unlist(strsplit(alltext,' '))),'poisson')$estimate[1]
), id]

tmp6[is.na(tmp6)]=-1


write.csv(tmp6, "/home/mikeskim/Desktop/kaggle/telstra/features/feats46.csv", quote=FALSE, row.names = FALSE)


