require(tm)
require(data.table)

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


feat1 = fread('/home/mikeskim/Desktop/kaggle/telstra/data/event_type.csv')
#feat2 = fread('/home/mikeskim/Desktop/kaggle/telstra/data/severity_type.csv')#this join is 1-1
feat3 = fread('/home/mikeskim/Desktop/kaggle/telstra/data/resource_type.csv')
feat4 = fread('/home/mikeskim/Desktop/kaggle/telstra/data/log_feature.csv')



tmp1 <- feat1[ ,.(
  V1 = max(unlist(lapply(strsplit(event_type,' '),function(x) x[2])))
), id]

tmp3 <- feat3[ ,.(
  V3 = max(unlist(lapply(strsplit(resource_type,' '),function(x) x[2])))
), id]

tmp4 <- feat4[ ,.(
  V4 = max(unlist(lapply(strsplit(log_feature,' '),function(x) x[2])))
), id]

tmp5 <- feat4[ ,.(
  V5 = max(volume)
), id]

tmp1 = data.frame(tmp1)
tmp1 = cbind(tmp1,tmp3$V3,tmp4$V4,tmp5$V5)


write.csv(tmp1, "/home/mikeskim/Desktop/kaggle/telstra/features/feats24.csv", quote=FALSE, row.names = FALSE)


