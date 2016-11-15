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


feat1 = fread('/home/mikeskim/Desktop/kaggle/telstra/data/event_type.csv')
#feat2 = fread('/home/mikeskim/Desktop/kaggle/telstra/data/severity_type.csv')#this join is 1-1
feat3 = fread('/home/mikeskim/Desktop/kaggle/telstra/data/resource_type.csv')
feat4 = fread('/home/mikeskim/Desktop/kaggle/telstra/data/log_feature.csv')



tmp1 <- feat1[ ,.(
  V1 = Mean(unlist(lapply(strsplit(event_type,' '),function(x) x[2])))
), id]

tmp3 <- feat3[ ,.(
  V3 = Mean(unlist(lapply(strsplit(resource_type,' '),function(x) x[2])))
), id]

tmp4 <- feat4[ ,.(
  V4 = Mean(unlist(lapply(strsplit(log_feature,' '),function(x) x[2])))
), id]

tmp5 <- feat4[ ,.(
  V5 = Mean(volume)
), id]

tmp1 = data.frame(tmp1)
tmp1 = cbind(tmp1,tmp3$V3,tmp4$V4,tmp5$V5)
colnames(tmp1) = c('id','z1','z2','z3','z4')

write.csv(tmp1, "/home/mikeskim/Desktop/kaggle/telstra/features/feats26.csv", quote=FALSE, row.names = FALSE)


