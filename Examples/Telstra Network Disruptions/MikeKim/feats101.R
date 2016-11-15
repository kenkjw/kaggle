


require(MASS)
require(data.table)
require(tm)

Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}


train = fread('/home/mikeskim/Desktop/kaggle/telstra/data/train.csv',data.table=F)
test = fread('/home/mikeskim/Desktop/kaggle/telstra/data/test.csv',data.table=F)

test$fault_severity=-1

both = rbind(train,test)
nrow(both)
tmp = fread('/home/mikeskim/Desktop/kaggle/telstra/data/severity_type.csv')
tmp$time1 = 1:nrow(tmp)

both = data.table(merge(both,tmp,by='id',all.x=T))
both = data.frame(both)
both = both[,c(1,5)]
nrow(both)
write.csv(both, "/home/mikeskim/Desktop/kaggle/telstra/features/feats101.csv", quote=FALSE, row.names = FALSE)


