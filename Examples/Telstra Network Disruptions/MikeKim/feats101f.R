


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

tmp = fread('/home/mikeskim/Desktop/kaggle/telstra/data/resource_type.csv')
tmp$time1 = 1:nrow(tmp)
combiner = tmp[ ,.(
 timeres = mean(time1)
), id]

tmp = fread('/home/mikeskim/Desktop/kaggle/telstra/data/event_type.csv')
tmp$time1 = 1:nrow(tmp)
tmp = tmp[ ,.(
  timeev = mean(time1)
), id]
tmp = data.frame(tmp)
combiner = cbind(combiner,tmp$timeev)

tmp = fread('/home/mikeskim/Desktop/kaggle/telstra/data/severity_type.csv')
tmp$time1 = 1:nrow(tmp)
tmp = tmp[ ,.(
  timesev = mean(time1)
), id]
tmp = data.frame(tmp)
combiner = cbind(combiner,tmp$timesev)

tmp = fread('/home/mikeskim/Desktop/kaggle/telstra/data/log_feature.csv')
tmp$time1 = 1:nrow(tmp)
tmp = tmp[ ,.(
  timelog = mean(time1)
), id]
tmp = data.frame(tmp)
combiner = cbind(combiner,tmp$timelog)


both = data.table(merge(both,combiner,by='id',all.x=T))
both = data.frame(both)


tmp = apply(both,1,function(x) mean(as.numeric(x[4:7])))

tmp = data.frame(tmp)
tmp$id = both$id

write.csv(tmp, "/home/mikeskim/Desktop/kaggle/telstra/features/feats101f.csv", quote=FALSE, row.names = FALSE)


