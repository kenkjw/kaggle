require(tm)
require(data.table)
require(MASS)

Mean = function(x) {
  quantile(as.numeric(x),0.25)
}

Mean1 = function(x) {
  quantile(as.numeric(x),0.75)
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


#feat1 = fread('/home/mikeskim/Desktop/kaggle/telstra/data/event_type.csv')
#feat2 = fread('/home/mikeskim/Desktop/kaggle/telstra/data/severity_type.csv')#this join is 1-1
#feat3 = fread('/home/mikeskim/Desktop/kaggle/telstra/data/resource_type.csv')
feat4 = fread('/home/mikeskim/Desktop/kaggle/telstra/data/log_feature.csv')

tmp4 <- feat4[ ,.(
  E1 = as.numeric(unlist(lapply(strsplit(log_feature,' '),function(x) x[2]))),
  F1 = as.numeric(volume)
), id]

tmp5 <- tmp4[ ,.(
 E2 = rep(E1,F1)
), id]

tmp6 <- tmp5[ ,.(
  E3 = fitdistr(E2,'exponential')$estimate[1],
  E4 = fitdistr(E2,'poisson')$estimate[1]
), id]

tmp6[is.na(tmp6)]=-999
head(tmp6)


write.csv(tmp6, "/home/mikeskim/Desktop/kaggle/telstra/features/feats37.csv", quote=FALSE, row.names = FALSE)


