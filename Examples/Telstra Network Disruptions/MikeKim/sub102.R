require(xgboost)
#require(randomForest)
require(data.table)
require(caret)
#set.seed(48)
set.seed(6)

tableX = function(x) {
  tmpT = table(x)
  max(tmpT/sum(tmpT))
}

decor = function(data,cutoff) {
  tmp = apply(data,2,function(x) length(unique(x)))
  data = data[,tmp>1]
  tmp <- cor(data)
  tmp[!lower.tri(tmp)] <- 0
  data[,!apply(tmp,2,function(x) any(x > cutoff))]
}

decor1 = function(data,cutoff1) {
  tmp = apply(data,2,function(x) length(unique(x)))
  data = data[,tmp>1]
  
  df2 = cor(data)
  hc = findCorrelation(df2, cutoff=cutoff1,exact = T) # putt any value as a "cutoff" 
  hc = sort(hc)
  data[,-c(hc)]
}

which2 = function(x) {
  tmp = which(x>0)
  if (length(tmp)==0) {
    return(-1)
  }
  else {
    return(mean(tmp))
  }
}

decor2 = function(data,cutoff1) {
  #  data1 = data
  #  data1$fault_severity = NULL#both has -1,0,1,2 target vals
  tmp = apply(data,2,function(x) length(unique(x)))
  data = data[,tmp>1]  
  
  data1 = data[,tmp==2]
  tmp1 = apply(data1,2,tableX)
  tmp1 = which(tmp1 >= (1-(1/18522)))
  data1 = data1[,tmp1]
  tmpfeat1 = apply(data1,1,function(x) sum(x>0))
  #  tmpfeat1[tmpfeat1>0]=1
  tmpfeat2 = apply(data1,1,which.min)
  tmpfeat3 = apply(data1,1,which.max)
  #  tmpfeat3 = apply(data1,1,which2)
  #tmpfeat4 = apply(data1,1,mean)
  
  data = data[setdiff(names(data),names(tmp1))]
  #  data = cbind(data,tmpfeat1,tmpfeat2,tmpfeat3)
  
  df2 = cor(data)
  hc = findCorrelation(df2, cutoff=cutoff1,exact = T) # putt any value as a "cutoff" 
  hc = sort(hc)
  data[,-c(hc)]
}

train = fread('/home/mikeskim/Desktop/kaggle/telstra/data/train.csv',data.table=F)
test = fread('/home/mikeskim/Desktop/kaggle/telstra/data/test.csv',data.table=F)

test$fault_severity=-1

both = rbind(train,test)

#this definitely lowers cv. tried many times to cv.
#tmp = fread('/home/mikeskim/Desktop/kaggle/telstra/data/severity_type.csv',data.table=F)
#both = merge(both,tmp,by="id",all.x=T)
#both$severity_type = as.numeric(sapply(both$severity_type, function(x) as.numeric(unlist(strsplit(x, ' '))[2])))

both$locnum = 0
tmpU = unique(both$location)
for (loc in tmpU) {
  tmpC0 = both$location==loc
  tmpC1 = sum(tmpC0)
  both$locnum[tmpC0]=tmpC1
}


feats = fread('/home/mikeskim/Desktop/kaggle/telstra/features/feats28.csv',data.table=F)
both = merge(both,feats,by.x="location",by.y="tmpL",all.x=T)

#feats = fread('/home/mikeskim/Desktop/kaggle/telstra/features/feats29.csv',data.table=F)
#both = merge(both,feats,by.x="location",by.y="location",all.x=T)

#both$placenum = 1:nrow(both) #no order leak here.
#both$locnum = 0
#both$locnum1 = 0
#both$locnum2 = 0
#tmpU = unique(both$location)
#for (loc in tmpU) {
#  tmpC0 = both$location==loc
#  tmpC1 = sum(tmpC0)
#  both$locnum[tmpC0]=tmpC1
#  both$locnum1[tmpC0]=mean(both[tmpC0,]$id)
#  both$locnum2[tmpC0]=sd(both[tmpC0,]$id)
#}
#both$locnum2[is.na(both$locnum2)]=0

#both$location=NULL
both$location = as.numeric(unlist(lapply(strsplit(both$location,' '),function(x) x[2])))#as.numeric(as.factor(both$location))

#both$loc2 = both$location^2*both$locnum^0.6 #worse
#both$location=NULL; both$locnum=NULL

feats = fread('/home/mikeskim/Desktop/kaggle/telstra/features/feats4x.csv',data.table=F)
#featsX = fread('/home/mikeskim/Desktop/kaggle/telstra/features/feats15.csv',data.table=F)#doesn't help
dim(feats)

tmpF = apply(feats,1,function(x) sum(x[2:(ncol(feats)-3)]))
tmpF1 = apply(feats,1,function(x) which.max(x[2:(ncol(feats)-3)]))
tmpF2 = apply(feats,1,function(x) sum(x==0))
tmpF3 = apply(feats,1,function(x) max(x[2:(ncol(feats)-3)]))
tmpF4 = apply(feats,1,function(x) sum(which(x==0)))
tmpF5 = apply(feats,1,function(x) median(which(x==0)))
tmpF6 = apply(feats,1,function(x) min(which(x==0)))# no difference out
tmpF7 = apply(feats,1,function(x) max(which(x==0)))
tmpF8 = apply(feats,1,function(x) sum(which(x==0)^2)) #try 0.5 0.8 etc.
tmpF9 = apply(feats,1,function(x) quantile(which(x==0),0.25))
tmpF10 = apply(feats,1,function(x) quantile(which(x==0),0.75))
tmpF4a = apply(feats,1,function(x) sum(which(x>0)))
#tmpF5a = apply(feats,1,function(x) median(which(x>0)))
tmpF6a = apply(feats,1,function(x) min(which(x>0)))
tmpF7a = apply(feats,1,function(x) max(which(x>0)))
#tmpF8a = apply(feats,1,function(x) sum(which(x>0)^2)) #try 0.5 0.8 etc.
#tmpF9a = apply(feats,1,function(x) quantile(which(x>0),0.25))
#tmpF10a = apply(feats,1,function(x) quantile(which(x>0),0.75))
#tmpF4b = apply(feats,1,function(x) sum(which(x>1)))
#tmpF6b = apply(feats,1,function(x) min(which(x[2:(ncol(feats)-3)]>1)))
#tmpF7b = apply(feats,1,function(x) max(which(x[2:(ncol(feats)-3)]>1)))
#tmpF9 = apply(feats,1,function(x) sum(which(x==0)^0.5))
#tmpF8 = apply(feats,1,function(x) mad(which(x==0)))
#tmpF5 = apply(feats,1,function(x) sd(which(x==0)))
#tmpF1 = apply(feats,1,function(x) sum(x[(ncol(feats)-2):(ncol(feats))]))
#tmpF1 = apply(feats11,1,function(x) sum(x[2:(ncol(feats11))]))
#tmpA1 = apply(feats,1,function(x) sd(which(x>1)))
feats = cbind(feats,tmpF,tmpF1,tmpF2,tmpF3,tmpF4, tmpF5, tmpF6, tmpF7, tmpF8, tmpF9, tmpF10,#tmpF6
              tmpF6a,tmpF7a, tmpF4a)#, tmpF6b,tmpF7b)#,featsX[,2])#,tmpF1)
#just second col. cor(train[,ncol(train)],train$fault_severity) 0.117... others are 0.01

both = merge(both,feats,by.x='id',by.y='feats$id',all.x=T)

feats1 = fread('/home/mikeskim/Desktop/kaggle/telstra/features/feats22.csv',data.table=F)
both = merge(both,feats1,by.x='id',by.y='id',all.x=T)

feats1 = fread('/home/mikeskim/Desktop/kaggle/telstra/features/feats23.csv',data.table=F)
both = merge(both,feats1,by.x='id',by.y='id',all.x=T)

feats1 = fread('/home/mikeskim/Desktop/kaggle/telstra/features/feats24.csv',data.table=F)
both = merge(both,feats1,by.x='id',by.y='id',all.x=T)

feats1 = fread('/home/mikeskim/Desktop/kaggle/telstra/features/feats25.csv',data.table=F)
both = merge(both,feats1,by.x='id',by.y='id',all.x=T)

feats1 = fread('/home/mikeskim/Desktop/kaggle/telstra/features/feats26.csv',data.table=F)
both = merge(both,feats1,by.x='id',by.y='id',all.x=T)

feats1 = fread('/home/mikeskim/Desktop/kaggle/telstra/features/feats27.csv',data.table=F)
both = merge(both,feats1,by.x='id',by.y='id',all.x=T)

feats1 = fread('/home/mikeskim/Desktop/kaggle/telstra/features/feats36.csv',data.table=F)
#feats1 = feats1[,c(1,3)] #all,c(1,4),c(1,2)
both = merge(both,feats1,by.x='id',by.y='id',all.x=T)

feats1 = fread('/home/mikeskim/Desktop/kaggle/telstra/features/feats37.csv',data.table=F)
#feats1 = feats1[,c(1,3)] #all,c(1,4),c(1,2)
both = merge(both,feats1,by.x='id',by.y='id',all.x=T)

feats1 = fread('/home/mikeskim/Desktop/kaggle/telstra/features/feats46.csv',data.table=F)
#feats1 = feats1[,c(1,2:3)]  #[440]	train-mlogloss:0.278286+0.001140	test-mlogloss:0.479247+0.019084
#feats1 = feats1[,c(1,2)]  [430]	train-mlogloss:0.280816+0.001092	test-mlogloss:0.479149+0.019104
feats1 = feats1[,c(1,2)]
both = merge(both,feats1,by.x='id',by.y='id',all.x=T)

feats1 = fread('/home/mikeskim/Desktop/kaggle/telstra/features/feats51.csv',data.table=F)
both = merge(both,feats1,by.x='id',by.y='id',all.x=T)

feats1 = fread('/home/mikeskim/Desktop/kaggle/telstra/features/feats58.csv',data.table=F)#keepin
both = merge(both,feats1,by.x='id',by.y='id',all.x=T)

feats1 = fread('/home/mikeskim/Desktop/kaggle/telstra/features/feats60.csv',data.table=F)#feats60a.csv same score
both = merge(both,feats1,by.x='id',by.y='id',all.x=T)

feats1 = fread('/home/mikeskim/Desktop/kaggle/telstra/features/feats61.csv',data.table=F)
both = merge(both,feats1,by.x='id',by.y='id',all.x=T)

feats1 = fread('/home/mikeskim/Desktop/kaggle/telstra/features/feats74.csv',data.table=F)
both = merge(both,feats1,by.x='id',by.y='id',all.x=T)

feats1 = fread('/home/mikeskim/Desktop/kaggle/telstra/features/feats78.csv',data.table=F)
both = merge(both,feats1,by.x='id',by.y='id',all.x=T)

feats1 = fread('/home/mikeskim/Desktop/kaggle/telstra/features/feats79.csv',data.table=F)
both = merge(both,feats1,by.x='id',by.y='id',all.x=T)

feats1 = fread('/home/mikeskim/Desktop/kaggle/telstra/features/feats80.csv',data.table=F)
both = merge(both,feats1,by.x='id',by.y='id',all.x=T)

feats1 = fread('/home/mikeskim/Desktop/kaggle/telstra/features/feats79tm.csv',data.table=F)
both = merge(both,feats1,by.x='id',by.y='id',all.x=T)

feats1 = fread('/home/mikeskim/Desktop/kaggle/telstra/features/feats80tm.csv',data.table=F)
both = merge(both,feats1,by.x='id',by.y='id',all.x=T)#better in

feats1 = fread('/home/mikeskim/Desktop/kaggle/telstra/features/feats95.csv',data.table=F)
both = merge(both,feats1,by.x='id',by.y='id',all.x=T)#better in

feats1 = fread('/home/mikeskim/Desktop/kaggle/telstra/features/feats101.csv',data.table=F)
both = merge(both,feats1,by.x='id',by.y='id',all.x=T)

feats1 = fread('/home/mikeskim/Desktop/kaggle/telstra/features/feats101f.csv',data.table=F)
both = merge(both,feats1,by.x='id',by.y='id',all.x=T)#better in

#feats1 = fread('/home/mikeskim/Desktop/kaggle/telstra/features/sub59b_feats.csv',data.table=F)
#both = merge(both,feats1,by.x='id',by.y='id',all.x=T) #take out better

#feats1 = fread('/home/mikeskim/Desktop/kaggle/telstra/features/sub59b_feats2.csv',data.table=F)
#both = merge(both,feats1,by.x='id',by.y='id',all.x=T) #take out better.

feats1 = fread('/home/mikeskim/Desktop/kaggle/telstra/data/severity_type.csv',data.table=F)
feats1$severity_type = unlist(lapply(strsplit(feats1$severity_type,' '),function(x) as.numeric(x[2])))
both = merge(both,feats1,by.x='id',by.y='id',all.x=T)


#feats1 = fread('/home/mikeskim/Desktop/kaggle/telstra/features/feats117.csv',data.table=F)
#both = merge(both,feats1,by.x='id',by.y='id',all.x=T)


#feats1 = fread('/home/mikeskim/Desktop/kaggle/telstra/features/feats111.csv',data.table=F)
#both = merge(both,feats1,by.x='id',by.y='id',all.x=T)#better in

#/home/mikeskim/Desktop/kaggle/telstra/features/metaTsne2.csv

#metatrain4_1 /metatrain4_2 metatrain2 metatrain19 metatrain18 metatrain17

#feats1 = fread('/home/mikeskim/Desktop/kaggle/telstra/features/sub59b_feats3/4/5/6/9.csv',data.table=F)
#both = merge(both,feats1,by.x='id',by.y='id',all.x=T)

#feats1 = fread('/home/mikeskim/Desktop/kaggle/telstra/features/sub59b_feats10.csv',data.table=F)
#both = merge(both,feats1,by.x='id',by.y='id',all.x=T)

#feats1 = fread('/home/mikeskim/Desktop/kaggle/telstra/features/sub59Feat_train_test.csv',data.table=F)
#both = merge(both,feats1,by.x='id',by.y='id',all.x=T)

#feats1 = fread('/home/mikeskim/Desktop/kaggle/telstra/features/feats101c.csv',data.table=F)
#both = merge(both,feats1,by.x='id',by.y='id',all.x=T)


both$loc203 = 0
#both$loc204 = log(both$time1)
both$loc205 = 0
tmpLoc = unique(both$location)
for (loc in tmpLoc) {
  #  both$loc203[both$location==loc] = sum(both$q_feature_203[both$location==loc])
  both$loc203[both$location==loc] = rank(both$time1[both$location==loc]) 
  both$loc205[both$location==loc] = rank(both$time1[both$location==loc]) / max(rank(both$time1[both$location==loc]))
  #  both$loc203[both$location==loc] = both$time1[both$location==loc]
  #  both$loc204[both$location==loc] = log(both$time1[both$location==loc])
}


#both$crossx = both$loc203*both$loc204
#both$logfeat203 = log(1+both$q_feature_203)#best single feature 0.4 to 0.49 cor

#both$loc_203 = 0
#both$loc_204 = 0
#tmpLoc = unique(both$locnum)
#for (loc in tmpLoc) {
#  both$loc_203[both$locnum==loc] = rank(both$time1[both$locnum==loc])
#  both$loc_204[both$locnum==loc] = sqrt(both$time1[both$locnum==loc])
#}


#F1,tmpF1,tmpF2,tmpF5,tmpF7
#both$locros1 = both$loc205*both$tmpF2#try +1
both$locros1 = log(both$loc203+1)*both$tmpF2#try +1
both$locros2 = log(both$loc203+1)*both$q_1#
##q_2,3,5,6 small.

#both$locros3 = log(both$loc203+1)*both$z4.x#try +1
#both$locros3 = log(both$loc203+1)*both$tmpF10 #tmpF5,7,8,9 too high cor

#tmp = apply(train,2,function(x) cor(x,trainY))
#both$sev1 = 0
#both$sev1[both$q_severity_type_1>0]=log(both$time1[both$q_severity_type_1>0])
#both$sev1[both$q_severity_type_2>0]=log(both$time1[both$q_severity_type_2>0])
#both$sev1[both$q_severity_type_3>0]=log(both$time1[both$q_severity_type_3>0])
#both$sev1[both$q_severity_type_4>0]=log(both$time1[both$q_severity_type_4>0])
#both$sev1[both$q_severity_type_5>0]=log(both$time1[both$q_severity_type_5>0])

#[4,]   8 0.404038
#[42,]  46 0.404971

jjj=8
both$locx1 = 0
tmpLoc = unique(both[,jjj])
for (loc in tmpLoc) {
  both$locx1[both[,jjj]==loc] = rank(both$time1[both[,jjj]==loc]) / max(rank(both$time1[both[,jjj]==loc]))
}

jjj=46
both$locx2 = 0
tmpLoc = unique(both[,jjj])
for (loc in tmpLoc) {
  both$locx2[both[,jjj]==loc] = rank(both$time1[both[,jjj]==loc]) / max(rank(both$time1[both[,jjj]==loc]))
}

#jjj=470
#both$locx3 = 0
#tmpLoc = unique(both[,jjj])
#for (loc in tmpLoc) {
#  both$locx3[both[,jjj]==loc] = rank(both$time1[both[,jjj]==loc]) / max(rank(both$time1[both[,jjj]==loc]))
#}

jjj=37#300
both$locx3 = 0
tmpLoc = unique(both[,jjj])
for (loc in tmpLoc) {
  both$locx3[both[,jjj]==loc] = rank(both$time1[both[,jjj]==loc]) / max(rank(both$time1[both[,jjj]==loc]))
}

#jjj=77#36#300
#both$locx4 = 0
#tmpLoc = unique(both[,jjj])
#for (loc in tmpLoc) {
#  both$locx4[both[,jjj]==loc] = rank(both$time1[both[,jjj]==loc]) / max(rank(both$time1[both[,jjj]==loc]))
#}


#tmp = both[both$fault_severity!=-1,]
#tmp$timerank = -1
#tmpLoc = unique(tmp$location)
#notes = NULL
#for (j in 5:553) {
#  for (loc in tmpLoc) {
#    tmp$timerank[tmp$location==loc] = rank(tmp[tmp$location==loc,j])
#  }
#  notes = rbind(notes,c(j,cor(tmp$timerank,tmp$fault_severity)))
#}


#feats1 = fread('/home/mikeskim/Desktop/kaggle/telstra/features/feats99.csv',data.table=F)
#both = merge(both,feats1,by.x='location',by.y='location',all.x=T)


#tmp = aggregate(both$tmpF2, by=list(both$location), FUN=sum)
#tmp$location = tmp$Group.1; tmp$Group.1 = NULL
#both = merge(both,tmp,by="location",all.x=T)

#feats1 = fread('/home/mikeskim/Desktop/kaggle/telstra/features/feats86b.csv',data.table=F) 
#both = merge(both,feats1,by.x='id',by.y='id',all.x=T)


#feats1 = fread('/home/mikeskim/Desktop/kaggle/telstra/features/feats38.csv',data.table=F)#killed by correlation 0.98
#feats1 = feats1[,c(1,3)] #all,c(1,4),c(1,2)
#both = merge(both,feats1,by.x='id',by.y='id',all.x=T)

#tmp = tapply(both$tmpF1,both$location,mean)# worse
#tmp = tapply(both$q_feature_80,both$location,mean)
#tmp = tapply(both$tmpF,both$location,mean)
#tmp = tapply(both$tmpF7a,both$location,mean)
#tmp = tapply(both$F1,both$location,mean)
#tmp = data.frame(cbind(names(tmp),as.numeric(tmp)))
#tmp[,2] = as.numeric(as.character(tmp[,2]))
#both = merge(both,tmp,by.x='location',by.y='X1',all.x=T)

#both = cbind(both,feats1)
#add tsne into here

#both$locdiff1 = both$location +both$locnum 

#feat1 = data.table(both)
#feats1 <- feat1[ ,.(
# mean1 = sum(F3)
#), location]
#both = merge(both,feats1,by.x='location',by.y='location',all.x=T)

#tmp = apply(both,2,function(x) length(unique(x)))
#index1 = as.numeric(which(tmp==2))
#tmpfeat1 = apply(both,1,function(x) sum(x[index1]))
#tmpfeat2 = apply(both,1,function(x) which.min(x[index1]))
#tmpfeat3 = apply(both,1,function(x) which.max(x[index1]))



dim(both)
#both = decor1(both,0.975)#worse
both = decor1(both,0.98)#best
dim(both)

#feats1 = fread('/home/mikeskim/Desktop/kaggle/telstra/features/feats85.csv',data.table=F)
#both = merge(both,feats1,by.x='id',by.y='id',all.x=T)

###########3


train = both[both$fault_severity!=-1,]
test = both[both$fault_severity==-1,]

testid = test$id; trainid = train$id
train$id=NULL; test$id=NULL

trainY = train$fault_severity
train$fault_severity=NULL; test$fault_severity=NULL

numberOfClasses <- max(trainY) + 1

param <- list(
  eval_metric = 'mlogloss',
  num_class = numberOfClasses,
  eta = 0.004,#0.025,#keep it 0.03
  max_depth = 6,#7, #8,9 worse
  subsample = 0.87, #0.87 worse
  colsample_bylevel = 0.76,#0.76,   0.73,75,77 worse
  colsample_bytree = 0.85, #0.8 worse
  objective = 'multi:softprob',
  num_class = numberOfClasses,
  min_child_weight = 2,  #1 #3 worse, scale_pos_weight doesn't work here. no change.
  gamma = 0.1,#0.15#0.05 worse
  lambda=0.9) #.0.9 def 0.88worse

cv.nround <- 39900
cv.nfold <- 10


bst.cv = xgb.cv(param=param, data = data.matrix(train), label = trainY, 
                nfold = cv.nfold, nrounds = cv.nround, print.every.n = 10, early.stop.round=100)
#early.stop.round = 100

#[3690]	train-mlogloss:0.189621+0.002240	test-mlogloss:0.405390+0.020602 seed6
#[3880]	train-mlogloss:0.183229+0.002564	test-mlogloss:0.404123+0.017283 seed1
#[3510]	train-mlogloss:0.195873+0.001656	test-mlogloss:0.404958+0.020331 seed5
#[3760]	train-mlogloss:0.187415+0.001949	test-mlogloss:0.405824+0.026019 seed4
#[3700]	train-mlogloss:0.189348+0.002224	test-mlogloss:0.403432+0.029247 seed3
#[3840]	train-mlogloss:0.185273+0.001790	test-mlogloss:0.403466+0.029002 seed3
#[3860]	train-mlogloss:0.184680+0.001976	test-mlogloss:0.405016+0.026098 seed2
#[3740]	train-mlogloss:0.188561+0.002819	test-mlogloss:0.403858+0.017268 Stopping. Best iteration: 3753
#[3850]	train-mlogloss:0.187798+0.002821	test-mlogloss:0.405394+0.017024 Stopping. Best iteration: 3856
#[700]	train-mlogloss:0.171515+0.002654	test-mlogloss:0.402285+0.021542 seed 48
#[620]	train-mlogloss:0.186463+0.002804	test-mlogloss:0.405703+0.017199 seed1
#[3270]	train-mlogloss:0.180070+0.002904	test-mlogloss:0.405612+0.017039 seed1 eta = 0.005 Stopping. Best iteration: 3273


NROUNDS=which.min(bst.cv$test.mlogloss.mean) #3856
print(NROUNDS)
tmpP = rep(0,(nrow(test)*3))
for (j in 1:300) {
  print(j)
  set.seed(j+1323456)
  model1 = xgboost(param=param, data = data.matrix(train), label = trainY, nrounds=NROUNDS, print.every.n = 2000)
  tmpP = tmpP + predict(model1,data.matrix(test) )
}
tmpP = tmpP/j
testpreds = matrix(tmpP, ncol=3, byrow=TRUE)

sub = fread('/home/mikeskim/Desktop/kaggle/telstra/data/sample_submission.csv',data.table=F)
sub$id = testid
sub[,2:4] = testpreds
write.csv(sub, file='/home/mikeskim/Desktop/kaggle/telstra/testpred/sub102_test.csv', quote=FALSE,row.names=FALSE)

#