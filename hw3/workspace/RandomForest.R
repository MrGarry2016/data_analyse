library(randomForest)
library(pROC)
setwd("/Users/huangge/GoogleCloudDrive/6740/hw3/workspace")
data = read.csv("cleandata.csv")[,-1]
myy = data[,1,drop = F]
myx = data.matrix(data[,-1])
n = dim(myx)[1]
p = dim(myx)[2]
set.seed(71)


#### start CV
Niter = 100
kfold = 10
##1. change para
##para for randomforest:ntree
para = seq(30,800,30)
auc.res = matrix(NA,Niter,length(para))

for(j in 1:Niter){
  testID = sample(1:n,round(n/kfold))
  for(i in 1:length(para)){
    print(j)
    print(para[i])
    ##2. change modelling
    model = randomForest(myx[-testID,],myy[-testID],ntree  = para[i], probability = T)
    yhat = predict(model,myx[testID,],type = "prob")
    roc = roc(myy[testID], yhat[,1])
    auc.res[j,i] = auc(roc)
  }
}

auc.list = apply(auc.res,2,mean)
bestpara = para[which.max(auc.list)]
plot(para,auc.list)

rf_best_model = randomForest(myx,myy,ntree  = bestpara, probability = T)

dput(para,"2-rf.para.r")
dput(auc.res,"2-auc.res.rf.r")
dput(rf_best_model,"2-rf_best_model.r")

## read para ###
para = dget("2-rf.para.r")
auc.res = dget("2-auc.res.rf.r")


