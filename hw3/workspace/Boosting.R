library(ada)
library(pROC)
data = read.csv("cleandata.csv")[,-1]
myy = data[,1,drop = F]
myx = data.matrix(data[,-1])
n = dim(myx)[1]
p = dim(myx)[2]

##learn model
model = ada(myx,myy )
pred = predict(model,as.data.frame(myx))

##check prediiction
table(pred,myy)


#### start CV
Niter = 100
kfold = 10
##1. change para
##para for randomforest:nu
para = 10**seq(-4,0.5,0.2)
auc.res = matrix(NA,Niter,length(para))

for(j in 1:Niter){
  testID = sample(1:n,round(n/kfold))
  for(i in 1:length(para)){
    print(j)
    print(para[i])
    ##2. change modelling
    model = ada(myx[-testID,],myy[-testID,] ,nu=para[i])
    yhat = predict(model,as.data.frame(myx[testID,]),type = "prob")
    roc = roc(myy[testID,], yhat[,1])
    auc.res[j,i] = auc(roc)
    print(auc.res[j,i])
  }
}

#auc.list = apply(auc.res,2,mean)
#bestpara = para[which.max(auc.list)]
#plot(para,auc.list)

dput(para,"2-ada.para.r")
dput(auc.res,"2-au.res.ada.r")


## read data ##
#para = dget("2-ada.para.r")
#auc.res = dget("2-au.res.ada.r")
#ada_best_model = dget("2-ada_best_model.r")

