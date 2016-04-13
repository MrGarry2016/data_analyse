rm(list=ls())
library(nnet)
library(pROC)
library(lattice)
data = read.csv("cleandata.csv")[,-1]
myy = data[,1,drop=F]
myx = data.matrix(data[,-1])
n =dim(myx)[1]
p =dim(myx)[2]


##learn model
model = nnet(ANGLE.CLOSURE~.,data = data, size = 6,decay = 0.5 )
pred = predict(model,data,type = "raw")
roc = roc(data[,1],pred)


##start CV
Niter = 100
kfold = 10
##1.specify your parameter here
##size
para = sapply(seq(6,50,2),function(xx){
  return((xx))
})
##decay
para2 = sapply(seq(0.5,10,0.5),function(xx){
  return((xx))
})
auc.res = array(NA,c(Niter,length(para),length(para2)))
for(j in 1:Niter){
  testID = sample(1:n,round(n/kfold))
  for(i in 1:length(para)){
    for(k in 1:length(para2)){  
      print(j)
      ##modelling
      model = nnet(ANGLE.CLOSURE~.,data = data[-testID,], size = 6,decay = 0.5 )
      yhat = predict(model,data[testID,],type = "raw")
      roc = roc(myy[testID,], yhat)
      auc.res[j,i,k] = auc(roc)
    }
  }
}
auc.list = apply(auc.res,c(2,3),mean)
levelplot(auc.list)
##bestpara = para[which.max(auc.list)]
dput(para,"nn.size.para.r")
dput(para2,"nn.decay.para.r")
dput(auc.res,"auc.res.nn.r")

##read data
nn.size.para.r = dget("nn.size.para.r")
nn.decay.para.r = dget("nn.decay.para.r")
auc.res = dget("auc.res.nn.r")

