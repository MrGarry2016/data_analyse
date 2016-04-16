rm(list=ls())
library(e1071)
library(pROC)
library(lattice)
setwd("/Users/huangge/GoogleCloudDrive/6740/hw3/workspace")
data = read.csv("cleandata.csv")[,-1]
myy = data[,1,drop = F]
myx = data.matrix(data[,-1])
n =dim(myx)[1]
p =dim(myx)[2]


##start CV
Niter = 100
kfold = 10
##1.specify your parameter here
##cost
para = sapply(seq(-9,0,0.2),function(xx){
  return(10**(xx))
})
##gama
para2 = sapply(seq(-9,0,0.2),function(xx){
  return(10**(xx))
})
auc.res = array(NA,c(Niter,length(para),length(para2)))

for(j in 1:Niter){
  testID = sample(1:n,round(n/kfold))
  for(i in 1:length(para)){
    for(k in 1:length(para2)){  
      print(j)
      ##modelling
      model = svm(myx[-testID,],myy[-testID,],cost = para[i],gamma = para2[k], probability = T)
      yhat = predict(model,myx[testID,],probability = T)
      yhat=attr(yhat,"probabilities")
      roc = roc(myy[testID,], yhat[,1])
      auc.res[j,i,k] = auc(roc)
      
    }
  }
}

dput(para,"svm.cost.para.r")
dput(para2,"svm.gamma.para.r")
dput(auc.res,"auc.res.svm.r")

##read data
svm.cost.para.r = dget("svm.cost.para.r")
svm.gamma.para.r = dget("svm.gamma.para.r")
auc.res = dget("auc.res.svm.r")
mytest = dget("mytest.r")


auc.list = apply(auc.res,c(2,3),mean)
levelplot(auc.list)
bestpara_pos = which(auc.list == max(auc.list), arr.ind = TRUE)
bestpara1 = svm.cost.para.r[bestpara_pos[1]]
bestpara2 = svm.gamma.para.r[bestpara_pos[2]]


model = svm(myx,myy[,],cost = bestpara1,gamma = bestpara2, probability = T)
yhat = predict(model,mytest[,-1],probability = T)
yhat=attr(yhat,"probabilities")
roc = roc(mytest[,1], yhat[,1])
auc.res[j,i,k] = auc(roc)
auc(roc)

