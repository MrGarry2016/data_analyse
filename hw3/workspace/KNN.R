rm(list=ls())
library(kknn)
library(pROC)
library(lattice)
data = read.csv("cleandata.csv")[,-1]
myy = data[,1,drop=F]
myx = data.matrix(data[,-1])
n =dim(myx)[1]
p =dim(myx)[2]

##learn model
model = kknn(ANGLE.CLOSURE~.,train = data,test = data, k = 10,distance = 3 )
pred = predict(model,data,type = "raw")
roc = roc(data[,1],as.numeric(pred))


##start CV
Niter = 100
kfold = 10
##1.specify your parameter here
##k
para = sapply(seq(1,100,2),function(xx){
  return((xx))
})
##distance
para2 = sapply(seq(1,10,0.5),function(xx){
  return((xx))
})
auc.res = array(NA,c(Niter,length(para),length(para2)))
for(j in 1:Niter){
  testID = sample(1:n,round(n/kfold))
  for(i in 1:length(para)){
    for(k in 1:length(para2)){  
      print(j)
      print(para[i])
      print(para2[k])
      ##modelling
      model = kknn(ANGLE.CLOSURE~.,train = data[-testID,],test = data[testID,], k = para[i],distance = para2[k] )
      yhat = model$prob
      roc = roc(myy[testID,], yhat[,1])
      auc.res[j,i,k] = auc(roc)
      print(auc.res[j,i,k])
    }
  }
}

##bestpara = para[which.max(auc.list)]
dput(para,"knn.k.para.r")
dput(para2,"knn.distance.para.r")
dput(auc.res,"auc.res.knn.r")

##read data
nn.k.para.r = dget("knn.k.para.r")
nn.distance.para.r = dget("knn.distance.para.r")
auc.res = dget("auc.res.knn.r")
mytest = dget("mytest.r")

data = read.csv("cleandata.csv")[,-1]
myy = data[,1,drop=F]
myx = data.matrix(data[,-1])


auc.list = apply(auc.res,c(2,3),mean)
levelplot(auc.list)


bestpara_pos = which(auc.list == max(auc.list), arr.ind = TRUE)
bestpara1 = nn.k.para.r[bestpara_pos[1]]##99
bestpara2 = nn.distance.para.r[bestpara_pos[2]]##1


model = kknn(ANGLE.CLOSURE~.,train = data[,],test = mytest[,],k=bestpara1,distance = bestpara2 )
yhat = model$prob
roc = roc(mytest[,1], yhat[,1])
auc(roc)##0.959
plot(roc)
